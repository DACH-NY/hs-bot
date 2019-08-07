{-# LANGUAGE TypeApplications #-}

module DA.Ledger.App.MasterCopy.CopyBot (copyBot) where

import DA.Ledger as Ledger
import DA.Ledger.App.MasterCopy.Bot
import DA.Ledger.App.MasterCopy.Contracts
import DA.Ledger.App.MasterCopy.Domain
import DA.Ledger.App.MasterCopy.Logging
import DA.Ledger.App.MasterCopy.RuleBot
import DA.Ledger.IsLedgerValue (IsLedgerValue(..))
import DA.Ledger.Types
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set

copyBot
    :: Logger
    -> PackageId
    -> TimeSettings
    -> BotContext
    -> IO ()
copyBot log pid ts bc =
    simpleRuleNanobot log ts bc copyUpd (copyRule pid bc) (copyRecov pid bc) ()

copyUpd :: StateUpdate () ()
copyUpd _ _ = ()

copyRecov :: PackageId -> BotContext -> Recovery () ()
copyRecov pid bc bs _ _ _ = updateCopies pid bc bs

copyRule :: PackageId -> BotContext -> Rule () ()
copyRule pid bc bs _ = updateCopies pid bc bs

updateCopies :: PackageId -> BotContext -> BotState () () -> [((), [Command], PendingSet)]
updateCopies pid BotContext{party} BotState{acs} = cmds
    where
        subsTid = getTid pid "Subscriber"
        masterTid = getTid pid "Master"
        copyTid = getTid pid "Copy"

        subscribers = map (\(cid, r) -> (cid, fromJust $ fromRecord @Subscriber r)) (getRecords subsTid acs)
        masters = map (\(cid, r) -> (cid, fromJust $ fromRecord @Master r)) (getRecords masterTid acs)
        copies = map (\(cid, r) -> (cid, fromJust $ fromRecord @Copy r)) (getRecords copyTid acs)

        ownedSubscribers = Prelude.filter (\(_, Subscriber{owner}) -> party == owner) subscribers
        ownedMasters = Prelude.filter (\(_, Master{owner}) -> party == owner) masters
        ownedCopies = Prelude.filter (\(_, Copy{master=Master{owner}}) -> party == owner) copies
        
        groupedcs = List.groupBy (\(_, x) (_, y) -> x == y) ownedCopies
        duplicatecs = concatMap tail groupedcs
        uniquecs = concatMap (take 1) groupedcs

        subscribingParties = map ((\Subscriber{obs} -> obs) . snd) ownedSubscribers
        archiveMissingMaster = Prelude.filter (\(_, Copy{master}) -> master `notElem` map snd ownedMasters) uniquecs
        archiveMissingSubscriber = Prelude.filter (\(_, Copy{obs}) -> obs `notElem` subscribingParties) uniquecs
        archiveCids = Set.toList $ Set.fromList (map fst (archiveMissingMaster ++ archiveMissingSubscriber ++ duplicatecs))
        archiveCmds = map (\cid -> ((), [makeArchiveCommand cid copyTid], Map.singleton copyTid (Set.singleton cid))) archiveCids

        pendingCs = concatMap (copiesFromCommands pid . snd . snd) (Map.toList $ commandsInFlight acs)
        eventualcs = pendingCs ++ map snd uniquecs
        neededCopies = [Copy m o | m <- map snd ownedMasters, o <- subscribingParties]
        createCps = Set.toList (Set.difference (Set.fromList neededCopies) (Set.fromList eventualcs))
        createCmds = map (\cp -> ((), [makeCreateCommand pid (CCopy cp)], Map.empty)) createCps

        cmds = createCmds ++ archiveCmds

copiesFromCommands :: PackageId -> Commands -> [Copy]
copiesFromCommands pid Commands{coms} = mapMaybe mapFn coms
    where
        copyTid = getTid pid "Copy"
        mapFn = \case
            CreateCommand{tid=copyTid, args} -> fromRecord args
            _ -> Nothing

getRecords :: TemplateId -> ACS () -> [(ContractId, Record)]
getRecords tid acs = maybe [] (Map.toList . contracts) mtacs
    where
        mtacs = Map.lookup tid (templateACSs acs)
