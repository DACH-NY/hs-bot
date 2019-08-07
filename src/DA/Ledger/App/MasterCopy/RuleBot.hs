module DA.Ledger.App.MasterCopy.RuleBot 
    ( StateUpdate
    , Rule
    , Recovery
    , TimeMode(..)
    , TimeSettings(..)
    , BotState(..)
    , ACS(..)
    , TemplateACS(..)
    , PendingSet
    , simpleRuleNanobot
    ) where

import DA.Ledger as Ledger
import DA.Ledger.App.MasterCopy.Bot
import DA.Ledger.App.MasterCopy.Logging
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import Data.UUID
import System.Random

import Debug.Trace

data TemplateACS = TemplateACS {
    contracts :: Map.Map ContractId Record,
    pending :: Map.Map ContractId Record,
    pendingByCommandId :: Map.Map CommandId (Set.Set ContractId)
} deriving Show

data ACS = ACS {
    templateACSs :: Map.Map TemplateId TemplateACS,
    commandsInFlight :: Map.Map CommandId (Maybe Message, Commands)
} deriving Show

type PendingSet = Map.Map TemplateId (Set.Set ContractId)

emptyACS :: ACS
emptyACS = ACS {
    templateACSs = Map.empty,
    commandsInFlight = Map.empty
}

emptyTemplateACS :: TemplateACS
emptyTemplateACS = TemplateACS {
    contracts = Map.empty,
    pending = Map.empty,
    pendingByCommandId = Map.empty
}

data BotState cs = BotState {
    custom :: cs,
    acs :: ACS,
    uuids :: [UUID]
}

instance Show cs => Show (BotState cs) where
    show BotState{custom, acs} = "Active Contract Set: " <> show acs <> "\nCustom State: " <> show custom

type StateUpdate cs = BotState cs -> Event -> cs
type Rule cs = BotState cs -> Maybe Event -> [([Command], PendingSet)]
type Recovery cs = BotState cs -> Maybe Message -> Commands -> CommandCompletion -> [([Command], PendingSet)]

data TimeMode = Static | Wallclock
data TimeSettings = TimeSettings {
    mode :: TimeMode,
    ttl :: Integer
}

acsProcessEvent :: ACS -> Event -> ACS
acsProcessEvent a = \case
    CreatedEvent{cid, tid, createArgs} -> insertIntoACS a tid cid createArgs
    ArchivedEvent{cid, tid} -> removeFromACS a tid cid

insertIntoACS :: ACS -> TemplateId -> ContractId -> Record -> ACS
insertIntoACS a@ACS{templateACSs} tid cid r = a{templateACSs=ta'}
    where
        tacs@TemplateACS{contracts} = fromMaybe emptyTemplateACS (Map.lookup tid templateACSs)
        ta' = Map.insert tid tacs{contracts = Map.insert cid r contracts} templateACSs

removeFromACS :: ACS -> TemplateId -> ContractId -> ACS
removeFromACS a@ACS{templateACSs} tid cid = a{templateACSs=ta'}
    where
        tacs@TemplateACS{contracts} = fromMaybe emptyTemplateACS (Map.lookup tid templateACSs)
        ta' = Map.insert tid tacs{contracts = Map.delete cid contracts} templateACSs

acsProcessCompletion :: ACS -> CommandCompletion -> ACS
acsProcessCompletion a@ACS{templateACSs, commandsInFlight} c@CommandCompletion{command_id, result} =
    ACS{templateACSs=templateACSs', commandsInFlight=commandsInFlight'}
    where
        commandsInFlight' = Map.delete command_id commandsInFlight
        tacsAction =  case traceShowId result of
            Left _ -> processCommandFailure
            Right _ -> processCommandSuccess
        templateACSs' = Map.map (tacsAction command_id) templateACSs

processCommandSuccess :: CommandId -> TemplateACS -> TemplateACS
processCommandSuccess cmdid tacs@TemplateACS{pending, pendingByCommandId} =
    tacs{pending=pending', pendingByCommandId=pendingByCommandId'}
    where
        mpending = Map.lookup cmdid pendingByCommandId
        pendingByCommandId' = Map.delete cmdid pendingByCommandId
        cids = maybe [] Set.toList mpending
        pending' = foldl (flip Map.delete) pending cids

processCommandFailure :: CommandId -> TemplateACS -> TemplateACS
processCommandFailure cmdid tacs@TemplateACS{contracts, pending, pendingByCommandId} =
    tacs{contracts=contracts', pending=pending', pendingByCommandId=pendingByCommandId'}
    where
        mpending = Map.lookup cmdid pendingByCommandId
        pendingByCommandId' = Map.delete cmdid pendingByCommandId
        cids = fromMaybe Set.empty mpending
        (pending', contracts') = transfer cids pending contracts
        

processEvent :: StateUpdate s -> BotState s  -> Event -> BotState s
processEvent upd bs e =
    let acs' = acsProcessEvent (acs bs) e
        custom = upd (bs{acs=acs'}) e
    in bs{acs=acs', custom}

processEvents :: StateUpdate s -> BotState s -> [Event] -> BotState s
processEvents upd = foldl (processEvent upd)

addToPending :: BotState cs -> (UUID, PendingSet) -> BotState cs
addToPending bs (uuid, ps) = bs{acs=acs'}
    where
        cmdid = CommandId . T.pack . toString $ uuid
        acs'@ACS{commandsInFlight} = foldl addToTemplatePending (acs bs) (Map.toList ps)
        addToTemplatePending a@ACS{templateACSs} (tid, cids) = a{templateACSs = ta'}
            where
                tacs@TemplateACS{contracts, pending, pendingByCommandId} = fromMaybe emptyTemplateACS (Map.lookup tid templateACSs)
                pendingByCommandId' = Map.insert cmdid cids pendingByCommandId
                (contracts', pending') = transfer cids contracts pending
                ta' = Map.insert tid (TemplateACS contracts' pending' pendingByCommandId') templateACSs

transfer :: (Eq k, Ord k) => Set.Set k -> Map.Map k v -> Map.Map k v -> (Map.Map k v, Map.Map k v)
transfer ks f t = (f', t')
    where
        (f', nt) = Map.partitionWithKey (\k _ -> k `notElem` ks) f
        t' = Map.union t nt

addToInFlight :: Message -> BotState cs -> Commands -> BotState cs
addToInFlight m bs@BotState{acs=acs@ACS{commandsInFlight}} cmds@Commands{cid} = 
    bs{acs=acs{commandsInFlight=Map.insert cid (Just m, cmds) commandsInFlight}}

processCommands :: BotContext -> TimeSettings -> Timestamp -> Message -> [([Command], PendingSet)] -> BotState cs -> ([Commands], BotState cs)
processCommands BotContext{aid, party, lid} ts  systime m cmds bs = (cs, bs3)
    where
        (uuids, bs1) = takeUUIDs bs (length cmds)
        ps = zip uuids (map snd cmds)
        bs2 = foldl addToPending bs1 ps
        bs3 = foldl (addToInFlight m) bs2 cs
        cs = zipWith mkCommand uuids (map fst cmds)
        leTime@Timestamp{seconds, nanos} = case mode ts of
            Static -> Timestamp 0 0
            Wallclock -> systime
        mkCommand uuid coms =
            Commands {
            lid,
            wid = Nothing,
            aid,
            cid = CommandId . T.pack . toString $ uuid,
            party,
            leTime,
            mrTime = Timestamp (seconds + ttl ts) nanos,
            coms
        }

randomUUIDs :: IO [UUID]
randomUUIDs = uuids <$> getStdGen
    where
        toFours = \case
            x1:x2:x3:x4:xs -> (x1, x2, x3, x4) : toFours xs
            _ -> error "Four elements expected"
        uncurry4 f (x1, x2, x3, x4) = f x1 x2 x3 x4
        uuids g = map (uncurry4 fromWords) (toFours (randoms g))

takeUUIDs :: BotState cs -> Int -> ([UUID], BotState cs)
takeUUIDs bs@BotState{uuids} n = (take n uuids, bs{uuids = drop n uuids})

simpleRuleNanobot
    :: Show cs
    => Logger
    -> TimeSettings
    -> BotContext
    -> StateUpdate cs
    -> Rule cs
    -> Recovery cs
    -> cs
    -> IO ()
simpleRuleNanobot log ts bc upd rule recov init
    = do
        uuids <- randomUUIDs
        nanobot log bc (srnProcessMessage ts upd rule recov) (BotState {acs = emptyACS, custom = init, uuids})

srnProcessMessage
    :: Show cs
    => TimeSettings
    -> StateUpdate cs
    -> Rule cs
    -> Recovery cs
    -> BotContext
    -> Message
    -> Timestamp
    -> BotState cs
    -> ([Commands], BotState cs)
srnProcessMessage ts upd rule recov bc m systime bs = 
    case m of
        MActiveContracts ces ->
            let newbs = processEvents upd bs ces
            in processCommands bc ts systime m (rule newbs Nothing) newbs
        MTransaction Transaction{events} ->
            let newbs = processEvents upd bs events
                commands = concatMap (rule newbs . Just) events
            in processCommands bc ts systime m commands newbs
        MCompletion cp@CommandCompletion{command_id, result} -> case result of
            Right _ ->([], bs{acs = acsProcessCompletion (acs bs) cp})
            Left _ -> let newbs = bs{acs = acsProcessCompletion (acs bs) cp}
                        in case Map.lookup command_id (commandsInFlight (acs bs)) of
                            Just (e, cmd) -> processCommands bc ts systime m (recov newbs e cmd cp) newbs
                            Nothing -> processCommands bc ts systime m (rule newbs Nothing) newbs
