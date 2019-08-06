module DA.Ledger.App.MasterCopy.RuleBot () where

import DA.Ledger as Ledger
import DA.Ledger.App.MasterCopy.Bot
import DA.Ledger.App.MasterCopy.Logging
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import Data.UUID
import System.Random

data TemplateACS = TemplateACS {
    contracts :: Map.Map ContractId Record,
    pending :: Set.Set ContractId,
    pendingByCommandId :: Map.Map CommandId (Set.Set ContractId)
}

data ACS = ACS {
    templateACSs :: Map.Map TemplateId TemplateACS,
    commandsInFlight :: Map.Map CommandId (Maybe Message, Commands)
}

type PendingSet = Map.Map TemplateId (Set.Set ContractId)

emptyACS :: ACS
emptyACS = ACS {
    templateACSs = Map.empty,
    commandsInFlight = Map.empty
}

emptyTemplateACS :: TemplateACS
emptyTemplateACS = TemplateACS {
    contracts = Map.empty,
    pending = Set.empty,
    pendingByCommandId = Map.empty
}

data BotState cs = BotState {
    custom :: cs,
    acs :: ACS,
    uuids :: [UUID]
}

type StateUpdate cs = BotState cs -> Event -> cs
type Rule cs = BotState cs -> Maybe Event -> [[Command]]
type Recovery cs = BotState cs -> Maybe Message -> Commands -> CommandCompletion -> [[Command]]

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
acsProcessCompletion a c = a -- TODO -- Do the obvious thing to purge or re-instate pending set

processEvent :: StateUpdate s -> BotState s  -> Event -> BotState s
processEvent upd bs e =
    let acs' = acsProcessEvent (acs bs) e
        custom = upd (bs{acs=acs'}) e
    in bs{acs=acs', custom}

processEvents :: StateUpdate s -> BotState s -> [Event] -> BotState s
processEvents upd = foldl (processEvent upd)

mkCommands :: BotContext -> Timestamp -> [[Command]] -> BotState cs -> ([Commands], BotState cs)
mkCommands BotContext{aid, party, lid} ledger_effective_time cmds bs = (cs, bs')
    where
        (uuids, bs') = takeUUIDs bs (length cmds)
        cs = zipWith mkCommand uuids cmds
        mkCommand uuid coms =
            Commands {
            lid,
            wid = Nothing,
            aid,
            cid = CommandId . T.pack . toString $ uuid,
            party,
            leTime = Timestamp 0 0,
            mrTime = Timestamp 5 0,
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
    :: Logger
    -> BotContext
    -> StateUpdate cs
    -> Rule cs
    -> Recovery cs
    -> cs
    -> IO ()
simpleRuleNanobot log bc upd rule recov init
    = do
        uuids <- randomUUIDs
        nanobot log bc (srnProcessMessage log upd rule recov) (BotState {acs = emptyACS, custom = init, uuids})

srnProcessMessage
    :: Logger
    -> StateUpdate cs
    -> Rule cs
    -> Recovery cs
    -> BotContext
    -> Message
    -> Timestamp
    -> BotState cs
    -> ([Commands], BotState cs)
srnProcessMessage log upd rule recov bc m t bs = case m of
    MActiveContracts ces ->
        let newbs = processEvents upd bs ces
        in mkCommands bc t (rule newbs Nothing) newbs
    MTransaction Transaction{events} ->
        let newbs = processEvents upd bs events
            commands = concatMap (rule newbs . Just) events
        in mkCommands bc t commands newbs
    MCompletion cp@CommandCompletion{command_id, result} -> case result of
        Right _ ->([], bs{acs = acsProcessCompletion (acs bs) cp})
        Left _ -> let newbs = bs{acs = acsProcessCompletion (acs bs) cp}
                    in case Map.lookup command_id (commandsInFlight (acs bs)) of
                        Just (e, cmd) -> mkCommands bc t (recov newbs e cmd cp) newbs
                        Nothing -> mkCommands bc t (rule newbs Nothing) bs