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
import Data.Tuple.Extra

data TemplateACS = TemplateACS {
    contracts :: Map.Map ContractId Record,
    pending :: Map.Map ContractId Record,
    pendingByCommandId :: Map.Map CommandId (Set.Set ContractId)
} deriving Show

data ACS cm = ACS {
    templateACSs :: Map.Map TemplateId TemplateACS,
    commandsInFlight :: Map.Map CommandId (cm, Commands)
} deriving Show

type PendingSet = Map.Map TemplateId (Set.Set ContractId)

emptyACS :: ACS cm
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

data BotState cs cm = BotState {
    custom :: cs,
    acs :: ACS cm,
    uuids :: [UUID]
}

instance (Show cs, Show cm) => Show (BotState cs cm) where
    show BotState{custom, acs} = "Active Contract Set: " <> show acs <> "\nCustom State: " <> show custom

type StateUpdate cs cm = BotState cs cm-> Event -> cs
type Rule cs cm = BotState cs cm -> Maybe Event -> [(cm, [Command], PendingSet)]
type Recovery cs cm = BotState cs cm -> cm -> Commands -> Rejection -> [(cm, [Command], PendingSet)]

data TimeMode = Static | Wallclock
data TimeSettings = TimeSettings {
    mode :: TimeMode,
    ttl :: Integer
}

acsProcessEvent :: ACS cm -> Event -> ACS cm
acsProcessEvent a = \case
    CreatedEvent{cid, tid, createArgs} -> insertIntoACS a tid cid createArgs
    ArchivedEvent{cid, tid} -> removeFromACS a tid cid

insertIntoACS :: ACS cm -> TemplateId -> ContractId -> Record -> ACS cm
insertIntoACS a@ACS{templateACSs} tid cid r = a{templateACSs=ta'}
    where
        tacs@TemplateACS{contracts} = fromMaybe emptyTemplateACS (Map.lookup tid templateACSs)
        ta' = Map.insert tid tacs{contracts = Map.insert cid r contracts} templateACSs

removeFromACS :: ACS cm -> TemplateId -> ContractId -> ACS cm
removeFromACS a@ACS{templateACSs} tid cid = a{templateACSs=ta'}
    where
        tacs@TemplateACS{contracts, pending} = fromMaybe emptyTemplateACS (Map.lookup tid templateACSs)
        ta' = Map.insert tid tacs{contracts = Map.delete cid contracts, pending = Map.delete cid pending} templateACSs

acsProcessCompletion :: ACS cm -> CommandCompletion -> ACS cm
acsProcessCompletion a@ACS{templateACSs, commandsInFlight} c@CommandCompletion{command_id, result} =
    ACS{templateACSs=templateACSs', commandsInFlight=commandsInFlight'}
    where
        commandsInFlight' = Map.delete command_id commandsInFlight
        templateACSs' = Map.map (tacsProcessCompletion command_id) templateACSs

tacsProcessCompletion :: CommandId -> TemplateACS -> TemplateACS
tacsProcessCompletion cmdid tacs@TemplateACS{contracts, pending, pendingByCommandId} =
    tacs{contracts=contracts', pending=pending', pendingByCommandId=pendingByCommandId'}
    where
        mpending = Map.lookup cmdid pendingByCommandId
        pendingByCommandId' = Map.delete cmdid pendingByCommandId
        cids = fromMaybe Set.empty mpending
        (pending', contracts') = transfer cids pending contracts
        

processEvent :: StateUpdate cs cm -> BotState cs cm  -> Event -> BotState cs cm
processEvent upd bs e =
    let acs' = acsProcessEvent (acs bs) e
        custom = upd (bs{acs=acs'}) e
    in bs{acs=acs', custom}

processEvents :: StateUpdate cs cm -> BotState cs cm -> [Event] -> BotState cs cm
processEvents upd = foldl (processEvent upd)

addToPending :: BotState cs cm -> (UUID, PendingSet) -> BotState cs cm
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

addToInFlight :: BotState cs cm -> (cm, Commands) -> BotState cs cm
addToInFlight bs@BotState{acs=acs@ACS{commandsInFlight}} (cm, cmds@Commands{cid}) = 
    bs{acs=acs{commandsInFlight=Map.insert cid (cm, cmds) commandsInFlight}}

processCommands :: BotContext -> TimeSettings -> Timestamp -> [(cm, [Command], PendingSet)] -> BotState cs cm -> ([Commands], BotState cs cm)
processCommands BotContext{aid, party, lid} ts  systime cmds bs = (map snd cs, bs3)
    where
        (uuids, bs1) = takeUUIDs bs (length cmds)
        ps = zip uuids (map thd3 cmds)
        bs2 = foldl addToPending bs1 ps
        bs3 = foldl addToInFlight bs2 cs
        cs = zipWith mkCommand uuids cmds
        leTime@Timestamp{seconds, nanos} = case mode ts of
            Static -> Timestamp 0 0
            Wallclock -> systime
        mkCommand uuid (cm, coms, _) =
            (
                cm,
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
            )

randomUUIDs :: IO [UUID]
randomUUIDs = uuids <$> getStdGen
    where
        toFours = \case
            x1:x2:x3:x4:xs -> (x1, x2, x3, x4) : toFours xs
            _ -> error "Four elements expected"
        uncurry4 f (x1, x2, x3, x4) = f x1 x2 x3 x4
        uuids g = map (uncurry4 fromWords) (toFours (randoms g))

takeUUIDs :: BotState cs cm -> Int -> ([UUID], BotState cs cm)
takeUUIDs bs@BotState{uuids} n = (take n uuids, bs{uuids = drop n uuids})

simpleRuleNanobot
    :: (Show cs, Show cm)
    => Logger
    -> TimeSettings
    -> BotContext
    -> StateUpdate cs cm
    -> Rule cs cm
    -> Recovery cs cm
    -> cs
    -> IO ()
simpleRuleNanobot log ts bc upd rule recov init
    = do
        uuids <- randomUUIDs
        nanobot log bc (srnProcessMessage ts upd rule recov) (BotState {acs = emptyACS, custom = init, uuids})

srnProcessMessage
    :: (Show cs, Show cm)
    => TimeSettings
    -> StateUpdate cs cm
    -> Rule cs cm
    -> Recovery cs cm
    -> BotContext
    -> Message
    -> Timestamp
    -> BotState cs cm
    -> ([Commands], BotState cs cm)
srnProcessMessage ts upd rule recov bc m systime bs = 
    case m of
        MActiveContracts ces ->
            let newbs = processEvents upd bs ces
            in processCommands bc ts systime (rule newbs Nothing) newbs
        MTransaction Transaction{events} ->
            let newbs = processEvents upd bs events
                commands = concatMap (rule newbs . Just) events
            in processCommands bc ts systime commands newbs
        MCompletion cp@CommandCompletion{command_id, result} -> case result of
            Right _ ->([], bs{acs = acsProcessCompletion (acs bs) cp})
            Left rej -> let newbs = bs{acs = acsProcessCompletion (acs bs) cp}
                        in case Map.lookup command_id (commandsInFlight (acs bs)) of
                            Just (e, cmd) -> processCommands bc ts systime (recov newbs e cmd rej) newbs
                            Nothing -> processCommands bc ts systime (rule newbs Nothing) newbs
