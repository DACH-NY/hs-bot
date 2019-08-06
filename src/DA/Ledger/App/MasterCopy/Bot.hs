module DA.Ledger.App.MasterCopy.Bot (Message(..), BotContext(..), nanobot) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import DA.Ledger as Ledger
import DA.Ledger.App.MasterCopy.Logging (colourLog,plainLog,colourWrap, Logger)
import DA.Ledger.App.MasterCopy.MCLedger as MCLedger
import Data.Dynamic
import Data.Map
import Data.Set
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Tuple.Extra
import qualified Data.Text as Text.Read
import qualified Data.Text.Lazy as Text (pack)
import System.Console.ANSI (Color(..))

-- Low level Nanobot
--------------------

type Rejection = String

data Message
    = MActiveContracts [Event]
    | MTransaction Transaction
    | MCompletion Completion
    deriving (Show)

getTimestamp :: IO Timestamp
getTimestamp = do
    now <- getPOSIXTime
    return (Timestamp (round now) 0)

data CommandCompletion = CommandCompletion {
    command_id :: CommandId,
    result :: Either Rejection TransactionId
} deriving (Show)

submitCommands :: Logger -> LedgerId -> Commands -> IO CommandCompletion
submitCommands log lid cs@Commands{cid} = do
    result <- run 5 $ Ledger.submitAndWaitForTransactionId cs
    let cp = CommandCompletion cid result
    log $ "Command completion: " <> show cp
    return (CommandCompletion cid result)

data BotContext = BotContext {
    aid :: ApplicationId,
    party :: Party,
    lid :: LedgerId
} deriving (Show)

nanobot
    :: Logger
    -> BotContext
    -> (BotContext -> Message -> Timestamp -> s -> ([Commands], s))
    -> s
    -> IO()
nanobot log bc@BotContext{lid, party} u s = do
    log "Creating Channel"
    chan <- newChan
    log "Calling Active Contract Service"
    (offset, ces) <- getACS
    log $ "ACS delivered contracts to offset " <> show offset <> " " <> show ces
    writeChan chan (MActiveContracts ces)
    log "Calling Transaction Service"
    txs <- MCLedger.run 600 $ getTransactions (txsReq offset)
    eor <- async (txloop chan txs offset)
    eow <- async (uptloop s chan (u bc))
    wait eor
    wait eow
    where
        filter = filterEverthingForParty party
        verbose = Verbosity False
        txsReq os = GetTransactionsRequest lid (LedgerAbsOffset os) Nothing filter verbose
        getACS = do
            rs <- MCLedger.run 600 $ getActiveContracts lid filter verbose
            let ces = concatMap thd3 rs
                offset = fst3 (last rs)
            return (offset, ces)
        txloop c ts os = do
            r <- takeStream ts
            case r of
                Left _ -> do
                    log "End of transaction stream reached. Re-connecting."
                    ts <- MCLedger.run 600 $ getTransactions (txsReq os)
                    txloop c ts os
                Right t -> do
                    log $ "Transactions received: " <> show t
                    writeList2Chan c (Prelude.map MTransaction t)
                    txloop c ts os
        uptloop s c u = do
            m <- readChan c
            t <- getTimestamp
            log $ "Processing message at: " <> show t <> ": " <> show m
            let (cs, s') = u m t s
            mapM_ (async . submitCommands log lid) cs
            uptloop s' c u
