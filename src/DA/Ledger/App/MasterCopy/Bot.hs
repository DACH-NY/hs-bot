{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

module DA.Ledger.App.MasterCopy.Bot (Message(..), BotContext(..), nanobot, CommandCompletion(..), Rejection) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import DA.Ledger as Ledger
import DA.Ledger.App.MasterCopy.Logging (colourLog,plainLog,colourWrap, Logger)
import DA.Ledger.App.MasterCopy.MCLedger as MCLedger
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Tuple.Extra
import qualified Data.Text.Lazy as Text (pack)

-- Low level Nanobot
--------------------

deriving instance Show Commands

type Rejection = String

data Message
    = MActiveContracts [Event]
    | MTransaction Transaction
    | MCompletion CommandCompletion
    deriving (Show)

getTimestamp :: IO Timestamp
getTimestamp = do
    now <- getPOSIXTime
    return (Timestamp (round now) 0)

data CommandCompletion = CommandCompletion {
    command_id :: CommandId,
    result :: Either Rejection TransactionId
} deriving (Show)

submitCommands :: Logger -> LedgerId -> Chan Message -> Commands -> IO ()
submitCommands log lid chan cs@Commands{cid} = do
    result <- run 5 $ Ledger.submitAndWaitForTransactionId cs
    let cp = CommandCompletion cid result
    log $ "Command completion: " <> show cp
    writeChan chan (MCompletion cp)

parallel :: [IO a] -> IO [a]
parallel (x:xs) = withAsync x (\r -> do
        ys <- parallel xs
        y <- wait r
        return (y : ys)
    )

data BotContext = BotContext {
    aid :: ApplicationId,
    party :: Party,
    lid :: LedgerId
} deriving (Show)

nanobot
    :: Show s
    => Logger
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
    log $ "Initial State: " <> show s
    withAsync (txloop chan txs offset) $ \eor -> do
        uptloop s chan (u bc)
        wait eor
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
        uptloop !s c u = do
            m <- readChan c
            t <- getTimestamp
            log $ "Processing message at: " <> show t <> ": " <> show m
            let (cs, s') = u m t s
            log $ "New State: " <> show s'
            log $ "New Commands: " <> show cs
            withAsync (mapConcurrently_  (submitCommands log lid c) cs) (\eoc -> do
                    uptloop s' c u
                    wait eoc
                )
