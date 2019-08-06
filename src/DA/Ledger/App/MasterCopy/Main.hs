
-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
module DA.Ledger.App.MasterCopy.Main (main) where

import Control.Monad.Trans.Class (lift)
import DA.Ledger as Ledger
import DA.Ledger.App.MasterCopy.Bot
import DA.Ledger.App.MasterCopy.Contracts
import DA.Ledger.App.MasterCopy.Domain (Party(..), Copy(..))
import DA.Ledger.App.MasterCopy.Logging (Logger, colourLog,plainLog,colourWrap)
import DA.Ledger.App.MasterCopy.MCLedger
import Data.Maybe
import Data.UUID
import qualified Data.Text.Lazy as T
import System.Console.ANSI (Color(..))
import System.Environment (getArgs)
import System.Random

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just party -> runMain party
        Nothing -> do
            putStrLn $ "failed to parse command line: " <> show args
            runMain defaultParty

randomUUIDs :: IO [UUID]
randomUUIDs = uuids <$> getStdGen
    where
        toFours = \case
            x1:x2:x3:x4:xs -> (x1, x2, x3, x4) : toFours xs
            _ -> error "Four elements expected"
        uncurry4 f (x1, x2, x3, x4) = f x1 x2 x3 x4
        uuids g = map (uncurry4 fromWords) (toFours (randoms g))


runMain :: Party -> IO ()
runMain p = do
    let errLog = putStrLn
    h@Handle{lid, pid, log} <- connect errLog
    errLog "Starting bot."
    let bc = BotContext (ApplicationId "CopyBot") p lid
    uuids <- randomUUIDs
    nanobot log bc (copyMaster pid) uuids

copyMaster :: PackageId -> BotContext -> Message -> Timestamp -> [UUID] -> ([Commands], [UUID])
copyMaster pid bc m t uuids = case m of
    MActiveContracts es -> copyNewMasters pid bc es t uuids
    MTransaction Transaction{events} -> copyNewMasters pid bc events t uuids
    MCompletion _ -> ([], uuids)

copyNewMasters :: PackageId -> BotContext -> [Event] -> Timestamp -> [UUID] -> ([Commands], [UUID])
copyNewMasters pid BotContext{aid, party, lid} es t uuids = (toCommands t copies, tail uuids)
    where
        contracts = map extractCreateEvent es
        cp = \case
            Just (CMaster master) -> Just (CCopy Copy {master, obs=party})
            _ -> Nothing
        copies = mapMaybe cp contracts
        toCommands t = \case
            [] -> []
            cs -> [Commands {
                    lid,
                    wid = Nothing,
                    aid,
                    cid = CommandId . T.pack . toString . head $ uuids,
                    party,
                    leTime = Timestamp 0 0,
                    mrTime = Timestamp 5 0,
                    coms = map (makeLedgerCommand pid) cs
                }]

parseArgs :: [String] -> Maybe Party
parseArgs = \case
    [who] -> Just (Party (T.pack who))
    [] -> Just defaultParty
    _ -> Nothing

defaultParty :: Party
defaultParty = Party "Alice"
