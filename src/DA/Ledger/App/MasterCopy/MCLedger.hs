-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Ledger.App.MasterCopy.MCLedger (Handle(..), connect, run) where

import DA.Ledger as Ledger
import DA.Ledger.App.MasterCopy.Contracts (MCContract,makeLedgerCommand)
import DA.Ledger.App.MasterCopy.Logging (Logger)
import Data.Maybe (maybeToList, isJust, catMaybes)
import Data.Text (pack)
import qualified DA.Daml.LF.Ast as LF
import qualified Data.NameMap as NM

data Handle = Handle {
    log :: Logger,
    lid :: LedgerId,
    pid :: PackageId
    }

port :: Port
port = 6865 -- port on which we expect to find a ledger. should be a command line option

run :: TimeoutSeconds -> LedgerService a -> IO a
run timeout ls  = runLedgerService ls timeout (configOfPort port)

connect :: Logger -> IO Handle
connect log = do
    lid <- run 5 getLedgerIdentity
    ids <- run 5 $ listPackages lid
    mpackages <- run 5 $ mapM (getPackage lid) ids
    let
        packages = zip ids mpackages
        pid = fst . head . Prelude.filter (hasTemplate "MasterCopy" "Master") $ packages
    return Handle{log,lid,pid}

hasTemplate :: String -> String -> (PackageId, Maybe LF.Package) -> Bool
hasTemplate m t mp = isJust $ do
    p <- snd mp
    mod <- NM.lookup (LF.ModuleName [pack m]) (LF.packageModules p)
    NM.lookup (LF.TypeConName [pack t]) (LF.moduleTemplates mod)
