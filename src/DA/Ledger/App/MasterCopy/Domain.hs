-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Ledger.App.MasterCopy.Domain (
    Party(..),
    Master(..),
    Copy(..),
    ) where

import DA.Ledger.IsLedgerValue (IsLedgerValue(..))
import DA.Ledger.Types (Party(..),Value(..))
import Data.Dynamic
import Data.Text.Lazy (Text)

data Master = Master { owner :: Party, name :: Text, info :: Text }
    deriving (Show, Typeable, Eq, Ord)

instance IsLedgerValue Master where
    toValue Master{owner, name, info} = VList [toValue owner, toValue name, toValue info]
    fromValue = \case
        VList [v1,v2, v3] -> do
            owner <- fromValue v1
            name <- fromValue v2
            info <- fromValue v3
            return Master{owner, name, info}
        _ -> Nothing

data Copy = Copy { master :: Master, obs :: Party }
    deriving (Show, Typeable, Eq, Ord)

instance IsLedgerValue Copy where
    toValue Copy{master, obs} = VList [VRecord $ toRecord master, toValue obs]
    fromValue = \case
        VList [v1,v2] -> do
            master <- fromValue v1
            obs <- fromValue v2
            return Copy{master, obs}
        _ -> Nothing
