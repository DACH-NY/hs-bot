-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Ledger.App.MasterCopy.Contracts (
    MCContract(..),
    makeLedgerCommand,extractCreateEvent,
    ) where

import DA.Ledger (
    PackageId,Command,ModuleName(..),EntityName(..),TemplateId(..),Identifier(..),
    Command(..), Event(..), Transaction(..)
    )

import DA.Ledger.App.MasterCopy.Domain (Master, Copy)
import DA.Ledger.App.MasterCopy.Logging (Logger)
import DA.Ledger.IsLedgerValue (toRecord,fromRecord)
import DA.Ledger.Types
import Data.Text.Internal.Lazy(Text)

data MCContract
    = CMaster Master
    | CCopy Copy

makeLedgerCommand :: PackageId -> MCContract -> Command
makeLedgerCommand pid = \case
    CMaster x -> makeLedgerCommandInner "MasterCopy" "Master" (toRecord x) pid
    CCopy x -> makeLedgerCommandInner "MasterCopy" "Copy" (toRecord x) pid

makeLedgerCommandInner :: Text -> Text -> Record -> PackageId -> Command
makeLedgerCommandInner m e args pid = do
    let mod = ModuleName m
    let ent = EntityName e
    let tid = TemplateId (Identifier pid mod ent)
    CreateCommand {tid,args}

extractCreateEvent :: Event -> Maybe MCContract
extractCreateEvent = \case
    CreatedEvent{tid=TemplateId Identifier{ent=EntityName"Master"}, createArgs} -> do
        x <- fromRecord createArgs
        return $ CMaster x
    CreatedEvent{tid=TemplateId Identifier{ent=EntityName"Copy"}, createArgs} -> do
        x <- fromRecord createArgs
        return $ CCopy x
    _ ->
        Nothing
