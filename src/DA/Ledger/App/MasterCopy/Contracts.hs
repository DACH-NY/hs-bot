-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Ledger.App.MasterCopy.Contracts (
    MCContract(..),
    makeCreateCommand,makeArchiveCommand,extractCreateEvent,getTid
    ) where

import DA.Ledger (
    PackageId,Command,ModuleName(..),EntityName(..),TemplateId(..),Identifier(..),
    Command(..), Event(..), Transaction(..)
    )

import DA.Ledger.App.MasterCopy.Domain (Master, Copy, Subscriber)
import DA.Ledger.App.MasterCopy.Logging (Logger)
import DA.Ledger.IsLedgerValue (toRecord,fromRecord)
import DA.Ledger.Types
import Data.Text.Internal.Lazy(Text)

data MCContract
    = CMaster Master
    | CCopy Copy
    | CSubscriber Subscriber

makeCreateCommand :: PackageId -> MCContract -> Command
makeCreateCommand pid = \case
    CMaster x -> CreateCommand {tid = getTid pid "Master", args=toRecord x}
    CCopy x -> CreateCommand {tid = getTid pid "Copy", args=toRecord x}
    CSubscriber x -> CreateCommand {tid = getTid pid "Subscriber", args=toRecord x}

makeArchiveCommand :: ContractId -> TemplateId -> Command
makeArchiveCommand cid tid = ExerciseCommand{tid, cid, choice=Choice "Archive", arg=VRecord (Record Nothing [])}

extractCreateEvent :: Event -> Maybe MCContract
extractCreateEvent = \case
    CreatedEvent{tid=TemplateId Identifier{ent=EntityName"Master"}, createArgs} -> do
        x <- fromRecord createArgs
        return $ CMaster x
    CreatedEvent{tid=TemplateId Identifier{ent=EntityName"Copy"}, createArgs} -> do
        x <- fromRecord createArgs
        return $ CCopy x
    CreatedEvent{tid=TemplateId Identifier{ent=EntityName"Subscriber"}, createArgs} -> do
        x <- fromRecord createArgs
        return $ CSubscriber x
    _ ->
        Nothing

getTid :: PackageId -> Text -> TemplateId
getTid pid s = TemplateId (Identifier pid (ModuleName "MasterCopy") (EntityName s))
