# hs-bot

A simple example of a Haskell Nanobot for DAML. The Bot abstraction is defined in `DA.Ledger.App.MasterCopy.Bot`. An
example is given in the `Main` class. It listens for any `create` events of `Master` contracts and creates copies thereof.

# On MacOS

## Requirements:

1. Homebrew (https://brew.sh/)
1. Stack, OpenSSL and c-ares (`brew install haskell-stack openssl c-ares`)
1. Working DAML Dev Env (https://github.com/digital-asset/daml)
1. gRPC v1.2.2 (https://github.com/grpc/grpc/blob/v1.22.x/BUILDING.md)
1. Compiled Haskell Bindings (https://github.com/digital-asset/daml/tree/master/language-support/hs/bindings)
1. Installed DAML SDK (https://docs.daml.com/getting-started/installation.html)

## Building

1. Copy the compiled bindings into the folder lib
2. Build with `stack build`


# Running

1. Start Sandbox and Navigator with `daml start`
1. Create some contracts for Alice through Navigator
1. Start client with `stack run hs-bot-exe -- Alice`
1. Create some more contracts for Alice through Navigator