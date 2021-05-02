-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Arguments(Arguments(..), getArguments) where

import           Development.IDE.Main (Command (..), commandP)
import           Options.Applicative

data Arguments = Arguments
    {argsVersion               :: Bool
    ,argsVSCodeExtensionSchema :: Bool
    ,argsDefaultConfig         :: Bool
    ,argsShakeProfiling        :: Maybe FilePath
    ,argsOTMemoryProfiling     :: Bool
    ,argsThreads               :: Int
    ,argsVerbose               :: Bool
    ,argsCommand               :: Command
    }

getArguments :: IO Arguments
getArguments = execParser opts
  where
    opts = info (arguments <**> helper)
      ( fullDesc
     <> header "ghcide - the core of a Haskell IDE")

arguments :: Parser Arguments
arguments = Arguments
      <$> switch (long "version" <> help "Show ghcide and GHC versions")
      <*> switch (long "vscode-extension-schema" <> help "Print generic config schema for plugins (used in the package.json of haskell vscode extension)")
      <*> switch (long "generate-default-config" <> help "Print config supported by the server with default values")
      <*> optional (strOption $ long "shake-profiling" <> metavar "DIR" <> help "Dump profiling reports to this directory")
      <*> switch (long "ot-memory-profiling" <> help "Record OpenTelemetry info to the eventlog. Needs the -l RTS flag to have an effect")
      <*> option auto (short 'j' <> help "Number of threads (0: automatic)" <> metavar "NUM" <> value 0 <> showDefault)
      <*> switch (long "verbose" <> help "Include internal events in logging output")
      <*> (commandP <|> lspCommand <|> checkCommand)
      where
          checkCommand = Check <$> many (argument str (metavar "FILES/DIRS..."))
          lspCommand = LSP <$ flag' True (long "lsp" <> help "Start talking to an LSP client")
