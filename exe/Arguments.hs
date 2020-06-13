-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above

module Arguments
  ( Arguments(..)
  , getArguments
  , ghcideVersion
  , getGhcLibDir
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Version
import Development.GitRev
import qualified GHC.Paths
import HIE.Bios.Types
import Options.Applicative
import Paths_haskell_language_server
import System.Environment
import System.Process

-- ---------------------------------------------------------------------

data Arguments = Arguments
    {argLSP :: Bool
    ,argsCwd :: Maybe FilePath
    ,argFiles :: [FilePath]
    ,argsVersion :: Bool
    ,argsShakeProfiling :: Maybe FilePath
    ,argsTesting :: Bool
    ,argsExamplePlugin :: Bool
    -- These next two are for compatibility with existing hie clients, allowing
    -- them to just change the name of the exe and still work.
    , argsDebugOn       :: Bool
    , argsLogFile       :: Maybe String
    , argsThreads       :: Int
    , argsProjectGhcVersion :: Bool
    } deriving Show

getArguments :: String -> IO Arguments
getArguments exeName = execParser opts
  where
    opts = info (arguments exeName <**> helper)
      ( fullDesc
     <> progDesc "Used as a test bed to check your IDE Client will work"
     <> header (exeName ++ " - GHC Haskell LSP server"))

arguments :: String -> Parser Arguments
arguments exeName = Arguments
      <$> switch (long "lsp" <> help "Start talking to an LSP server")
      <*> optional (strOption $ long "cwd" <> metavar "DIR"
                  <> help "Change to this directory")
      <*> many (argument str (metavar "FILES/DIRS..."))
      <*> switch (long "version"
                  <> help ("Show " ++ exeName  ++ " and GHC versions"))
      <*> optional (strOption $ long "shake-profiling" <> metavar "DIR"
                  <> help "Dump profiling reports to this directory")
      <*> switch (long "test"
                  <> help "Enable additional lsp messages used by the testsuite")
      <*> switch (long "example"
                  <> help "Include the Example Plugin. For Plugin devs only")

      <*> switch
           ( long "debug"
          <> short 'd'
          <> help "Generate debug output"
           )
      <*> optional (strOption
           ( long "logfile"
          <> short 'l'
          <> metavar "LOGFILE"
          <> help "File to log to, defaults to stdout"
           ))
      <*> option auto
           (short 'j'
          <> help "Number of threads (0: automatic)"
          <> metavar "NUM"
          <> value 0
          <> showDefault
           )
      <*> switch (long "project-ghc-version"
                  <> help "Work out the project GHC version and print it")  

-- ---------------------------------------------------------------------
-- Set the GHC libdir to the nix libdir if it's present.
getGhcLibDir :: ComponentOptions -> IO FilePath
getGhcLibDir opts = do
  nixLibDir <- lookupEnv "NIX_GHC_LIBDIR"
  -- We want to avoid using ghc-paths, as it is not portable
  -- in the static binary sense - it just bakes in the path to the
  -- libraries at compile time! This is ok if the user built from
  -- source, but if they downloaoded a binary then this will return
  -- some path that doesn't exist on their computer.
  return $ fromMaybe GHC.Paths.libdir (nixLibDir <|> ghcLibDir opts)

ghcideVersion :: IO String
ghcideVersion = do
  path <- getExecutablePath
  let gitHashSection = case $(gitHash) of
        x | x == "UNKNOWN" -> ""
        x -> " (GIT hash: " <> x <> ")"
  return $ "ghcide version: " <> showVersion version
             <> " (GHC: " <> VERSION_ghc
             <> ") (PATH: " <> path <> ")"
             <> gitHashSection

-- ---------------------------------------------------------------------
