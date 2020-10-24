{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Tactic.Metaprogramming where

import           Control.Monad.Extra (unlessM)
import qualified Data.Map as M
import qualified Data.Text as T
import           Ide.Plugin.Tactic.Types
import           Language.Haskell.LSP.Types
import           System.Directory
import           System.FilePath ((</>))

import qualified Text.Megaparsec as P

import           Ide.Plugin.Tactic.Parser
import Text.Megaparsec (errorBundlePretty)


buildCache :: [Metaprogram] -> MetaprogramCache
buildCache mps = MetaprogramCache $ M.fromList $ do
  mp <- mps
  pure (mp_name mp, mp)


configDir :: IO FilePath
configDir = do
  dir <- getXdgDirectory XdgConfig "hls-tactics-plugin"
  unlessM (doesDirectoryExist dir) $ createDirectory dir
  pure dir


getKnownFiles :: IO [NormalizedFilePath]
getKnownFiles = do
  dir    <- configDir
  files  <- fmap (dir </>) <$> listDirectory dir
  pure $ fmap toNormalizedFilePath files


parseMetaprogram :: FilePath -> T.Text -> Either String Metaprogram
parseMetaprogram fp str = case P.runParser metaprogram fp str of
  Left err -> trace (errorBundlePretty err) $ Left $ show err
  Right tac -> Right tac

