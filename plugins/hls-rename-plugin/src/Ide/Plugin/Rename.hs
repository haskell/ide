{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Ide.Plugin.Rename (descriptor) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Bifunctor
import           Data.HashMap.Internal                (fromList)
import qualified Data.HashMap.Strict                  as HM
import           Data.List.Extra
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           Debug.Trace
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint
import           HieDb
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           Name

type HiePosMap = HM.HashMap NormalizedFilePath (HieAstResult, PositionMapping)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
}

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state _pluginId (RenameParams tdi pos _progToken newName) = do
    edits <- liftIO $ renameEdits state tdi pos newName
    pure $ Right (WorkspaceEdit {
        _changes = Just (fromList edits),
        _documentChanges = Nothing,
        _changeAnnotations = Nothing
    })

renameEdits :: IdeState
    -> TextDocumentIdentifier
    -> Position
    -> T.Text
    -> IO [(Uri, List TextEdit)]
renameEdits state tdi pos newName = do
    List locs <- runNameRefs state $ ReferenceParams tdi pos Nothing Nothing (ReferenceContext False)
    pure $ map (Data.Bifunctor.second List)
        $ groupSort [(uri, TextEdit range newName) | Location uri range <- locs]

runNameRefs :: IdeState -> ReferenceParams -> IO (List Location)
runNameRefs ide (ReferenceParams (TextDocumentIdentifier uri) pos _ _ _) =
    case uriToFilePath' uri of
        Just fp -> List <$> runAction "Rename.references" ide (refsAtPoint (toNormalizedFilePath' fp) pos)
        Nothing -> pure $ List []

-- some code duplication with AtPoint could be removed
refsAtPoint :: NormalizedFilePath -> Position -> Action [Location]
refsAtPoint nfp pos = do
    ShakeExtras{hiedb} <- getShakeExtras
    fois <- HM.keys <$> getFilesOfInterest
    asts <- HM.fromList . mapMaybe sequence . zip fois <$> usesWithStale GetHieAst fois
    case nameAtPoint asts pos nfp of
        Nothing   -> pure []
        Just name -> refsAtName asts name hiedb

nameAtPoint :: HiePosMap -> Position -> NormalizedFilePath -> Maybe Name
nameAtPoint asts pos nfp = name =<< HM.lookup nfp asts
    where name (HAR _ ast _ _ _, mapping) = listToMaybe $ getNamesAtPoint ast pos mapping

-- remove HM.keys O(n) by passing foiRefs
refsAtName :: HiePosMap -> Name -> HieDb -> Action [Location]
refsAtName asts name hiedb = do
    let foiRefs = concat $ mapMaybe (getNameAstLocations name) (HM.elems asts)
    refs <- nameDbRefs (HM.keys asts) name hiedb
    pure $ nubOrd $ map (updateLocLength (length $ getOccString name)) $ foiRefs ++ refs

nameDbRefs :: [NormalizedFilePath] -> Name -> HieDb -> Action [Location]
nameDbRefs fois name hiedb =
    case nameModule_maybe name of
        Nothing -> pure []
        Just mod -> do
            let exclude = map fromNormalizedFilePath fois
            rows <- liftIO $ findReferences hiedb True (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod) exclude
            pure $ mapMaybe rowToLoc rows

getNameAstLocations :: Name -> (HieAstResult, PositionMapping) -> Maybe [Location]
getNameAstLocations name (HAR _ _ rm _ _, mapping) =
    mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation . fst) <$> M.lookup (Right name) rm

-- sets location length to n (keeping rightmost position the same)
-- Useful to drop module ID prefix from qualified name
updateLocLength :: Int -> Location -> Location
updateLocLength n (Location uri (Range (Position ln _) end@(Position _ col))) =
    Location uri (Range (Position ln (col - n)) end)
