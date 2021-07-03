{-# LANGUAGE NoApplicativeDo #-}
{-# LANGUAGE TypeFamilies    #-}
module Development.IDE.Core.Actions
( getAtPoint
, getDefinition
, getTypeDefinition
, highlightAtPoint
, refsAtPoint
, useE
, useNoFileE
, usesE
, workspaceSymbols
) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict                  as HM
import           Data.Maybe
import qualified Data.Text                            as T
import           Data.Tuple.Extra
import           Development.IDE.Core.OfInterest
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat           hiding (TargetFile,
                                                       TargetModule,
                                                       parseModule,
                                                       typecheckModule,
                                                       writeHieFile)
import           Development.IDE.Graph
import qualified Development.IDE.Spans.AtPoint        as AtPoint
import           Development.IDE.Types.Location
import qualified HieDb
import           Language.LSP.Types                   (DocumentHighlight (..),
                                                       SymbolInformation (..))


-- | Eventually this will lookup/generate URIs for files in dependencies, but not in the
-- project. Right now, this is just a stub.
lookupMod
  :: HieDbWriter -- ^ access the database
  -> FilePath -- ^ The `.hie` file we got from the database
  -> ModuleName
  -> UnitId
  -> Bool -- ^ Is this file a boot file?
  -> MaybeT IdeAction Uri
lookupMod _dbchan _hie_f _mod _uid _boot = MaybeT $ pure Nothing


-- IMPORTANT NOTE : make sure all rules `useE`d by these have a "Persistent Stale" rule defined,
-- so we can quickly answer as soon as the IDE is opened
-- Even if we don't have persistent information on disk for these rules, the persistent rule
-- should just return an empty result
-- It is imperative that the result of the persistent rule succeed in such a case, or we will
-- block waiting for the rule to be properly computed.

-- | Try to get hover text for the name under point.
getAtPoint :: NormalizedFilePath -> Position -> IdeAction (Maybe (Maybe Range, [T.Text]))
getAtPoint file pos = runMaybeT $ do
  ide <- ask
  opts <- liftIO $ getIdeOptionsIO ide

  (hf, mapping) <- useE GetHieAst file
  dkMap <- lift $ maybe (DKMap mempty mempty) fst <$> (runMaybeT $ useE GetDocMap file)

  !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
  MaybeT $ pure $ fmap (first (toCurrentRange mapping =<<)) $ AtPoint.atPoint opts hf dkMap pos'

toCurrentLocations :: PositionMapping -> [Location] -> [Location]
toCurrentLocations mapping = mapMaybe go
  where
    go (Location uri range) = Location uri <$> toCurrentRange mapping range

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useE :: IdeRule k v => k -> NormalizedFilePath -> MaybeT IdeAction (v, PositionMapping)
useE k = MaybeT . useWithStaleFast k

useNoFileE :: IdeRule k v => IdeState -> k -> MaybeT IdeAction v
useNoFileE _ide k = fst <$> useE k emptyFilePath

usesE :: IdeRule k v => k -> [NormalizedFilePath] -> MaybeT IdeAction [(v,PositionMapping)]
usesE k = MaybeT . fmap sequence . mapM (useWithStaleFast k)

-- | Goto Definition.
getDefinition :: NormalizedFilePath -> Position -> IdeAction (Maybe [Location])
getDefinition file pos = runMaybeT $ do
    ide <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (hf, mapping) <- useE GetHieAst file
    (ImportMap imports, _) <- useE GetImportMap file
    !pos' <- MaybeT (pure $ fromCurrentPosition mapping pos)
    hiedb <- lift $ asks hiedb
    dbWriter <- lift $ asks hiedbWriter
    toCurrentLocations mapping <$> AtPoint.gotoDefinition hiedb (lookupMod dbWriter) opts imports hf pos'

getTypeDefinition :: NormalizedFilePath -> Position -> IdeAction (Maybe [Location])
getTypeDefinition file pos = runMaybeT $ do
    ide <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (hf, mapping) <- useE GetHieAst file
    !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
    hiedb <- lift $ asks hiedb
    dbWriter <- lift $ asks hiedbWriter
    toCurrentLocations mapping <$> AtPoint.gotoTypeDefinition hiedb (lookupMod dbWriter) opts hf pos'

highlightAtPoint :: NormalizedFilePath -> Position -> IdeAction (Maybe [DocumentHighlight])
highlightAtPoint file pos = runMaybeT $ do
    (HAR _ hf rf _ _,mapping) <- useE GetHieAst file
    !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
    let toCurrentHighlight (DocumentHighlight range t) = flip DocumentHighlight t <$> toCurrentRange mapping range
    mapMaybe toCurrentHighlight <$>AtPoint.documentHighlight hf rf pos'

-- Refs are not an IDE action, so it is OK to be slow and (more) accurate
refsAtPoint :: NormalizedFilePath -> Position -> Action [Location]
refsAtPoint file pos = do
    ShakeExtras{hiedb} <- getShakeExtras
    fs <- HM.keys <$> getFilesOfInterestUntracked
    asts <- HM.fromList . mapMaybe sequence . zip fs <$> usesWithStale GetHieAst fs
    AtPoint.referencesAtPoint hiedb file pos (AtPoint.FOIReferences asts)

workspaceSymbols :: T.Text -> IdeAction (Maybe [SymbolInformation])
workspaceSymbols query = runMaybeT $ do
  hiedb <- lift $ asks hiedb
  res <- liftIO $ HieDb.searchDef hiedb $ T.unpack query
  pure $ mapMaybe AtPoint.defRowToSymbolInfo res
