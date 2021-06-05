-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}

-- | Gives information about symbols at a given point in DAML files.
-- These are all pure functions that should execute quickly.
module Development.IDE.Spans.AtPoint (
    atPoint
  , gotoDefinition
  , gotoTypeDefinition
  , documentHighlight
  , pointCommand
  , referencesAtPoint
  , computeTypeReferences
  , FOIReferences(..)
  , defRowToSymbolInfo
  , rowToLoc
  , getNamesAtPoint
  , toCurrentLocation
  ) where

import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Orphans          ()
import           Development.IDE.Types.Location
import           Language.LSP.Types

-- compiler and infrastructure
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.Common
import           Development.IDE.Types.Options

-- GHC API imports
import           FastString                           (unpackFS)
import           IfaceType
import           Name
import           NameEnv
import           Outputable                           hiding ((<>))
import           SrcLoc
import           TyCoRep                              hiding (FunTy)
import           TyCon
import qualified Var

import           Control.Applicative
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Map.Strict                      as M
import           Data.Maybe
import qualified Data.Text                            as T

import qualified Data.Array                           as A
import           Data.Either
import           Data.List                            (isSuffixOf)
import           Data.List.Extra                      (dropEnd1, nubOrd)

import           HieDb                                hiding (pointCommand)

-- | Gives a Uri for the module, given the .hie file location and the the module info
-- The Bool denotes if it is a boot module
type LookupModule m = FilePath -> ModuleName -> UnitId -> Bool -> MaybeT m Uri

-- | HieFileResult for files of interest, along with the position mappings
newtype FOIReferences = FOIReferences (HM.HashMap NormalizedFilePath (HieAstResult, PositionMapping))

computeTypeReferences :: Foldable f => f (HieAST Type) -> M.Map Name [Span]
computeTypeReferences = foldr (\ast m -> M.unionWith (++) (go ast) m) M.empty
  where
    go ast = M.unionsWith (++) (this : map go (nodeChildren ast))
      where
        this = M.fromListWith (++)
          $ map (, [nodeSpan ast])
          $ concatMap namesInType
          $ mapMaybe (\x -> guard (not $ all isOccurrence $ identInfo x) *> identType x)
          $ M.elems
          $ nodeIdentifiers $ nodeInfo ast

-- | Given a file and position, return the names at a point, the references for
-- those names in the FOIs, and a list of file paths we already searched through
foiReferencesAtPoint
  :: NormalizedFilePath
  -> Position
  -> FOIReferences
  -> ([Name],[Location],[FilePath])
foiReferencesAtPoint file pos (FOIReferences asts) =
  case HM.lookup file asts of
    Nothing -> ([],[],[])
    Just (HAR _ hf _ _ _,mapping) ->
      let names = getNamesAtPoint hf pos mapping
          adjustedLocs = HM.foldr go [] asts
          go (HAR _ _ rf tr _, mapping) xs = refs ++ typerefs ++ xs
            where
              refs = mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation . fst)
                   $ concat $ mapMaybe (\n -> M.lookup (Right n) rf) names
              typerefs = mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation)
                   $ concat $ mapMaybe (`M.lookup` tr) names
        in (names, adjustedLocs,map fromNormalizedFilePath $ HM.keys asts)

getNamesAtPoint :: HieASTs a -> Position -> PositionMapping -> [Name]
getNamesAtPoint hf pos mapping =
  let posFile = fromMaybe pos $ fromCurrentPosition mapping pos
  in concat $ pointCommand hf posFile (rights . M.keys . nodeIdentifiers . nodeInfo)

toCurrentLocation :: PositionMapping -> Location -> Maybe Location
toCurrentLocation mapping (Location uri range) =
  Location uri <$> toCurrentRange mapping range

referencesAtPoint
  :: MonadIO m
  => HieDb
  -> NormalizedFilePath -- ^ The file the cursor is in
  -> Position -- ^ position in the file
  -> FOIReferences -- ^ references data for FOIs
  -> m [Location]
referencesAtPoint hiedb nfp pos refs = do
  -- The database doesn't have up2date references data for the FOIs so we must collect those
  -- from the Shake graph.
  let (names, foiRefs, exclude) = foiReferencesAtPoint nfp pos refs
  nonFOIRefs <- forM names $ \name ->
    case nameModule_maybe name of
      Nothing -> pure []
      Just mod -> do
         -- Look for references (strictly in project files, not dependencies),
         -- excluding the files in the FOIs (since those are in foiRefs)
         rows <- liftIO $ findReferences hiedb True (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod) exclude
         pure $ mapMaybe rowToLoc rows
  typeRefs <- forM names $ \name ->
    case nameModule_maybe name of
      Just mod | isTcClsNameSpace (occNameSpace $ nameOccName name) -> do
        refs <- liftIO $ findTypeRefs hiedb True (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod) exclude
        pure $ mapMaybe typeRowToLoc refs
      _ -> pure []
  pure $ nubOrd $ foiRefs ++ concat nonFOIRefs ++ concat typeRefs

rowToLoc :: Res RefRow -> Maybe Location
rowToLoc (row:.info) = flip Location range <$> mfile
  where
    range = Range start end
    start = Position (refSLine row - 1) (refSCol row -1)
    end = Position (refELine row - 1) (refECol row -1)
    mfile = case modInfoSrcFile info of
      Just f  -> Just $ toUri f
      Nothing -> Nothing

typeRowToLoc :: Res TypeRef -> Maybe Location
typeRowToLoc (row:.info) = do
  file <- modInfoSrcFile info
  pure $ Location (toUri file) range
  where
    range = Range start end
    start = Position (typeRefSLine row - 1) (typeRefSCol row -1)
    end = Position (typeRefELine row - 1) (typeRefECol row -1)

documentHighlight
  :: Monad m
  => HieASTs a
  -> RefMap a
  -> Position
  -> MaybeT m [DocumentHighlight]
documentHighlight hf rf pos = pure highlights
  where
    ns = concat $ pointCommand hf pos (rights . M.keys . nodeIdentifiers . nodeInfo)
    highlights = do
      n <- ns
      ref <- fromMaybe [] (M.lookup (Right n) rf)
      pure $ makeHighlight ref
    makeHighlight (sp,dets) =
      DocumentHighlight (realSrcSpanToRange sp) (Just $ highlightType $ identInfo dets)
    highlightType s =
      if any (isJust . getScopeFromContext) s
        then HkWrite
        else HkRead

gotoTypeDefinition
  :: MonadIO m
  => HieDb
  -> LookupModule m
  -> IdeOptions
  -> HieAstResult
  -> Position
  -> MaybeT m [Location]
gotoTypeDefinition hiedb lookupModule ideOpts srcSpans pos
  = lift $ typeLocationsAtPoint hiedb lookupModule ideOpts pos srcSpans

-- | Locate the definition of the name at a given position.
gotoDefinition
  :: MonadIO m
  => HieDb
  -> LookupModule m
  -> IdeOptions
  -> M.Map ModuleName NormalizedFilePath
  -> HieASTs a
  -> Position
  -> MaybeT m [Location]
gotoDefinition hiedb getHieFile ideOpts imports srcSpans pos
  = lift $ locationsAtPoint hiedb getHieFile ideOpts imports pos srcSpans

-- | Synopsis for the name at a given position.
atPoint
  :: IdeOptions
  -> HieAstResult
  -> DocAndKindMap
  -> Position
  -> Maybe (Maybe Range, [T.Text])
atPoint IdeOptions{} (HAR _ hf _ _ kind) (DKMap dm km) pos = listToMaybe $ pointCommand hf pos hoverInfo
  where
    -- Hover info for values/data
    hoverInfo ast = (Just range, prettyNames ++ pTypes)
      where
        pTypes
          | length names == 1 = dropEnd1 $ map wrapHaskell prettyTypes
          | otherwise = map wrapHaskell prettyTypes

        range = realSrcSpanToRange $ nodeSpan ast

        wrapHaskell x = "\n```haskell\n"<>x<>"\n```\n"
        info = nodeInfo ast
        names = M.assocs $ nodeIdentifiers info
        types = nodeType info

        prettyNames :: [T.Text]
        prettyNames = map prettyName names
        prettyName (Right n, dets) = T.unlines $
          wrapHaskell (showNameWithoutUniques n <> maybe "" (" :: " <>) ((prettyType <$> identType dets) <|> maybeKind))
          : definedAt n
          ++ catMaybes [ T.unlines . spanDocToMarkdown <$> lookupNameEnv dm n
                       ]
          where maybeKind = fmap showGhc $ safeTyThingType =<< lookupNameEnv km n
        prettyName (Left m,_) = showGhc m

        prettyTypes = map (("_ :: "<>) . prettyType) types
        prettyType t = case kind of
          HieFresh -> showGhc t
          HieFromDisk full_file -> showGhc $ hieTypeToIface $ recoverFullType t (hie_types full_file)

        definedAt name =
          -- do not show "at <no location info>" and similar messages
          -- see the code of 'pprNameDefnLoc' for more information
          case nameSrcLoc name of
            UnhelpfulLoc {} | isInternalName name || isSystemName name -> []
            _ -> ["*Defined " <> T.pack (showSDocUnsafe $ pprNameDefnLoc name) <> "*"]

typeLocationsAtPoint
  :: forall m
   . MonadIO m
  => HieDb
  -> LookupModule m
  -> IdeOptions
  -> Position
  -> HieAstResult
  -> m [Location]
typeLocationsAtPoint hiedb lookupModule _ideOptions pos (HAR _ ast _ _ hieKind) =
  case hieKind of
    HieFromDisk hf ->
      let arr = hie_types hf
          ts = concat $ pointCommand ast pos getts
          unfold = map (arr A.!)
          getts x = nodeType ni  ++ (mapMaybe identType $ M.elems $ nodeIdentifiers ni)
            where ni = nodeInfo x
          getTypes ts = flip concatMap (unfold ts) $ \case
            HTyVarTy n -> [n]
#if MIN_VERSION_ghc(8,8,0)
            HAppTy a (HieArgs xs) -> getTypes (a : map snd xs)
#else
            HAppTy a b -> getTypes [a,b]
#endif
            HTyConApp tc (HieArgs xs) -> ifaceTyConName tc : getTypes (map snd xs)
            HForAllTy _ a -> getTypes [a]
            HFunTy a b -> getTypes [a,b]
            HQualTy a b -> getTypes [a,b]
            HCastTy a -> getTypes [a]
            _ -> []
        in fmap nubOrd $ concatMapM (fmap (fromMaybe []) . nameToLocation hiedb lookupModule) (getTypes ts)
    HieFresh ->
      let ts = concat $ pointCommand ast pos getts
          getts x = nodeType ni  ++ (mapMaybe identType $ M.elems $ nodeIdentifiers ni)
            where ni = nodeInfo x
        in fmap nubOrd $ concatMapM (fmap (fromMaybe []) . nameToLocation hiedb lookupModule) (getTypes ts)

namesInType :: Type -> [Name]
namesInType (TyVarTy n)      = [Var.varName n]
namesInType (AppTy a b)      = getTypes [a,b]
namesInType (TyConApp tc ts) = tyConName tc : getTypes ts
namesInType (ForAllTy b t)   = Var.varName (binderVar b) : namesInType t
namesInType (FunTy a b)      = getTypes [a,b]
namesInType (CastTy t _)     = namesInType t
namesInType (LitTy _)        = []
namesInType _                = []

getTypes :: [Type] -> [Name]
getTypes ts = concatMap namesInType ts

locationsAtPoint
  :: forall m a
   . MonadIO m
  => HieDb
  -> LookupModule m
  -> IdeOptions
  -> M.Map ModuleName NormalizedFilePath
  -> Position
  -> HieASTs a
  -> m [Location]
locationsAtPoint hiedb lookupModule _ideOptions imports pos ast =
  let ns = concat $ pointCommand ast pos (M.keys . nodeIdentifiers . nodeInfo)
      zeroPos = Position 0 0
      zeroRange = Range zeroPos zeroPos
      modToLocation m = fmap (\fs -> pure $ Location (fromNormalizedUri $ filePathToUri' fs) zeroRange) $ M.lookup m imports
    in fmap (nubOrd . concat) $ mapMaybeM (either (pure . modToLocation) $ nameToLocation hiedb lookupModule) ns

-- | Given a 'Name' attempt to find the location where it is defined.
nameToLocation :: MonadIO m => HieDb -> LookupModule m -> Name -> m (Maybe [Location])
nameToLocation hiedb lookupModule name = runMaybeT $
  case nameSrcSpan name of
    sp@(RealSrcSpan rsp)
      -- Lookup in the db if we got a location in a boot file
      | not $ "boot" `isSuffixOf` unpackFS (srcSpanFile rsp) -> MaybeT $ pure $ fmap pure $ srcSpanToLocation sp
    sp -> do
      guard (sp /= wiredInSrcSpan)
      -- This case usually arises when the definition is in an external package.
      -- In this case the interface files contain garbage source spans
      -- so we instead read the .hie files to get useful source spans.
      mod <- MaybeT $ return $ nameModule_maybe name
      erow <- liftIO $ findDef hiedb (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod)
      case erow of
        [] -> do
          -- If the lookup failed, try again without specifying a unit-id.
          -- This is a hack to make find definition work better with ghcide's nascent multi-component support,
          -- where names from a component that has been indexed in a previous session but not loaded in this
          -- session may end up with different unit ids
          erow <- liftIO $ findDef hiedb (nameOccName name) (Just $ moduleName mod) Nothing
          case erow of
            [] -> MaybeT $ pure Nothing
            xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation lookupModule) xs
        xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation lookupModule) xs

defRowToLocation :: Monad m => LookupModule m -> Res DefRow -> MaybeT m Location
defRowToLocation lookupModule (row:.info) = do
  let start = Position (defSLine row - 1) (defSCol row - 1)
      end   = Position (defELine row - 1) (defECol row - 1)
      range = Range start end
  file <- case modInfoSrcFile info of
    Just src -> pure $ toUri src
    Nothing -> lookupModule (defSrc row) (modInfoName info) (modInfoUnit info) (modInfoIsBoot info)
  pure $ Location file range

toUri :: FilePath -> Uri
toUri = fromNormalizedUri . filePathToUri' . toNormalizedFilePath'

defRowToSymbolInfo :: Res DefRow -> Maybe SymbolInformation
defRowToSymbolInfo (DefRow{..}:.(modInfoSrcFile -> Just srcFile))
  = Just $ SymbolInformation (showGhc defNameOcc) kind Nothing Nothing loc Nothing
  where
    kind
      | isVarOcc defNameOcc = SkVariable
      | isDataOcc defNameOcc = SkConstructor
      | isTcOcc defNameOcc = SkStruct
      | otherwise = SkUnknown 1
    loc   = Location file range
    file  = fromNormalizedUri . filePathToUri' . toNormalizedFilePath' $ srcFile
    range = Range start end
    start = Position (defSLine - 1) (defSCol - 1)
    end   = Position (defELine - 1) (defECol - 1)
defRowToSymbolInfo _ = Nothing

pointCommand :: HieASTs t -> Position -> (HieAST t -> a) -> [a]
pointCommand hf pos k =
    catMaybes $ M.elems $ flip M.mapWithKey (getAsts hf) $ \fs ast ->
      case selectSmallestContaining (sp fs) ast of
        Nothing   -> Nothing
        Just ast' -> Just $ k ast'
 where
   sloc fs = mkRealSrcLoc fs (line+1) (cha+1)
   sp fs = mkRealSrcSpan (sloc fs) (sloc fs)
   line = _line pos
   cha = _character pos
