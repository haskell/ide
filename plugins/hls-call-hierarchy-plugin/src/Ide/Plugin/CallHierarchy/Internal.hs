{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Ide.Plugin.CallHierarchy.Internal (
  prepareCallHierarchy
, incomingCalls
, outgoingCalls
) where

import           Control.Lens                   ((^.))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Data.Aeson                     as A
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (groupBy, sortBy)
import qualified Data.Map                       as M
import           Data.Maybe
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import           Development.IDE
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint
import           HieDb                          (Symbol (Symbol))
import qualified Ide.Plugin.CallHierarchy.Query as Q
import           Ide.Plugin.CallHierarchy.Types
import           Ide.Types
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens        as L
import           Name
import           Text.Read                      (readMaybe)

prepareCallHierarchy :: PluginMethodHandler IdeState TextDocumentPrepareCallHierarchy
prepareCallHierarchy state pluginId param
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri =
    liftIO (runAction "CallHierarchy.prepareHierarchy" state (prepareCallHierarchyItem nfp pos)) >>=
      \case
        Just items -> pure $ Right $ Just $ List items
        Nothing    -> pure $ Left $ responseError "Call Hierarchy: No result"
  | otherwise = pure $ Left $ responseError $ T.pack $ "Call Hierarchy: uriToNormalizedFilePath failed for: " <> show uri
  where
    uri = param ^. (L.textDocument . L.uri)
    pos = param ^. L.position

prepareCallHierarchyItem :: NormalizedFilePath -> Position -> Action (Maybe [CallHierarchyItem])
prepareCallHierarchyItem = constructFromAst

constructFromAst :: NormalizedFilePath -> Position -> Action (Maybe [CallHierarchyItem])
constructFromAst nfp pos =
  use GetHieAst nfp >>=
    \case
      Nothing -> pure Nothing
      Just (HAR _ hf _ _ _) -> do
        case listToMaybe $ pointCommand hf pos extract of
          Just res -> pure $ Just $ mapMaybe (construct nfp) res
          Nothing  -> pure Nothing

extract :: HieAST a -> [(Identifier, S.Set ContextInfo, Span)]
extract ast = let span = nodeSpan ast
                  infos = M.toList $ M.map identInfo (nodeIdentifiers $ nodeInfo ast)
              in  [ (ident, contexts, span) | (ident, contexts) <- infos ]

recFieldInfo, declInfo, valBindInfo, classTyDeclInfo,
  useInfo, patternBindInfo :: S.Set ContextInfo -> Maybe ContextInfo
recFieldInfo    ctxs = listToMaybe [ctx | ctx@RecField{}    <- S.toList ctxs]
declInfo        ctxs = listToMaybe [ctx | ctx@Decl{}        <- S.toList ctxs]
valBindInfo     ctxs = listToMaybe [ctx | ctx@ValBind{}     <- S.toList ctxs]
classTyDeclInfo ctxs = listToMaybe [ctx | ctx@ClassTyDecl{} <- S.toList ctxs]
useInfo         ctxs = listToMaybe [Use | Use               <- S.toList ctxs]
patternBindInfo ctxs = listToMaybe [ctx | ctx@PatternBind{} <- S.toList ctxs]

construct :: NormalizedFilePath -> (Identifier, S.Set ContextInfo, Span) -> Maybe CallHierarchyItem
construct nfp (ident, contexts, ssp)
  | isInternalIdentifier ident = Nothing

  | Just (RecField RecFieldDecl _) <- recFieldInfo contexts
    -- ignored type span
    = Just $ mkCallHierarchyItem' ident SkField ssp ssp

  | Just ctx <- valBindInfo contexts
    = Just $ case ctx of
        ValBind _ _ span -> mkCallHierarchyItem' ident SkFunction (renderSpan span) ssp
        _                -> mkCallHierarchyItem' ident skUnknown ssp ssp

  | Just ctx <- declInfo contexts
    = Just $ case ctx of
        Decl ClassDec span -> mkCallHierarchyItem' ident SkInterface (renderSpan span) ssp
        Decl ConDec   span -> mkCallHierarchyItem' ident SkConstructor (renderSpan span) ssp
        Decl DataDec  span -> mkCallHierarchyItem' ident SkStruct (renderSpan span) ssp
        Decl FamDec   span -> mkCallHierarchyItem' ident SkFunction (renderSpan span) ssp
        Decl InstDec  span -> mkCallHierarchyItem' ident SkInterface (renderSpan span) ssp
        Decl SynDec   span -> mkCallHierarchyItem' ident SkTypeParameter (renderSpan span) ssp
        _ -> mkCallHierarchyItem' ident skUnknown ssp ssp

  | Just (ClassTyDecl span) <- classTyDeclInfo contexts
    = Just $ mkCallHierarchyItem' ident SkMethod (renderSpan span) ssp

  | Just (PatternBind _ _ span) <- patternBindInfo contexts
    = Just $ mkCallHierarchyItem' ident SkFunction (renderSpan span) ssp

  | Just Use <- useInfo contexts
    = Just $ mkCallHierarchyItem' ident SkInterface ssp ssp

  | otherwise = Nothing
  where
    renderSpan = \case Just span -> span
                       _         -> ssp

    skUnknown = SkUnknown 27

    mkCallHierarchyItem' = mkCallHierarchyItem nfp

    isInternalIdentifier = \case
      Left _     -> False
      Right name -> isInternalName name

mkCallHierarchyItem :: NormalizedFilePath -> Identifier -> SymbolKind -> Span -> Span -> CallHierarchyItem
mkCallHierarchyItem nfp ident kind span selSpan =
  CallHierarchyItem
    (T.pack $ identifierName ident)
    kind
    Nothing
    (Just $ T.pack $ identifierToDetail ident)
    (fromNormalizedUri $ normalizedFilePathToUri nfp)
    (realSrcSpanToRange span)
    (realSrcSpanToRange selSpan)
    (toJSON . show <$> mkSymbol ident)
  where
    identifierToDetail :: Identifier -> String
    identifierToDetail = \case
      Left modName -> moduleNameString modName
      Right name   -> (moduleNameString . moduleName . nameModule) name

    identifierName :: Identifier -> String
    identifierName = \case
      Left modName -> moduleNameString modName
      Right name   -> occNameString $ nameOccName name

mkSymbol :: Identifier -> Maybe Symbol
mkSymbol = \case
  Left _     -> Nothing
  Right name -> Just $ Symbol (occName name) (nameModule name)


-- Incoming calls and outgoing calls

deriving instance Ord SymbolKind
deriving instance Ord SymbolTag
deriving instance Ord CallHierarchyItem

incomingCalls :: PluginMethodHandler IdeState CallHierarchyIncomingCalls
incomingCalls state pluginId param = do
  liftIO $ runAction "CallHierarchy.incomingCalls" state $
      queryCalls (param ^. L.item) Q.incomingCalls mkCallHierarchyIncomingCall
        foiIncomingCalls mergeIncomingCalls >>=
    \case
      Just x  -> pure $ Right $ Just $ List x
      Nothing -> pure $ Left $ responseError "CallHierarchy: IncomingCalls internal error"
  where
    mkCallHierarchyIncomingCall :: Vertex -> Action (Maybe CallHierarchyIncomingCall)
    mkCallHierarchyIncomingCall = mkCallHierarchyCall CallHierarchyIncomingCall

    mergeIncomingCalls :: [CallHierarchyIncomingCall] -> [CallHierarchyIncomingCall]
    mergeIncomingCalls = map merge
                       . groupBy (\a b -> a ^. L.from == b ^. L.from)
                       . sortBy (\a b -> (a ^. L.from) `compare` (b ^. L.from))
      where
        merge calls = let ranges = concatMap ((\(List x) -> x) . (^. L.fromRanges)) calls
                      in  CallHierarchyIncomingCall (head calls ^. L.from) (List ranges)

outgoingCalls :: PluginMethodHandler IdeState CallHierarchyOutgoingCalls
outgoingCalls state pluginId param = do
  liftIO $ runAction "CallHierarchy.outgoingCalls" state $
      queryCalls (param ^. L.item) Q.outgoingCalls mkCallHierarchyOutgoingCall
                 foiOutgoingCalls mergeOutgoingCalls >>=
    \case
      Just x  -> pure $ Right $ Just $ List x
      Nothing -> pure $ Left $ responseError "CallHierarchy: OutgoingCalls internal error"
  where
    mkCallHierarchyOutgoingCall :: Vertex -> Action (Maybe CallHierarchyOutgoingCall)
    mkCallHierarchyOutgoingCall = mkCallHierarchyCall CallHierarchyOutgoingCall

    mergeOutgoingCalls :: [CallHierarchyOutgoingCall] -> [CallHierarchyOutgoingCall]
    mergeOutgoingCalls = map merge
                       . groupBy (\a b -> a ^. L.to == b ^. L.to)
                       . sortBy (\a b -> (a ^. L.to) `compare` (b ^. L.to))
      where
        merge calls = let ranges = concatMap ((\(List x) -> x) . (^. L.fromRanges)) calls
                      in  CallHierarchyOutgoingCall (head calls ^. L.to) (List ranges)

mkCallHierarchyCall :: (CallHierarchyItem -> List Range -> a) -> Vertex -> Action (Maybe a)
mkCallHierarchyCall mk Vertex{..} = do
  let pos = Position (sl - 1) (sc - 1)
      nfp = toNormalizedFilePath' hieSrc
      range = mkRange (casl - 1) (casc - 1) (cael - 1) (caec - 1)
  items <- prepareCallHierarchyItem nfp pos
  case items of
    Just [item] -> pure $ Just $ mk item (List [range])
    _           -> pure Nothing

-- Unified queries include incoming calls and outgoing calls.
queryCalls :: (Show a)
  => CallHierarchyItem
  -> (HieDb -> Symbol -> IO [Vertex])
  -> (Vertex -> Action (Maybe a))
  -> (NormalizedFilePath -> Position -> Action (Maybe [a]))
  -> ([a] -> [a])
  -> Action (Maybe [a])
queryCalls item queryFunc makeFunc foiCalls merge
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
    ShakeExtras{hiedb} <- getShakeExtras
    maySymbol <- getSymbol nfp
    case maySymbol of
      Nothing -> error "CallHierarchy.Impossible"
      Just symbol -> do
        vs <- liftIO $ queryFunc hiedb symbol
        nonFOIItems <- mapM makeFunc vs
        foiRes <- foiCalls nfp pos
        if isJust foiRes && (length <$> foiRes) /= Just 0 then liftIO $ putStrLn (show "fois:" <> show foiRes) else pure ()
        let nonFOIRes = Just $ catMaybes nonFOIItems
        pure $ merge <$> (nonFOIRes <> foiRes)
  | otherwise = pure Nothing
  where
    uri = item ^. L.uri
    xdata = item ^. L.xdata
    pos = item ^. (L.selectionRange . L.start)

    getSymbol nfp =
      case item ^. L.xdata of
        Just xdata -> case fromJSON xdata of
          A.Success (symbolStr :: String) ->
            case readMaybe symbolStr of
              Just symbol -> pure $ Just symbol
              Nothing     -> getSymbolFromAst nfp pos
          A.Error _ -> getSymbolFromAst nfp pos
        Nothing -> getSymbolFromAst nfp pos

    getSymbolFromAst :: NormalizedFilePath -> Position -> Action (Maybe Symbol)
    getSymbolFromAst nfp pos =
      use GetHieAst nfp >>=
        \case
          Nothing -> pure Nothing
          Just (HAR _ hf _ _ _) -> do
            case listToMaybe $ pointCommand hf pos extract of
              Just infos -> case (\(ident, _, _) -> mkSymbol ident) <$> listToMaybe infos of
                Nothing  -> pure Nothing
                Just res -> pure res
              Nothing -> pure Nothing

foiIncomingCalls :: NormalizedFilePath -> Position -> Action (Maybe [CallHierarchyIncomingCall])
foiIncomingCalls nfp pos =
  use GetHieAst nfp >>=
    \case
      Nothing -> pure Nothing
      Just (HAR _ hf _ _ _) -> do
        case listToMaybe $ pointCommand hf pos id of
          Nothing -> pure Nothing
          Just ast -> do
            fs <- HM.keys <$> getFilesOfInterestUntracked
            Just . concatMap (`callers` ast) <$> mapMaybeM (use GetHieAst) fs
  where
    callers :: HieAstResult -> HieAST a -> [CallHierarchyIncomingCall]
    callers (HAR _ hf _ _ _) ast = mkIncomingCalls $ filter (sameAst ast) $ M.elems (getAsts hf)

    sameAst :: HieAST a -> HieAST b -> Bool
    sameAst ast1 ast2 = (M.keys .nodeIdentifiers . nodeInfo) ast1
                     == (M.keys .nodeIdentifiers . nodeInfo) ast2

    mkIncomingCalls asts = let infos = concatMap extract asts
                               items = mapMaybe (construct nfp) infos
                           in  map (\item ->
                                      CallHierarchyIncomingCall item
                                        (List [item ^. L.selectionRange])) items

foiOutgoingCalls :: NormalizedFilePath -> Position -> Action (Maybe [CallHierarchyOutgoingCall])
foiOutgoingCalls nfp pos =
  use GetHieAst nfp >>=
    \case
      Nothing -> pure Nothing
      Just (HAR _ hf _ _ _) -> do
        case listToMaybe $ pointCommand hf pos nodeChildren of
          Nothing       -> pure Nothing
          Just children -> pure $ Just $ mkOutgoingCalls children
  where
    mkOutgoingCalls asts = let infos = concatMap extract asts
                               items = mapMaybe (construct nfp) infos
                           in  map (\item ->
                                      CallHierarchyOutgoingCall item
                                        (List [item ^. L.selectionRange]) ) items
