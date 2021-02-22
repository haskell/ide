-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
#include "ghc-api-version.h"

-- | Go to the definition of a variable.
module Development.IDE.Plugin.CodeAction
    ( descriptor

    -- * For testing
    , matchRegExMultipleImports
    ) where

import Control.Monad (join, guard)
import Control.Monad.IO.Class
import Development.IDE.GHC.Compat
import Development.IDE.Core.Rules
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.GHC.Error
import Development.IDE.GHC.ExactPrint
import Development.IDE.Plugin.CodeAction.ExactPrint
import Development.IDE.Plugin.CodeAction.PositionIndexed
import Development.IDE.Plugin.TypeLenses (suggestSignature)
import Development.IDE.Types.Exports
import Development.IDE.Types.HscEnvEq
import Development.IDE.Types.Location
import Development.IDE.Types.Options
import qualified Data.HashMap.Strict as Map
import qualified Language.LSP.Server as LSP
import Language.LSP.VFS
import Language.LSP.Types
import qualified Data.Rope.UTF16 as Rope
import Data.Char
import Data.Maybe
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Text.Regex.TDFA (mrAfter, (=~), (=~~))
import Outputable (Outputable, ppr, showSDoc, showSDocUnsafe)
import Data.Function
import Control.Arrow ((>>>), second)
import Data.Functor
import Control.Applicative ((<|>))
import Safe (atMay)
import Bag (isEmptyBag)
import qualified Data.HashSet as Set
import Control.Concurrent.Extra (readVar)
import Development.IDE.GHC.Util (printRdrName, prettyPrint)
import Ide.PluginUtils (subRange)
import Ide.Types
import qualified Data.DList as DL
import Development.IDE.Spans.Common
import OccName
import qualified GHC.LanguageExtensions as Lang
import Control.Lens (alaf)
import Data.Monoid (Ap(..))
import TcRnTypes (TcGblEnv(..), ImportAvails(..))
import HscTypes (ImportedModsVal(..), importedByUser)
import RdrName (GlobalRdrElt(..), lookupGlobalRdrEnv, mkVarUnqual)
import SrcLoc (realSrcSpanStart)
import Module (moduleEnvElts)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics (Data, something, mkQ, everywhere, mkT)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginRules = mempty,
      pluginHandlers = mkPluginHandler STextDocumentCodeAction codeAction
    }

-- | Generate code actions.
codeAction
    :: IdeState
    -> PluginId
    -> CodeActionParams
    -> LSP.LspM c (Either ResponseError (List (Command |? CodeAction)))
codeAction state _ (CodeActionParams _ _ (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List xs}) = do
  contents <- LSP.getVirtualFile $ toNormalizedUri uri
  liftIO $ do
    let text = Rope.toText . (_text :: VirtualFile -> Rope.Rope) <$> contents
        mbFile = toNormalizedFilePath' <$> uriToFilePath uri
    diag <- fmap (\(_, _, d) -> d) . filter (\(p, _, _) -> mbFile == Just p) <$> getDiagnostics state
    (ideOptions, join -> parsedModule, join -> env, join -> annotatedPS, join -> tcM, join -> har) <- runAction "CodeAction" state $
      (,,,,,) <$> getIdeOptions
            <*> getParsedModule `traverse` mbFile
            <*> use GhcSession `traverse` mbFile
            <*> use GetAnnotatedParsedSource `traverse` mbFile
            <*> use TypeCheck `traverse` mbFile
            <*> use GetHieAst `traverse` mbFile
    -- This is quite expensive 0.6-0.7s on GHC
    pkgExports   <- maybe mempty envPackageExports env
    localExports <- readVar (exportsMap $ shakeExtras state)
    let
      exportsMap = localExports <> pkgExports
      df = ms_hspp_opts . pm_mod_summary <$> parsedModule
      actions =
        [ mkCA title  [x] edit
        | x <- xs, (title, tedit) <- suggestAction exportsMap ideOptions parsedModule text df annotatedPS tcM har x
        , let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
        ]
      actions' = caRemoveRedundantImports parsedModule text diag xs uri
               <> actions
               <> caRemoveInvalidExports parsedModule text diag xs uri
    pure $ Right $ List actions'

mkCA :: T.Text -> [Diagnostic] -> WorkspaceEdit -> (Command |? CodeAction)
mkCA title diags edit =
  InR $ CodeAction title (Just CodeActionQuickFix) (Just $ List diags) Nothing Nothing (Just edit) Nothing

rewrite ::
    Maybe DynFlags ->
    Maybe (Annotated ParsedSource) ->
    (DynFlags -> ParsedSource -> [(T.Text, [Rewrite])]) ->
    [(T.Text, [TextEdit])]
rewrite (Just df) (Just ps) f
    | Right edit <- (traverse . traverse)
        (alaf Ap foldMap (rewriteToEdit df (annsA ps)))
        (f df $ astA ps) = edit
rewrite _ _ _ = []

suggestAction
  :: ExportsMap
  -> IdeOptions
  -> Maybe ParsedModule
  -> Maybe T.Text
  -> Maybe DynFlags
  -> Maybe (Annotated ParsedSource)
  -> Maybe TcModuleResult
  -> Maybe HieAstResult
  -> Diagnostic
  -> [(T.Text, [TextEdit])]
suggestAction packageExports ideOptions parsedModule text df annSource tcM har diag =
    concat
   -- Order these suggestions by priority
    [ suggestSignature True diag
    , rewrite df annSource $ \_ ps -> suggestExtendImport packageExports ps diag
    , rewrite df annSource $ \df ps ->
        suggestImportDisambiguation df text ps diag
    , suggestFillTypeWildcard diag
    , suggestFixConstructorImport text diag
    , suggestModuleTypo diag
    , suggestReplaceIdentifier text diag
    , removeRedundantConstraints text diag
    , suggestAddTypeAnnotationToSatisfyContraints text diag
    , rewrite df annSource $ \df ps -> suggestConstraint df ps diag
    , rewrite df annSource $ \_ ps -> suggestImplicitParameter ps diag
    , rewrite df annSource $ \_ ps -> suggestHideShadow ps tcM har diag
    , rewrite df annSource $ \_ ps -> suggestRenameShadow ps tcM har diag
    ] ++ concat
    [  suggestNewDefinition ideOptions pm text diag
    ++ suggestNewImport packageExports pm diag
    ++ suggestDeleteUnusedBinding pm text diag
    ++ suggestExportUnusedTopBinding text pm diag
    ++ suggestDisableWarning pm text diag
    | Just pm <- [parsedModule]
    ] ++
    suggestFillHole diag                   -- Lowest priority

findSigOfDecl :: (IdP p -> Bool) -> [LHsDecl p] -> Maybe (Sig p)
findSigOfDecl pred decls =
  listToMaybe
    [ sig
      | L _ (SigD _ sig@(TypeSig _ idsSig _)) <- decls,
        any (pred . unLoc) idsSig
    ]

findInstanceHead :: (Outputable (HsType p)) => DynFlags -> String -> [LHsDecl p] -> Maybe (LHsType p)
findInstanceHead df instanceHead decls =
  listToMaybe
    [ hsib_body
      | L _ (InstD _ (ClsInstD _ ClsInstDecl {cid_poly_ty = HsIB {hsib_body}})) <- decls,
        showSDoc df (ppr hsib_body) == instanceHead
    ]

findDeclContainingLoc :: Position -> [Located a] -> Maybe (Located a)
findDeclContainingLoc loc = find (\(L l _) -> loc `isInsideSrcSpan` l)

-- Single:
-- This binding for ‘mod’ shadows the existing binding
--   imported from ‘Prelude’ at haskell-language-server/ghcide/src/Development/IDE/Plugin/CodeAction.hs:10:8-40
--   (and originally defined in ‘GHC.Real’)typecheck(-Wname-shadowing)
-- Multi:
--This binding for ‘pack’ shadows the existing bindings
--  imported from ‘Data.ByteString’ at B.hs:6:1-22
--  imported from ‘Data.ByteString.Lazy’ at B.hs:8:1-27
--  imported from ‘Data.Text’ at B.hs:7:1-16
suggestHideShadow :: ParsedSource -> Maybe TcModuleResult -> Maybe HieAstResult -> Diagnostic -> [(T.Text, [Rewrite])]
suggestHideShadow pm@(L _ HsModule {hsmodImports}) mTcM mHar Diagnostic {_message, _range}
  | Just [identifier, modName, s] <-
      matchRegexUnifySpaces
        _message
        "This binding for ‘([^`]+)’ shadows the existing binding imported from ‘([^`]+)’ at ([^ ]*)" =
    suggests identifier modName s
  | Just [identifier] <-
      matchRegexUnifySpaces
        _message
        "This binding for ‘([^`]+)’ shadows the existing bindings",
    Just matched <- allMatchRegexUnifySpaces _message "imported from ‘([^’]+)’ at ([^ ]*)",
    mods <- [(modName, s) | [_, modName, s] <- matched],
    result <- nubOrdBy (compare `on` fst) $ mods >>= uncurry (suggests identifier),
    hideAll <- ("Hide " <> identifier <> " from all occurence imports", concat $ snd <$> result) =
    result <> [hideAll]
  | otherwise = []
  where
    suggests identifier modName s
      | Just tcM <- mTcM,
        Just har <- mHar,
        [s'] <- [x | (x, "") <- readSrcSpan $ T.unpack s],
        isUnusedImportedId tcM har (T.unpack identifier) (T.unpack modName) (RealSrcSpan s'),
        mDecl <- findImportDeclByModuleName hsmodImports $ T.unpack modName,
        title <- "Hide " <> identifier <> " from " <> modName =
        if modName == "Prelude" && null mDecl
          then [(title, maybeToList $ hideImplicitPreludeSymbol (T.unpack identifier) pm)]
          else maybeToList $ (title,) . pure . hideSymbol (T.unpack identifier) <$> mDecl
      | otherwise = []

findImportDeclByModuleName :: [LImportDecl GhcPs] -> String -> Maybe (LImportDecl GhcPs)
findImportDeclByModuleName decls modName = flip find decls $ \case
  (L _ ImportDecl {..}) -> modName == moduleNameString (unLoc ideclName)
  _ -> error "impossible"

isTheSameLine :: SrcSpan -> SrcSpan -> Bool
isTheSameLine s1 s2
  | Just sl1 <- getStartLine s1,
    Just sl2 <- getStartLine s2 =
    sl1 == sl2
  | otherwise = False
  where
    getStartLine x = srcLocLine . realSrcSpanStart <$> realSpan x

isUnusedImportedId :: TcModuleResult -> HieAstResult -> String -> String -> SrcSpan -> Bool
isUnusedImportedId
  TcModuleResult {tmrTypechecked = TcGblEnv {tcg_imports = ImportAvails {imp_mods}}}
  HAR {refMap}
  identifier
  modName
  importSpan
    | occ <- mkVarOcc identifier,
      impModsVals <- importedByUser . concat $ moduleEnvElts imp_mods,
      Just rdrEnv <-
        listToMaybe
          [ imv_all_exports
            | ImportedModsVal {..} <- impModsVals,
              imv_name == mkModuleName modName,
              isTheSameLine imv_span importSpan
          ],
      [GRE {..}] <- lookupGlobalRdrEnv rdrEnv occ,
      importedIdentifier <- Right gre_name,
      refs <- M.lookup importedIdentifier refMap =
      maybe True (not . any (\(_, IdentifierDetails {..}) -> identInfo == S.singleton Use)) refs
    | otherwise = False

suggestRenameShadow :: ParsedSource -> Maybe TcModuleResult -> Maybe HieAstResult -> Diagnostic -> [(T.Text, [Rewrite])]
suggestRenameShadow (L _ HsModule {hsmodDecls}) mTcM mHar Diagnostic {_message, _range}
  | not $ T.isInfixOf "import" _message,
    Just [identifier] <-
      matchRegexUnifySpaces
        _message
        "This binding for ‘([^`]+)’ shadows the existing binding",
    Range p@(Position (succ -> line) (succ -> col)) _ <- _range,
    Just TcModuleResult {tmrRenamed = (hsGroup, _, _, _)} <- mTcM,
    Just HAR {refMap} <- mHar,
    Just name <- getIdByLoc (line, col) hsGroup,
    Just refSpans <- fmap fst <$> M.lookup (Right name) refMap,
    Just decl <- findDeclContainingLoc p hsmodDecls =
    [("Rename " <> identifier, pure $ Rewrite (getLoc decl) $ \_ -> pure $ renameRdr (mkVarUnqual "NEW_NAME") refSpans decl)]
  | otherwise = []

getIdByLoc :: (Data (f pass), Data (IdP pass)) => (Int, Int) -> f pass -> Maybe (IdP pass)
getIdByLoc (line, col) = something (mkQ Nothing f)
  where
    f :: Located a -> Maybe a
    f (L l name)
      | RealSrcSpan s <- l,
        srcSpanStartLine s == line,
        srcSpanStartCol s == col =
        Just name
      | otherwise = Nothing

renameRdr :: RdrName -> [Span] -> LHsDecl GhcPs -> LHsDecl GhcPs
renameRdr newName targets = everywhere (mkT f)
  where
    f :: Located RdrName -> Located RdrName
    f x@(L l _)
      | RealSrcSpan s <- l,
        s `elem` targets =
        L l newName
      | otherwise = x

suggestDisableWarning :: ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestDisableWarning pm contents Diagnostic{..}
    | Just (InR (T.stripPrefix "-W" -> Just w)) <- _code =
        pure
            ( "Disable \"" <> w <> "\" warnings"
            , [TextEdit (endOfModuleHeader pm contents) $ "{-# OPTIONS_GHC -Wno-" <> w <> " #-}\n"]
            )
    | otherwise = []

suggestRemoveRedundantImport :: ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestRemoveRedundantImport ParsedModule{pm_parsed_source = L _  HsModule{hsmodImports}} contents Diagnostic{_range=_range,..}
--     The qualified import of ‘many’ from module ‘Control.Applicative’ is redundant
    | Just [_, bindings] <- matchRegexUnifySpaces _message "The( qualified)? import of ‘([^’]*)’ from module [^ ]* is redundant"
    , Just (L _ impDecl) <- find (\(L l _) -> srcSpanToRange l == Just _range ) hsmodImports
    , Just c <- contents
    , ranges <- map (rangesForBindingImport impDecl . T.unpack) (T.splitOn ", " bindings)
    , ranges' <- extendAllToIncludeCommaIfPossible False (indexedByPosition $ T.unpack c) (concat ranges)
    , not (null ranges')
    = [( "Remove " <> bindings <> " from import" , [ TextEdit r "" | r <- ranges' ] )]

-- File.hs:16:1: warning:
--     The import of `Data.List' is redundant
--       except perhaps to import instances from `Data.List'
--     To import instances alone, use: import Data.List()
    | _message =~ ("The( qualified)? import of [^ ]* is redundant" :: String)
        = [("Remove import", [TextEdit (extendToWholeLineIfPossible contents _range) ""])]
    | otherwise = []

caRemoveRedundantImports :: Maybe ParsedModule -> Maybe T.Text -> [Diagnostic] -> [Diagnostic] -> Uri -> [Command |? CodeAction]
caRemoveRedundantImports m contents digs ctxDigs uri
  | Just pm <- m,
    r <- join $ map (\d -> repeat d `zip` suggestRemoveRedundantImport pm contents d) digs,
    allEdits <- [ e | (_, (_, edits)) <- r, e <- edits],
    caRemoveAll <- removeAll allEdits,
    ctxEdits <- [ x | x@(d, _) <- r, d `elem` ctxDigs],
    not $ null ctxEdits,
    caRemoveCtx <- map (\(d, (title, tedit)) -> removeSingle title tedit d) ctxEdits
      = caRemoveCtx ++ [caRemoveAll]
  | otherwise = []
  where
    removeSingle title tedit diagnostic = mkCA title [diagnostic] WorkspaceEdit{..} where
        _changes = Just $ Map.singleton uri $ List tedit
        _documentChanges = Nothing
    removeAll tedit = InR $ CodeAction{..} where
        _changes = Just $ Map.singleton uri $ List tedit
        _title = "Remove all redundant imports"
        _kind = Just CodeActionQuickFix
        _diagnostics = Nothing
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        _isPreferred = Nothing
        _command = Nothing
        _disabled = Nothing

caRemoveInvalidExports :: Maybe ParsedModule -> Maybe T.Text -> [Diagnostic] -> [Diagnostic] -> Uri -> [Command |? CodeAction]
caRemoveInvalidExports m contents digs ctxDigs uri
  | Just pm <- m,
    Just txt <- contents,
    txt' <- indexedByPosition $ T.unpack txt,
    r <- mapMaybe (groupDiag pm) digs,
    r' <- map (\(t,d,rs) -> (t,d,extend txt' rs)) r,
    caRemoveCtx <- mapMaybe removeSingle r',
    allRanges <- nubOrd $ [ range | (_,_,ranges) <- r, range <- ranges],
    allRanges' <- extend txt' allRanges,
    Just caRemoveAll <- removeAll allRanges',
    ctxEdits <- [ x | x@(_, d, _) <- r, d `elem` ctxDigs],
    not $ null ctxEdits
      = caRemoveCtx ++ [caRemoveAll]
  | otherwise = []
  where
    extend txt ranges = extendAllToIncludeCommaIfPossible True txt ranges

    groupDiag pm dig
      | Just (title, ranges) <- suggestRemoveRedundantExport pm dig
      = Just (title, dig, ranges)
      | otherwise = Nothing

    removeSingle (_, _, []) = Nothing
    removeSingle (title, diagnostic, ranges) = Just $ InR $ CodeAction{..} where
        tedit = concatMap (\r -> [TextEdit r ""]) $ nubOrd ranges
        _changes = Just $ Map.singleton uri $ List tedit
        _title = title
        _kind = Just CodeActionQuickFix
        _diagnostics = Just $ List [diagnostic]
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        _command = Nothing
        _isPreferred = Nothing
        _disabled = Nothing
    removeAll [] = Nothing
    removeAll ranges = Just $ InR $ CodeAction{..} where
        tedit = concatMap (\r -> [TextEdit r ""]) ranges
        _changes = Just $ Map.singleton uri $ List tedit
        _title = "Remove all redundant exports"
        _kind = Just CodeActionQuickFix
        _diagnostics = Nothing
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        _command = Nothing
        _isPreferred = Nothing
        _disabled = Nothing

suggestRemoveRedundantExport :: ParsedModule -> Diagnostic -> Maybe (T.Text, [Range])
suggestRemoveRedundantExport ParsedModule{pm_parsed_source = L _ HsModule{..}} Diagnostic{..}
  | msg <- unifySpaces _message
  , Just export <- hsmodExports
  , Just exportRange <- getLocatedRange export
  , exports <- unLoc export
  , Just (removeFromExport, !ranges) <- fmap (getRanges exports . notInScope) (extractNotInScopeName msg)
                         <|> (,[_range]) <$> matchExportItem msg
                         <|> (,[_range]) <$> matchDupExport msg
  , subRange _range exportRange
    = Just ("Remove ‘" <> removeFromExport <> "’ from export", ranges)
  where
    matchExportItem msg = regexSingleMatch msg "The export item ‘([^’]+)’"
    matchDupExport msg = regexSingleMatch msg "Duplicate ‘([^’]+)’ in export list"
    getRanges exports txt = case smallerRangesForBindingExport exports (T.unpack txt) of
      [] -> (txt, [_range])
      ranges -> (txt, ranges)
suggestRemoveRedundantExport _ _ = Nothing

suggestDeleteUnusedBinding :: ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestDeleteUnusedBinding
  ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls}}
  contents
  Diagnostic{_range=_range,..}
-- Foo.hs:4:1: warning: [-Wunused-binds] Defined but not used: ‘f’
    | Just [name] <- matchRegexUnifySpaces _message ".*Defined but not used: ‘([^ ]+)’"
    , Just indexedContent <- indexedByPosition . T.unpack <$> contents
      = let edits = flip TextEdit "" <$> relatedRanges indexedContent (T.unpack name)
        in ([("Delete ‘" <> name <> "’", edits) | not (null edits)])
    | otherwise = []
    where
      relatedRanges indexedContent name =
        concatMap (findRelatedSpans indexedContent name) hsmodDecls
      toRange = realSrcSpanToRange
      extendForSpaces = extendToIncludePreviousNewlineIfPossible

      findRelatedSpans :: PositionIndexedString -> String -> Located (HsDecl GhcPs) -> [Range]
      findRelatedSpans
        indexedContent
        name
        (L (RealSrcSpan l) (ValD _ (extractNameAndMatchesFromFunBind -> Just (lname, matches)))) =
        case lname of
          (L nLoc _name) | isTheBinding nLoc ->
            let findSig (L (RealSrcSpan l) (SigD _ sig)) = findRelatedSigSpan indexedContent name l sig
                findSig _ = []
            in
              extendForSpaces indexedContent (toRange l) :
              concatMap findSig hsmodDecls
          _ -> concatMap (findRelatedSpanForMatch indexedContent name) matches
      findRelatedSpans _ _ _ = []

      extractNameAndMatchesFromFunBind
        :: HsBind GhcPs
        -> Maybe (Located (IdP GhcPs), [LMatch GhcPs (LHsExpr GhcPs)])
      extractNameAndMatchesFromFunBind
        FunBind
          { fun_id=lname
          , fun_matches=MG {mg_alts=L _ matches}
          } = Just (lname, matches)
      extractNameAndMatchesFromFunBind _ = Nothing

      findRelatedSigSpan :: PositionIndexedString -> String -> RealSrcSpan -> Sig GhcPs -> [Range]
      findRelatedSigSpan indexedContent name l sig =
        let maybeSpan = findRelatedSigSpan1 name sig
        in case maybeSpan of
          Just (_span, True) -> pure $ extendForSpaces indexedContent $ toRange l -- a :: Int
          Just (RealSrcSpan span, False) -> pure $ toRange span -- a, b :: Int, a is unused
          _ -> []

      -- Second of the tuple means there is only one match
      findRelatedSigSpan1 :: String -> Sig GhcPs -> Maybe (SrcSpan, Bool)
      findRelatedSigSpan1 name (TypeSig _ lnames _) =
        let maybeIdx = findIndex (\(L _ id) -> isSameName id name) lnames
        in case maybeIdx of
            Nothing -> Nothing
            Just _ | length lnames == 1 -> Just (getLoc $ head lnames, True)
            Just idx ->
              let targetLname = getLoc $ lnames !! idx
                  startLoc = srcSpanStart targetLname
                  endLoc = srcSpanEnd targetLname
                  startLoc' = if idx == 0
                              then startLoc
                              else srcSpanEnd . getLoc $ lnames !! (idx - 1)
                  endLoc' = if idx == 0 && idx < length lnames - 1
                            then srcSpanStart . getLoc $ lnames !! (idx + 1)
                            else endLoc
              in Just (mkSrcSpan startLoc' endLoc', False)
      findRelatedSigSpan1 _ _ = Nothing

      -- for where clause
      findRelatedSpanForMatch
        :: PositionIndexedString
        -> String
        -> LMatch GhcPs (LHsExpr GhcPs)
        -> [Range]
      findRelatedSpanForMatch
        indexedContent
        name
        (L _ Match{m_grhss=GRHSs{grhssLocalBinds}}) = do
        case grhssLocalBinds of
          (L _ (HsValBinds _ (ValBinds _ bag lsigs))) ->
            if isEmptyBag bag
            then []
            else concatMap (findRelatedSpanForHsBind indexedContent name lsigs) bag
          _ -> []
      findRelatedSpanForMatch _ _ _ = []

      findRelatedSpanForHsBind
        :: PositionIndexedString
        -> String
        -> [LSig GhcPs]
        -> LHsBind GhcPs
        -> [Range]
      findRelatedSpanForHsBind
        indexedContent
        name
        lsigs
        (L (RealSrcSpan l) (extractNameAndMatchesFromFunBind -> Just (lname, matches))) =
        if isTheBinding (getLoc lname)
        then
          let findSig (L (RealSrcSpan l) sig) = findRelatedSigSpan indexedContent name l sig
              findSig _ = []
          in extendForSpaces indexedContent (toRange l) : concatMap findSig lsigs
        else concatMap (findRelatedSpanForMatch indexedContent name) matches
      findRelatedSpanForHsBind _ _ _ _ = []

      isTheBinding :: SrcSpan -> Bool
      isTheBinding span = srcSpanToRange span == Just _range

      isSameName :: IdP GhcPs -> String -> Bool
      isSameName x name = showSDocUnsafe (ppr x) == name

data ExportsAs = ExportName | ExportPattern | ExportAll
  deriving (Eq)

getLocatedRange :: Located a -> Maybe Range
getLocatedRange = srcSpanToRange . getLoc

suggestExportUnusedTopBinding :: Maybe T.Text -> ParsedModule -> Diagnostic -> [(T.Text, [TextEdit])]
suggestExportUnusedTopBinding srcOpt ParsedModule{pm_parsed_source = L _ HsModule{..}} Diagnostic{..}
-- Foo.hs:4:1: warning: [-Wunused-top-binds] Defined but not used: ‘f’
-- Foo.hs:5:1: warning: [-Wunused-top-binds] Defined but not used: type constructor or class ‘F’
-- Foo.hs:6:1: warning: [-Wunused-top-binds] Defined but not used: data constructor ‘Bar’
  | Just source <- srcOpt
  , Just [name] <- matchRegexUnifySpaces _message ".*Defined but not used: ‘([^ ]+)’"
                   <|> matchRegexUnifySpaces _message ".*Defined but not used: type constructor or class ‘([^ ]+)’"
                   <|> matchRegexUnifySpaces _message ".*Defined but not used: data constructor ‘([^ ]+)’"
  , Just (exportType, _) <- find (matchWithDiagnostic _range . snd)
                            . mapMaybe
                                (\(L l b) -> if maybe False isTopLevel $ srcSpanToRange l
                                                then exportsAs b else Nothing)
                            $ hsmodDecls
  , Just pos <- fmap _end . getLocatedRange =<< hsmodExports
  , Just needComma <- needsComma source <$> hsmodExports
  , let exportName = (if needComma then "," else "") <> printExport exportType name
        insertPos = pos {_character = pred $ _character pos}
  = [("Export ‘" <> name <> "’", [TextEdit (Range insertPos insertPos) exportName])]
  | otherwise = []
  where
    -- we get the last export and the closing bracket and check for comma in that range
    needsComma :: T.Text -> Located [LIE GhcPs] -> Bool
    needsComma _ (L _ []) = False
    needsComma source (L (RealSrcSpan l) exports) =
      let closeParan = _end $ realSrcSpanToRange l
          lastExport = fmap _end . getLocatedRange $ last exports
      in case lastExport of
        Just lastExport -> not $ T.isInfixOf "," $ textInRange (Range lastExport closeParan) source
        _ -> False
    needsComma _ _ = False

    opLetter :: String
    opLetter = ":!#$%&*+./<=>?@\\^|-~"

    parenthesizeIfNeeds :: Bool -> T.Text -> T.Text
    parenthesizeIfNeeds needsTypeKeyword x
      | T.head x `elem` opLetter = (if needsTypeKeyword then "type " else "") <> "(" <> x <>")"
      | otherwise = x

    matchWithDiagnostic :: Range -> Located (IdP GhcPs) -> Bool
    matchWithDiagnostic Range{_start=l,_end=r} x =
      let loc = fmap _start . getLocatedRange $ x
       in loc >= Just l && loc <= Just r

    printExport :: ExportsAs -> T.Text -> T.Text
    printExport ExportName x = parenthesizeIfNeeds False x
    printExport ExportPattern x = "pattern " <> x
    printExport ExportAll x = parenthesizeIfNeeds True x <> "(..)"

    isTopLevel :: Range -> Bool
    isTopLevel l = (_character . _start) l == 0

    exportsAs :: HsDecl p -> Maybe (ExportsAs, Located (IdP p))
    exportsAs (ValD _ FunBind {fun_id})          = Just (ExportName, fun_id)
    exportsAs (ValD _ (PatSynBind _ PSB {psb_id})) = Just (ExportPattern, psb_id)
    exportsAs (TyClD _ SynDecl{tcdLName})      = Just (ExportName, tcdLName)
    exportsAs (TyClD _ DataDecl{tcdLName})     = Just (ExportAll, tcdLName)
    exportsAs (TyClD _ ClassDecl{tcdLName})    = Just (ExportAll, tcdLName)
    exportsAs (TyClD _ FamDecl{tcdFam})        = Just (ExportAll, fdLName tcdFam)
    exportsAs _                                = Nothing

suggestAddTypeAnnotationToSatisfyContraints :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestAddTypeAnnotationToSatisfyContraints sourceOpt Diagnostic{_range=_range,..}
-- File.hs:52:41: warning:
--     * Defaulting the following constraint to type ‘Integer’
--        Num p0 arising from the literal ‘1’
--     * In the expression: 1
--       In an equation for ‘f’: f = 1
-- File.hs:52:41: warning:
--     * Defaulting the following constraints to type ‘[Char]’
--        (Show a0)
--          arising from a use of ‘traceShow’
--          at A.hs:228:7-25
--        (IsString a0)
--          arising from the literal ‘"debug"’
--          at A.hs:228:17-23
--     * In the expression: traceShow "debug" a
--       In an equation for ‘f’: f a = traceShow "debug" a
-- File.hs:52:41: warning:
--     * Defaulting the following constraints to type ‘[Char]’
--         (Show a0)
--          arising from a use of ‘traceShow’
--          at A.hs:255:28-43
--        (IsString a0)
--          arising from the literal ‘"test"’
--          at /Users/serhiip/workspace/ghcide/src/Development/IDE/Plugin/CodeAction.hs:255:38-43
--     * In the fourth argument of ‘seq’, namely ‘(traceShow "test")’
--       In the expression: seq "test" seq "test" (traceShow "test")
--       In an equation for ‘f’:
--          f = seq "test" seq "test" (traceShow "test")
    | Just [ty, lit] <- matchRegexUnifySpaces _message (pat False False True False)
                        <|> matchRegexUnifySpaces _message (pat False False False True)
                        <|> matchRegexUnifySpaces _message (pat False False False False)
            = codeEdit ty lit (makeAnnotatedLit ty lit)
    | Just source <- sourceOpt
    , Just [ty, lit] <- matchRegexUnifySpaces _message (pat True True False False)
            = let lit' = makeAnnotatedLit ty lit;
                  tir = textInRange _range source
              in codeEdit ty lit (T.replace lit lit' tir)
    | otherwise = []
    where
      makeAnnotatedLit ty lit = "(" <> lit <> " :: " <> ty <> ")"
      pat multiple at inArg inExpr = T.concat [ ".*Defaulting the following constraint"
                                       , if multiple then "s" else ""
                                       , " to type ‘([^ ]+)’ "
                                       , ".*arising from the literal ‘(.+)’"
                                       , if inArg then ".+In the.+argument" else ""
                                       , if at then ".+at" else ""
                                       , if inExpr then ".+In the expression" else ""
                                       , ".+In the expression"
                                       ]
      codeEdit ty lit replacement =
        let title = "Add type annotation ‘" <> ty <> "’ to ‘" <> lit <> "’"
            edits = [TextEdit _range replacement]
        in  [( title, edits )]


suggestReplaceIdentifier :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestReplaceIdentifier contents Diagnostic{_range=_range,..}
-- File.hs:52:41: error:
--     * Variable not in scope:
--         suggestAcion :: Maybe T.Text -> Range -> Range
--     * Perhaps you meant ‘suggestAction’ (line 83)
-- File.hs:94:37: error:
--     Not in scope: ‘T.isPrfixOf’
--     Perhaps you meant one of these:
--       ‘T.isPrefixOf’ (imported from Data.Text),
--       ‘T.isInfixOf’ (imported from Data.Text),
--       ‘T.isSuffixOf’ (imported from Data.Text)
--     Module ‘Data.Text’ does not export ‘isPrfixOf’.
    | renameSuggestions@(_:_) <- extractRenamableTerms _message
        = [ ("Replace with ‘" <> name <> "’", [mkRenameEdit contents _range name]) | name <- renameSuggestions ]
    | otherwise = []

suggestNewDefinition :: IdeOptions -> ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestNewDefinition ideOptions parsedModule contents Diagnostic{_message, _range}
--     * Variable not in scope:
--         suggestAcion :: Maybe T.Text -> Range -> Range
    | Just [name, typ] <- matchRegexUnifySpaces message "Variable not in scope: ([^ ]+) :: ([^*•]+)"
    = newDefinitionAction ideOptions parsedModule _range name typ
    | Just [name, typ] <- matchRegexUnifySpaces message "Found hole: _([^ ]+) :: ([^*•]+) Or perhaps"
    , [(label, newDefinitionEdits)] <- newDefinitionAction ideOptions parsedModule _range name typ
    = [(label, mkRenameEdit contents _range name : newDefinitionEdits)]
    | otherwise = []
    where
      message = unifySpaces _message

newDefinitionAction :: IdeOptions -> ParsedModule -> Range -> T.Text -> T.Text -> [(T.Text, [TextEdit])]
newDefinitionAction IdeOptions{..} parsedModule Range{_start} name typ
    | Range _ lastLineP : _ <-
      [ realSrcSpanToRange sp
      | (L l@(RealSrcSpan sp) _) <- hsmodDecls
      , _start `isInsideSrcSpan` l]
    , nextLineP <- Position{ _line = _line lastLineP + 1, _character = 0}
    = [ ("Define " <> sig
        , [TextEdit (Range nextLineP nextLineP) (T.unlines ["", sig, name <> " = error \"not implemented\""])]
        )]
    | otherwise = []
  where
    colon = if optNewColonConvention then " : " else " :: "
    sig = name <> colon <> T.dropWhileEnd isSpace typ
    ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls}} = parsedModule


suggestFillTypeWildcard :: Diagnostic -> [(T.Text, [TextEdit])]
suggestFillTypeWildcard Diagnostic{_range=_range,..}
-- Foo.hs:3:8: error:
--     * Found type wildcard `_' standing for `p -> p1 -> p'

    | "Found type wildcard" `T.isInfixOf` _message
    , " standing for " `T.isInfixOf` _message
    , typeSignature <- extractWildCardTypeSignature _message
        =  [("Use type signature: ‘" <> typeSignature <> "’", [TextEdit _range typeSignature])]
    | otherwise = []

suggestModuleTypo :: Diagnostic -> [(T.Text, [TextEdit])]
suggestModuleTypo Diagnostic{_range=_range,..}
-- src/Development/IDE/Core/Compile.hs:58:1: error:
--     Could not find module ‘Data.Cha’
--     Perhaps you meant Data.Char (from base-4.12.0.0)
    | "Could not find module" `T.isInfixOf` _message
    , "Perhaps you meant"     `T.isInfixOf` _message = let
      findSuggestedModules = map (head . T.words) . drop 2 . T.lines
      proposeModule mod = ("replace with " <> mod, [TextEdit _range mod])
      in map proposeModule $ nubOrd $ findSuggestedModules _message
    | otherwise = []

suggestFillHole :: Diagnostic -> [(T.Text, [TextEdit])]
suggestFillHole Diagnostic{_range=_range,..}
    | Just holeName <- extractHoleName _message
    , (holeFits, refFits) <- processHoleSuggestions (T.lines _message)
    = map (proposeHoleFit holeName False) holeFits
    ++ map (proposeHoleFit holeName True) refFits
    | otherwise = []
    where
      extractHoleName = fmap head . flip matchRegexUnifySpaces "Found hole: ([^ ]*)"
      proposeHoleFit holeName parenthise name =
          ( "replace " <> holeName <> " with " <> name
          , [TextEdit _range $ if parenthise then parens name else name])
      parens x = "(" <> x <> ")"

processHoleSuggestions :: [T.Text] -> ([T.Text], [T.Text])
processHoleSuggestions mm = (holeSuggestions, refSuggestions)
{-
    • Found hole: _ :: LSP.Handlers

      Valid hole fits include def
      Valid refinement hole fits include
        fromMaybe (_ :: LSP.Handlers) (_ :: Maybe LSP.Handlers)
        fromJust (_ :: Maybe LSP.Handlers)
        haskell-lsp-types-0.22.0.0:Language.LSP.Types.Window.$sel:_value:ProgressParams (_ :: ProgressParams
                                                                                                        LSP.Handlers)
        T.foldl (_ :: LSP.Handlers -> Char -> LSP.Handlers)
                (_ :: LSP.Handlers)
                (_ :: T.Text)
        T.foldl' (_ :: LSP.Handlers -> Char -> LSP.Handlers)
                 (_ :: LSP.Handlers)
                 (_ :: T.Text)
-}
  where
    t = id @T.Text
    holeSuggestions = do
      -- get the text indented under Valid hole fits
      validHolesSection <-
        getIndentedGroupsBy (=~ t " *Valid (hole fits|substitutions) include") mm
      -- the Valid hole fits line can contain a hole fit
      holeFitLine <-
        mapHead
            (mrAfter . (=~ t " *Valid (hole fits|substitutions) include"))
            validHolesSection
      let holeFit = T.strip $ T.takeWhile (/= ':') holeFitLine
      guard (not $ T.null holeFit)
      return holeFit
    refSuggestions = do -- @[]
      -- get the text indented under Valid refinement hole fits
      refinementSection <-
        getIndentedGroupsBy (=~ t " *Valid refinement hole fits include") mm
      -- get the text for each hole fit
      holeFitLines <- getIndentedGroups (tail refinementSection)
      let holeFit = T.strip $ T.unwords holeFitLines
      guard $ not $ holeFit =~ t "Some refinement hole fits suppressed"
      return holeFit

    mapHead f (a:aa) = f a : aa
    mapHead _ [] = []

-- > getIndentedGroups [" H1", "  l1", "  l2", " H2", "  l3"] = [[" H1,", "  l1", "  l2"], [" H2", "  l3"]]
getIndentedGroups :: [T.Text] -> [[T.Text]]
getIndentedGroups [] = []
getIndentedGroups ll@(l:_) = getIndentedGroupsBy ((== indentation l) . indentation) ll
-- |
-- > getIndentedGroupsBy (" H" `isPrefixOf`) [" H1", "  l1", "  l2", " H2", "  l3"] = [[" H1", "  l1", "  l2"], [" H2", "  l3"]]
getIndentedGroupsBy :: (T.Text -> Bool) -> [T.Text] -> [[T.Text]]
getIndentedGroupsBy pred inp = case dropWhile (not.pred) inp of
    (l:ll) -> case span (\l' -> indentation l < indentation l') ll of
        (indented, rest) -> (l:indented) : getIndentedGroupsBy pred rest
    _ -> []

indentation :: T.Text -> Int
indentation = T.length . T.takeWhile isSpace

suggestExtendImport :: ExportsMap -> ParsedSource -> Diagnostic -> [(T.Text, [Rewrite])]
suggestExtendImport exportsMap (L _ HsModule {hsmodImports}) Diagnostic{_range=_range,..}
    | Just [binding, mod, srcspan] <-
      matchRegexUnifySpaces _message
      "Perhaps you want to add ‘([^’]*)’ to the import list in the import of ‘([^’]*)’ *\\((.*)\\).$"
    = suggestions hsmodImports binding mod srcspan
    | Just (binding, mod_srcspan) <-
      matchRegExMultipleImports _message
    = mod_srcspan >>= uncurry (suggestions hsmodImports binding)
    | otherwise = []
    where
        unImportStyle (ImportTopLevel x) = (Nothing, T.unpack x)
        unImportStyle (ImportViaParent x y) = (Just $ T.unpack y, T.unpack x)
        suggestions decls binding mod srcspan
          |  range <- case [ x | (x,"") <- readSrcSpan (T.unpack srcspan)] of
                [s] -> let x = realSrcSpanToRange s
                   in x{_end = (_end x){_character = succ (_character (_end x))}}
                _ -> error "bug in srcspan parser",
            Just decl <- findImportDeclByRange decls range,
            Just ident <- lookupExportMap binding mod
          = [ ( "Add " <> renderImportStyle importStyle <> " to the import list of " <> mod
              , [uncurry extendImport (unImportStyle importStyle) decl]
              )
            | importStyle <- NE.toList $ importStyles ident
            ]
          | otherwise = []
        lookupExportMap binding mod
          | Just match <- Map.lookup binding (getExportsMap exportsMap)
          , [ident] <- filter (\ident -> moduleNameText ident == mod) (Set.toList match)
           = Just ident

            -- fallback to using GHC suggestion even though it is not always correct
          | otherwise
          = Just IdentInfo
                { name = binding
                , rendered = binding
                , parent = Nothing
                , isDatacon = False
                , moduleNameText = mod}

data HidingMode
    = HideOthers [ModuleTarget]
    | ToQualified
        Bool
        -- ^ Parenthesised?
        ModuleName
    deriving (Show)

data ModuleTarget
    = ExistingImp (NonEmpty (LImportDecl GhcPs))
    | ImplicitPrelude [LImportDecl GhcPs]
    deriving (Show)

targetImports :: ModuleTarget -> [LImportDecl GhcPs]
targetImports (ExistingImp ne) = NE.toList ne
targetImports (ImplicitPrelude xs) = xs

oneAndOthers :: [a] -> [(a, [a])]
oneAndOthers = go
    where
        go [] = []
        go (x : xs) = (x, xs) : map (second (x :)) (go xs)

isPreludeImplicit :: DynFlags -> Bool
isPreludeImplicit = xopt Lang.ImplicitPrelude

-- | Suggests disambiguation for ambiguous symbols.
suggestImportDisambiguation ::
    DynFlags ->
    Maybe T.Text ->
    ParsedSource ->
    Diagnostic ->
    [(T.Text, [Rewrite])]
suggestImportDisambiguation df (Just txt) ps@(L _ HsModule {hsmodImports}) diag@Diagnostic {..}
    | Just [ambiguous] <-
        matchRegexUnifySpaces
            _message
            "Ambiguous occurrence ‘([^’]+)’"
      , Just modules <-
            map last
                <$> allMatchRegexUnifySpaces _message "imported from ‘([^’]+)’" =
        suggestions ambiguous modules
    | otherwise = []
    where
        locDic =
            fmap (NE.fromList . DL.toList) $
            Map.fromListWith (<>) $
                map
                    ( \i@(L _ idecl) ->
                        ( T.pack $ moduleNameString $ unLoc $ ideclName idecl
                        , DL.singleton i
                        )
                    )
                    hsmodImports
        toModuleTarget "Prelude"
            | isPreludeImplicit df
             = Just $ ImplicitPrelude $
                maybe [] NE.toList (Map.lookup "Prelude" locDic)
        toModuleTarget mName = ExistingImp <$> Map.lookup mName locDic
        parensed =
            "(" `T.isPrefixOf` T.strip (textInRange _range txt)
        suggestions symbol mods
            | Just targets <- mapM toModuleTarget mods =
                sortOn fst
                [ ( renderUniquify mode modNameText symbol
                  , disambiguateSymbol ps diag symbol mode
                  )
                | (modTarget, restImports) <- oneAndOthers targets
                , let modName = targetModuleName modTarget
                      modNameText = T.pack $ moduleNameString modName
                , mode <-
                    HideOthers restImports :
                    [ ToQualified parensed qual
                    | ExistingImp imps <- [modTarget]
                    , L _ qual <- nubOrd $ mapMaybe (ideclAs . unLoc)
                        $ NE.toList imps
                    ]
                    ++ [ToQualified parensed modName
                        | any (occursUnqualified symbol . unLoc)
                            (targetImports modTarget)
                        || case modTarget of
                            ImplicitPrelude{} -> True
                            _ -> False
                        ]
                ]
            | otherwise = []
        renderUniquify HideOthers {} modName symbol =
            "Use " <> modName <> " for " <> symbol <> ", hiding other imports"
        renderUniquify (ToQualified _ qual) _ symbol =
            "Replace with qualified: "
                <> T.pack (moduleNameString qual)
                <> "."
                <> symbol
suggestImportDisambiguation _ _ _ _ = []

occursUnqualified :: T.Text -> ImportDecl GhcPs -> Bool
occursUnqualified symbol ImportDecl{..}
    | isNothing ideclAs = Just False /=
            -- I don't find this particularly comprehensible,
            -- but HLint suggested me to do so...
        (ideclHiding <&> \(isHiding, L _ ents) ->
            let occurs = any ((symbol `symbolOccursIn`) . unLoc) ents
            in isHiding && not occurs || not isHiding && occurs
        )
occursUnqualified _ _ = False

symbolOccursIn :: T.Text -> IE GhcPs -> Bool
symbolOccursIn symb = any ((== symb). showNameWithoutUniques) . ieNames

targetModuleName :: ModuleTarget -> ModuleName
targetModuleName ImplicitPrelude{} = mkModuleName "Prelude"
targetModuleName (ExistingImp (L _ ImportDecl{..} :| _)) =
    unLoc ideclName
targetModuleName (ExistingImp _) =
    error "Cannot happen!"

disambiguateSymbol ::
    ParsedSource ->
    Diagnostic ->
    T.Text ->
    HidingMode ->
    [Rewrite]
disambiguateSymbol pm Diagnostic {..} (T.unpack -> symbol) = \case
    (HideOthers hiddens0) ->
        [ hideSymbol symbol idecl
        | ExistingImp idecls <- hiddens0
        , idecl <- NE.toList idecls
        ]
            ++ mconcat
                [ if null imps
                    then maybeToList $ hideImplicitPreludeSymbol symbol pm
                    else hideSymbol symbol <$> imps
                | ImplicitPrelude imps <- hiddens0
                ]
    (ToQualified parensed qualMod) ->
        let occSym = mkVarOcc symbol
            rdr = Qual qualMod occSym
         in [ if parensed
                then Rewrite (rangeToSrcSpan "<dummy>" _range) $ \df ->
                    liftParseAST @(HsExpr GhcPs) df $
                    prettyPrint $
                        HsVar @GhcPs noExtField $
                            L (UnhelpfulSpan "") rdr
                else Rewrite (rangeToSrcSpan "<dummy>" _range) $ \df ->
                    liftParseAST @RdrName df $
                    prettyPrint $ L (UnhelpfulSpan "") rdr
            ]

findImportDeclByRange :: [LImportDecl GhcPs] -> Range -> Maybe (LImportDecl GhcPs)
findImportDeclByRange xs range = find (\(L l _)-> srcSpanToRange l == Just range) xs

suggestFixConstructorImport :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestFixConstructorImport _ Diagnostic{_range=_range,..}
    -- ‘Success’ is a data constructor of ‘Result’
    -- To import it use
    -- import Data.Aeson.Types( Result( Success ) )
    -- or
    -- import Data.Aeson.Types( Result(..) ) (lsp-ui)
  | Just [constructor, typ] <-
    matchRegexUnifySpaces _message
    "‘([^’]*)’ is a data constructor of ‘([^’]*)’ To import it use"
  = let fixedImport = typ <> "(" <> constructor <> ")"
    in [("Fix import of " <> fixedImport, [TextEdit _range fixedImport])]
  | otherwise = []
-- | Suggests a constraint for a declaration for which a constraint is missing.
suggestConstraint :: DynFlags -> ParsedSource -> Diagnostic -> [(T.Text, [Rewrite])]
suggestConstraint df parsedModule diag@Diagnostic {..}
  | Just missingConstraint <- findMissingConstraint _message
  = let codeAction = if _message =~ ("the type signature for:" :: String)
                        then suggestFunctionConstraint df parsedModule
                        else suggestInstanceConstraint df parsedModule
     in map (second (:[])) $ codeAction diag missingConstraint
  | otherwise = []
    where
      findMissingConstraint :: T.Text -> Maybe T.Text
      findMissingConstraint t =
        let regex = "(No instance for|Could not deduce) \\((.+)\\) arising from" -- a use of / a do statement
            regexImplicitParams = "Could not deduce: (\\?.+) arising from a use of"
            match = matchRegexUnifySpaces t regex
            matchImplicitParams = matchRegexUnifySpaces t regexImplicitParams
        in match <|> matchImplicitParams <&> last

-- | Suggests a constraint for an instance declaration for which a constraint is missing.
suggestInstanceConstraint :: DynFlags -> ParsedSource -> Diagnostic -> T.Text -> [(T.Text, Rewrite)]

suggestInstanceConstraint df (L _ HsModule {hsmodDecls}) Diagnostic {..} missingConstraint
  | Just instHead <- instanceHead
  = [(actionTitle missingConstraint , appendConstraint (T.unpack missingConstraint) instHead)]
  | otherwise = []
    where
      instanceHead
        -- Suggests a constraint for an instance declaration with no existing constraints.
        -- • No instance for (Eq a) arising from a use of ‘==’
        --   Possible fix: add (Eq a) to the context of the instance declaration
        -- • In the expression: x == y
        --   In an equation for ‘==’: (Wrap x) == (Wrap y) = x == y
        --   In the instance declaration for ‘Eq (Wrap a)’
        | Just [instanceDeclaration] <- matchRegexUnifySpaces _message "In the instance declaration for ‘([^`]*)’"
        , Just instHead <- findInstanceHead df (T.unpack instanceDeclaration) hsmodDecls
        = Just instHead
        -- Suggests a constraint for an instance declaration with one or more existing constraints.
        -- • Could not deduce (Eq b) arising from a use of ‘==’
        --   from the context: Eq a
        --     bound by the instance declaration at /path/to/Main.hs:7:10-32
        --   Possible fix: add (Eq b) to the context of the instance declaration
        -- • In the second argument of ‘(&&)’, namely ‘x' == y'’
        --   In the expression: x == y && x' == y'
        --   In an equation for ‘==’:
        --       (Pair x x') == (Pair y y') = x == y && x' == y'
        | Just [instanceLineStr, constraintFirstCharStr]
            <- matchRegexUnifySpaces _message "bound by the instance declaration at .+:([0-9]+):([0-9]+)"
        , Just (L _ (InstD _ (ClsInstD _ ClsInstDecl {cid_poly_ty = HsIB{hsib_body}})))
            <- findDeclContainingLoc (Position (readPositionNumber instanceLineStr) (readPositionNumber constraintFirstCharStr)) hsmodDecls
        = Just hsib_body
        | otherwise
        = Nothing

      readPositionNumber :: T.Text -> Int
      readPositionNumber = T.unpack >>> read

      actionTitle :: T.Text -> T.Text
      actionTitle constraint = "Add `" <> constraint
        <> "` to the context of the instance declaration"

suggestImplicitParameter ::
  ParsedSource ->
  Diagnostic ->
  [(T.Text, [Rewrite])]
suggestImplicitParameter (L _ HsModule {hsmodDecls}) Diagnostic {_message, _range}
  | Just [implicitT] <- matchRegexUnifySpaces _message "Unbound implicit parameter \\(([^:]+::.+)\\) arising",
    Just (L _ (ValD _ FunBind {fun_id = L _ funId})) <- findDeclContainingLoc (_start _range) hsmodDecls,
    Just (TypeSig _ _ HsWC {hswc_body = HsIB {hsib_body}}) <- findSigOfDecl (== funId) hsmodDecls
    =
      [( "Add " <> implicitT <> " to the context of " <> T.pack (printRdrName funId)
        , [appendConstraint (T.unpack implicitT) hsib_body])]
  | otherwise = []

findTypeSignatureName :: T.Text -> Maybe T.Text
findTypeSignatureName t = matchRegexUnifySpaces t "([^ ]+) :: " <&> head

findTypeSignatureLine :: T.Text -> T.Text -> Int
findTypeSignatureLine contents typeSignatureName =
  T.splitOn (typeSignatureName <> " :: ") contents & head & T.lines & length

-- | Suggests a constraint for a type signature with any number of existing constraints.
suggestFunctionConstraint :: DynFlags -> ParsedSource -> Diagnostic -> T.Text -> [(T.Text, Rewrite)]

suggestFunctionConstraint df (L _ HsModule {hsmodDecls}) Diagnostic {..} missingConstraint
-- • No instance for (Eq a) arising from a use of ‘==’
--   Possible fix:
--     add (Eq a) to the context of
--       the type signature for:
--         eq :: forall a. a -> a -> Bool
-- • In the expression: x == y
--   In an equation for ‘eq’: eq x y = x == y

-- • Could not deduce (Eq b) arising from a use of ‘==’
--   from the context: Eq a
--     bound by the type signature for:
--                eq :: forall a b. Eq a => Pair a b -> Pair a b -> Bool
--     at Main.hs:5:1-42
--   Possible fix:
--     add (Eq b) to the context of
--       the type signature for:
--         eq :: forall a b. Eq a => Pair a b -> Pair a b -> Bool
-- • In the second argument of ‘(&&)’, namely ‘y == y'’
--   In the expression: x == x' && y == y'
--   In an equation for ‘eq’:
--       eq (Pair x y) (Pair x' y') = x == x' && y == y'
  | Just typeSignatureName <- findTypeSignatureName _message
  , Just (TypeSig _ _ HsWC{hswc_body = HsIB {hsib_body = sig}})
    <- findSigOfDecl ((T.unpack typeSignatureName ==) . showSDoc df . ppr) hsmodDecls
  , title <- actionTitle missingConstraint typeSignatureName
  = [(title, appendConstraint (T.unpack missingConstraint) sig)]
  | otherwise
  = []
    where
      actionTitle :: T.Text -> T.Text -> T.Text
      actionTitle constraint typeSignatureName = "Add `" <> constraint
        <> "` to the context of the type signature for `" <> typeSignatureName <> "`"

-- | Suggests the removal of a redundant constraint for a type signature.
removeRedundantConstraints :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
removeRedundantConstraints mContents Diagnostic{..}
-- • Redundant constraint: Eq a
-- • In the type signature for:
--      foo :: forall a. Eq a => a -> a
-- • Redundant constraints: (Monoid a, Show a)
-- • In the type signature for:
--      foo :: forall a. (Num a, Monoid a, Eq a, Show a) => a -> Bool
  | Just contents <- mContents
  -- Account for both "Redundant constraint" and "Redundant constraints".
  , True <- "Redundant constraint" `T.isInfixOf` _message
  , Just typeSignatureName <- findTypeSignatureName _message
  , Just redundantConstraintList <- findRedundantConstraints _message
  , Just constraints <- findConstraints contents typeSignatureName
  = let constraintList = parseConstraints constraints
        newConstraints = buildNewConstraints constraintList redundantConstraintList
        typeSignatureLine = findTypeSignatureLine contents typeSignatureName
        typeSignatureFirstChar = T.length $ typeSignatureName <> " :: "
        startOfConstraint = Position typeSignatureLine typeSignatureFirstChar
        endOfConstraint = Position typeSignatureLine $
          typeSignatureFirstChar + T.length (constraints <> " => ")
        range = Range startOfConstraint endOfConstraint
     in [(actionTitle redundantConstraintList typeSignatureName, [TextEdit range newConstraints])]
  | otherwise = []
    where
      parseConstraints :: T.Text -> [T.Text]
      parseConstraints t = t
        & (T.strip >>> stripConstraintsParens >>> T.splitOn ",")
        <&> T.strip

      stripConstraintsParens :: T.Text -> T.Text
      stripConstraintsParens constraints =
        if "(" `T.isPrefixOf` constraints
           then constraints & T.drop 1 & T.dropEnd 1 & T.strip
           else constraints

      findRedundantConstraints :: T.Text -> Maybe [T.Text]
      findRedundantConstraints t = t
        & T.lines
        & head
        & T.strip
        & (`matchRegexUnifySpaces` "Redundant constraints?: (.+)")
        <&> (head >>> parseConstraints)

      -- If the type signature is not formatted as expected (arbitrary number of spaces,
      -- line feeds...), just fail.
      findConstraints :: T.Text -> T.Text -> Maybe T.Text
      findConstraints contents typeSignatureName = do
        constraints <- contents
          & T.splitOn (typeSignatureName <> " :: ")
          & (`atMay` 1)
          >>= (T.splitOn " => " >>> (`atMay` 0))
        guard $ not $ "\n" `T.isInfixOf` constraints || T.strip constraints /= constraints
        return constraints

      formatConstraints :: [T.Text] -> T.Text
      formatConstraints [] = ""
      formatConstraints [constraint] = constraint
      formatConstraints constraintList = constraintList
        & T.intercalate ", "
        & \cs -> "(" <> cs <> ")"

      formatConstraintsWithArrow :: [T.Text] -> T.Text
      formatConstraintsWithArrow [] = ""
      formatConstraintsWithArrow cs = cs & formatConstraints & (<> " => ")

      buildNewConstraints :: [T.Text] -> [T.Text] -> T.Text
      buildNewConstraints constraintList redundantConstraintList =
        formatConstraintsWithArrow $ constraintList \\ redundantConstraintList

      actionTitle :: [T.Text] -> T.Text -> T.Text
      actionTitle constraintList typeSignatureName =
        "Remove redundant constraint" <> (if length constraintList == 1 then "" else "s") <> " `"
        <> formatConstraints constraintList
        <> "` from the context of the type signature for `" <> typeSignatureName <> "`"

-------------------------------------------------------------------------------------------------

suggestNewImport :: ExportsMap -> ParsedModule -> Diagnostic -> [(T.Text, [TextEdit])]
suggestNewImport packageExportsMap ParsedModule {pm_parsed_source = L _ HsModule {..}} Diagnostic{_message}
  | msg <- unifySpaces _message
  , Just thingMissing <- extractNotInScopeName msg
  , qual <- extractQualifiedModuleName msg
  , Just insertLine <- case hsmodImports of
        [] -> case srcSpanStart $ getLoc (head hsmodDecls) of
          RealSrcLoc s -> Just $ srcLocLine s - 1
          _ -> Nothing
        _ -> case srcSpanEnd $ getLoc (last hsmodImports) of
          RealSrcLoc s -> Just $ srcLocLine s
          _ -> Nothing
  , insertPos <- Position insertLine 0
  , extendImportSuggestions <- matchRegexUnifySpaces msg
    "Perhaps you want to add ‘[^’]*’ to the import list in the import of ‘([^’]*)’"
  = [(imp, [TextEdit (Range insertPos insertPos) (imp <> "\n")])
    | imp <- sort $ constructNewImportSuggestions packageExportsMap (qual, thingMissing) extendImportSuggestions
    ]
suggestNewImport _ _ _ = []

constructNewImportSuggestions
  :: ExportsMap -> (Maybe T.Text, NotInScope) -> Maybe [T.Text] -> [T.Text]
constructNewImportSuggestions exportsMap (qual, thingMissing) notTheseModules = nubOrd
  [ suggestion
  | Just name <- [T.stripPrefix (maybe "" (<> ".") qual) $ notInScope thingMissing]
  , identInfo <- maybe [] Set.toList $ Map.lookup name (getExportsMap exportsMap)
  , canUseIdent thingMissing identInfo
  , moduleNameText identInfo `notElem` fromMaybe [] notTheseModules
  , suggestion <- renderNewImport identInfo
  ]
 where
  renderNewImport :: IdentInfo -> [T.Text]
  renderNewImport identInfo
    | Just q <- qual
    , asQ <- if q == m then "" else " as " <> q
    = ["import qualified " <> m <> asQ]
    | otherwise
    = ["import " <> m <> " (" <> renderImportStyle importStyle <> ")"
      | importStyle <- NE.toList $ importStyles identInfo] ++
      ["import " <> m ]
    where
        m = moduleNameText identInfo

canUseIdent :: NotInScope -> IdentInfo -> Bool
canUseIdent NotInScopeDataConstructor{} = isDatacon
canUseIdent _                           = const True

data NotInScope
    = NotInScopeDataConstructor T.Text
    | NotInScopeTypeConstructorOrClass T.Text
    | NotInScopeThing T.Text
    deriving Show

notInScope :: NotInScope -> T.Text
notInScope (NotInScopeDataConstructor t) = t
notInScope (NotInScopeTypeConstructorOrClass t) = t
notInScope (NotInScopeThing t) = t

extractNotInScopeName :: T.Text -> Maybe NotInScope
extractNotInScopeName x
  | Just [name] <- matchRegexUnifySpaces x "Data constructor not in scope: ([^ ]+)"
  = Just $ NotInScopeDataConstructor name
  | Just [name] <- matchRegexUnifySpaces x "Not in scope: data constructor [^‘]*‘([^’]*)’"
  = Just $ NotInScopeDataConstructor name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope: type constructor or class [^‘]*‘([^’]*)’"
  = Just $ NotInScopeTypeConstructorOrClass name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope: \\(([^‘ ]+)\\)"
  = Just $ NotInScopeThing name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope: ([^‘ ]+)"
  = Just $ NotInScopeThing name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope:[^‘]*‘([^’]*)’"
  = Just $ NotInScopeThing name
  | otherwise
  = Nothing

extractQualifiedModuleName :: T.Text -> Maybe T.Text
extractQualifiedModuleName x
  | Just [m] <- matchRegexUnifySpaces x "module named [^‘]*‘([^’]*)’"
  = Just m
  | otherwise
  = Nothing

-------------------------------------------------------------------------------------------------


mkRenameEdit :: Maybe T.Text -> Range -> T.Text -> TextEdit
mkRenameEdit contents range name =
    if maybeIsInfixFunction == Just True
      then TextEdit range ("`" <> name <> "`")
      else TextEdit range name
  where
    maybeIsInfixFunction = do
      curr <- textInRange range <$> contents
      pure $ "`" `T.isPrefixOf` curr && "`" `T.isSuffixOf` curr

extractWildCardTypeSignature :: T.Text -> T.Text
extractWildCardTypeSignature =
  -- inferring when parens are actually needed around the type signature would
  -- require understanding both the precedence of the context of the _ and of
  -- the signature itself. Inserting them unconditionally is ugly but safe.
  ("(" `T.append`) . (`T.append` ")") .
  T.takeWhile (/='’') . T.dropWhile (=='‘') . T.dropWhile (/='‘') .
  snd . T.breakOnEnd "standing for "

extractRenamableTerms :: T.Text -> [T.Text]
extractRenamableTerms msg
  -- Account for both "Variable not in scope" and "Not in scope"
  | "ot in scope:" `T.isInfixOf` msg = extractSuggestions msg
  | otherwise = []
  where
    extractSuggestions = map getEnclosed
                       . concatMap singleSuggestions
                       . filter isKnownSymbol
                       . T.lines
    singleSuggestions = T.splitOn "), " -- Each suggestion is comma delimited
    isKnownSymbol t = " (imported from" `T.isInfixOf` t || " (line " `T.isInfixOf` t
    getEnclosed = T.dropWhile (== '‘')
                . T.dropWhileEnd (== '’')
                . T.dropAround (\c -> c /= '‘' && c /= '’')

-- | If a range takes up a whole line (it begins at the start of the line and there's only whitespace
-- between the end of the range and the next newline), extend the range to take up the whole line.
extendToWholeLineIfPossible :: Maybe T.Text -> Range -> Range
extendToWholeLineIfPossible contents range@Range{..} =
    let newlineAfter = maybe False (T.isPrefixOf "\n" . T.dropWhile (\x -> isSpace x && x /= '\n') . snd . splitTextAtPosition _end) contents
        extend = newlineAfter && _character _start == 0 -- takes up an entire line, so remove the whole line
    in if extend then Range _start (Position (_line _end + 1) 0) else range

splitTextAtPosition :: Position -> T.Text -> (T.Text, T.Text)
splitTextAtPosition (Position row col) x
    | (preRow, mid:postRow) <- splitAt row $ T.splitOn "\n" x
    , (preCol, postCol) <- T.splitAt col mid
        = (T.intercalate "\n" $ preRow ++ [preCol], T.intercalate "\n" $ postCol : postRow)
    | otherwise = (x, T.empty)

-- | Returns [start .. end[
textInRange :: Range -> T.Text -> T.Text
textInRange (Range (Position startRow startCol) (Position endRow endCol)) text =
    case compare startRow endRow of
      LT ->
        let (linesInRangeBeforeEndLine, endLineAndFurtherLines) = splitAt (endRow - startRow) linesBeginningWithStartLine
            (textInRangeInFirstLine, linesBetween) = case linesInRangeBeforeEndLine of
              [] -> ("", [])
              firstLine:linesInBetween -> (T.drop startCol firstLine, linesInBetween)
            maybeTextInRangeInEndLine = T.take endCol <$> listToMaybe endLineAndFurtherLines
        in T.intercalate "\n" (textInRangeInFirstLine : linesBetween ++ maybeToList maybeTextInRangeInEndLine)
      EQ ->
        let line = fromMaybe "" (listToMaybe linesBeginningWithStartLine)
        in T.take (endCol - startCol) (T.drop startCol line)
      GT -> ""
    where
      linesBeginningWithStartLine = drop startRow (T.splitOn "\n" text)

-- | Returns the ranges for a binding in an import declaration
rangesForBindingImport :: ImportDecl GhcPs -> String -> [Range]
rangesForBindingImport ImportDecl{ideclHiding = Just (False, L _ lies)} b =
    concatMap (mapMaybe srcSpanToRange . rangesForBinding' b') lies
  where
    b' = modifyBinding b
rangesForBindingImport _ _ = []

modifyBinding :: String -> String
modifyBinding = wrapOperatorInParens . unqualify
  where
    wrapOperatorInParens x = if isAlpha (head x) then x else "(" <> x <> ")"
    unqualify x = snd $ breakOnEnd "." x

smallerRangesForBindingExport :: [LIE GhcPs] -> String -> [Range]
smallerRangesForBindingExport lies b =
    concatMap (mapMaybe srcSpanToRange . ranges') lies
  where
    b' = modifyBinding b
    ranges' (L _ (IEThingWith _ thing _  inners labels))
      | showSDocUnsafe (ppr thing) == b' = []
      | otherwise =
          [ l' | L l' x <- inners, showSDocUnsafe (ppr x) == b'] ++
          [ l' | L l' x <- labels, showSDocUnsafe (ppr x) == b']
    ranges' _ = []

rangesForBinding' :: String -> LIE GhcPs -> [SrcSpan]
rangesForBinding' b (L l x@IEVar{}) | showSDocUnsafe (ppr x) == b = [l]
rangesForBinding' b (L l x@IEThingAbs{}) | showSDocUnsafe (ppr x) == b = [l]
rangesForBinding' b (L l (IEThingAll _ x)) | showSDocUnsafe (ppr x) == b = [l]
rangesForBinding' b (L l (IEThingWith _ thing _  inners labels))
    | showSDocUnsafe (ppr thing) == b = [l]
    | otherwise =
        [ l' | L l' x <- inners, showSDocUnsafe (ppr x) == b] ++
        [ l' | L l' x <- labels, showSDocUnsafe (ppr x) == b]
rangesForBinding' _ _ = []

-- | 'matchRegex' combined with 'unifySpaces'
matchRegexUnifySpaces :: T.Text -> T.Text -> Maybe [T.Text]
matchRegexUnifySpaces message = matchRegex (unifySpaces message)

-- | 'allMatchRegex' combined with 'unifySpaces'
allMatchRegexUnifySpaces :: T.Text -> T.Text -> Maybe [[T.Text]]
allMatchRegexUnifySpaces message =
    allMatchRegex (unifySpaces message)

-- | Returns Just (the submatches) for the first capture, or Nothing.
matchRegex :: T.Text -> T.Text -> Maybe [T.Text]
matchRegex message regex = case message =~~ regex of
    Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, bindings) -> Just bindings
    Nothing -> Nothing

-- | Returns Just (all matches) for the first capture, or Nothing.
allMatchRegex :: T.Text -> T.Text -> Maybe [[T.Text]]
allMatchRegex message regex = message =~~ regex


unifySpaces :: T.Text -> T.Text
unifySpaces    = T.unwords . T.words

-- functions to help parse multiple import suggestions

-- | Returns the first match if found
regexSingleMatch :: T.Text -> T.Text -> Maybe T.Text
regexSingleMatch msg regex = case matchRegexUnifySpaces msg regex of
    Just (h:_) -> Just h
    _ -> Nothing

-- | Parses tuples like (‘Data.Map’, (app/ModuleB.hs:2:1-18)) and
-- | return (Data.Map, app/ModuleB.hs:2:1-18)
regExPair :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
regExPair (modname, srcpair) = do
  x <- regexSingleMatch modname "‘([^’]*)’"
  y <- regexSingleMatch srcpair "\\((.*)\\)"
  return (x, y)

-- | Process a list of (module_name, filename:src_span) values
-- | Eg. [(Data.Map, app/ModuleB.hs:2:1-18), (Data.HashMap.Strict, app/ModuleB.hs:3:1-29)]
regExImports :: T.Text -> Maybe [(T.Text, T.Text)]
regExImports msg = result
  where
    parts = T.words msg
    isPrefix = not . T.isPrefixOf "("
    (mod, srcspan) = partition isPrefix  parts
    -- check we have matching pairs like (Data.Map, (app/src.hs:1:2-18))
    result = if length mod == length srcspan then
               regExPair `traverse` zip mod srcspan
             else Nothing

matchRegExMultipleImports :: T.Text -> Maybe (T.Text, [(T.Text, T.Text)])
matchRegExMultipleImports message = do
  let pat = T.pack "Perhaps you want to add ‘([^’]*)’ to one of these import lists: *(‘.*\\))$"
  (binding, imports) <- case matchRegexUnifySpaces message pat of
                            Just [x, xs] -> Just (x, xs)
                            _ -> Nothing
  imps <- regExImports imports
  return (binding, imps)

-- | Possible import styles for an 'IdentInfo'.
--
-- The first 'Text' parameter corresponds to the 'rendered' field of the
-- 'IdentInfo'.
data ImportStyle
    = ImportTopLevel T.Text
      -- ^ Import a top-level export from a module, e.g., a function, a type, a
      -- class.
      --
      -- > import M (?)
      --
      -- Some exports that have a parent, like a type-class method or an
      -- associated type/data family, can still be imported as a top-level
      -- import.
      --
      -- Note that this is not the case for constructors, they must always be
      -- imported as part of their parent data type.

    | ImportViaParent T.Text T.Text
      -- ^ Import an export (first parameter) through its parent (second
      -- parameter).
      --
      -- import M (P(?))
      --
      -- @P@ and @?@ can be a data type and a constructor, a class and a method,
      -- a class and an associated type/data family, etc.
  deriving Show

importStyles :: IdentInfo -> NonEmpty ImportStyle
importStyles IdentInfo {parent, rendered, isDatacon}
  | Just p <- parent
    -- Constructors always have to be imported via their parent data type, but
    -- methods and associated type/data families can also be imported as
    -- top-level exports.
  = ImportViaParent rendered p :| [ImportTopLevel rendered | not isDatacon]
  | otherwise
  = ImportTopLevel rendered :| []

renderImportStyle :: ImportStyle -> T.Text
renderImportStyle (ImportTopLevel x) = x
renderImportStyle (ImportViaParent x p) = p <> "(" <> x <> ")"

-- | Find the first non-blank line before the first of (module name / imports / declarations).
-- Useful for inserting pragmas.
endOfModuleHeader :: ParsedModule -> Maybe T.Text -> Range
endOfModuleHeader pm contents =
    let mod = unLoc $ pm_parsed_source pm
        modNameLoc = getLoc <$> hsmodName mod
        firstImportLoc = getLoc <$> listToMaybe (hsmodImports mod)
        firstDeclLoc = getLoc <$> listToMaybe (hsmodDecls mod)
        line = fromMaybe 0 $ firstNonBlankBefore . _line . _start =<< srcSpanToRange =<<
            modNameLoc <|> firstImportLoc <|> firstDeclLoc
        firstNonBlankBefore n = (n -) . fromMaybe 0 . findIndex (not . T.null) . reverse . take n . T.lines <$> contents
        loc = Position line 0
     in Range loc loc
