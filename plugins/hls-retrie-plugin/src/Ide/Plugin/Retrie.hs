{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS -Wno-orphans #-}
#include "ghc-api-version.h"

module Ide.Plugin.Retrie (descriptor) where

import           Control.Concurrent.Extra             (readVar)
import           Control.Exception.Safe               (Exception (..),
                                                       SomeException, catch,
                                                       throwIO, try)
import           Control.Monad                        (forM, guard, unless)
import           Control.Monad.Extra                  (maybeM)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Control.Monad.Trans.Except           (ExceptT (..), runExceptT,
                                                       throwE)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..),
                                                       Value (Null),
                                                       genericParseJSON)
import qualified Data.Aeson                           as Aeson
import           Data.Bifunctor                       (Bifunctor (first),
                                                       second)
import           Data.Coerce
import           Data.Either                          (partitionEithers)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.HashSet                         as Set
import           Data.Hashable                        (unhashed)
import           Data.IORef.Extra                     (atomicModifyIORef'_,
                                                       newIORef, readIORef)
import           Data.List.Extra                      (find, nubOrdOn)
import           Data.Maybe
import           Data.String                          (IsString (fromString))
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import           Data.Typeable                        (Typeable)
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake           (ShakeExtras (knownTargetsVar),
                                                       toKnownFiles)
import           Development.IDE.GHC.Compat           (GenLocated (L), GhcRn,
                                                       HsBindLR (FunBind),
                                                       HsGroup (..),
                                                       HsImplicitBndrs (HsIB),
                                                       HsValBindsLR (..),
                                                       HsWildCardBndrs (HsWC),
                                                       HscEnv, IdP, LHsType,
                                                       LRuleDecls, LSig,
                                                       ModSummary (ModSummary, ms_hspp_buf, ms_mod),
                                                       NHsValBindsLR (..),
                                                       ParsedModule (..),
                                                       RuleDecl (HsRule),
                                                       RuleDecls (HsRules),
                                                       Sig (TypeSig),
                                                       SrcSpan (..),
                                                       TyClDecl (SynDecl),
                                                       TyClGroup (..), Type,
                                                       fun_id,
                                                       isTypeSynonymTyCon,
                                                       mi_fixities,
                                                       moduleNameString,
                                                       parseModule, rds_rules,
                                                       srcSpanFile,
                                                       synTyConRhs_maybe)
import           GHC.Generics                         (Generic)
import           GhcPlugins                           (Outputable,
                                                       SourceText (NoSourceText),
                                                       TyCon (tyConName),
                                                       hm_iface, isQual,
                                                       isQual_maybe,
                                                       nameModule_maybe,
                                                       nameRdrName, occNameFS,
                                                       occNameString,
                                                       rdrNameOcc,
                                                       typeEnvTyCons, unpackFS)
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server                  (LspM,
                                                       ProgressCancellable (Cancellable),
                                                       sendNotification,
                                                       sendRequest,
                                                       withIndefiniteProgress)
import           Language.LSP.Types                   as J
import           Retrie.CPP                           (CPP (NoCPP), parseCPP)
import           Retrie.ExactPrint                    (fix, relativiseApiAnns,
                                                       transformA, unsafeMkA)
import           Retrie.Fixity                        (mkFixityEnv)
import qualified Retrie.GHC                           as GHC
import           Retrie.Monad                         (addImports, apply,
                                                       getGroundTerms,
                                                       runRetrie)
import           Retrie.Options                       (defaultOptions,
                                                       getTargetFiles)
import qualified Retrie.Options                       as Retrie
import           Retrie.Replace                       (Change (..),
                                                       Replacement (..))
import           Retrie.Rewrites
import           Retrie.SYB                           (everything, listify, mkQ)
import           Retrie.Util                          (Verbosity (Loud))
import           StringBuffer                         (stringToStringBuffer)
import           System.Directory                     (makeAbsolute)
import           TcHsType
import           TcRnMonad
import           TcRnTypes
import           Unify

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction provider,
      pluginCommands = [retrieCommand]
    }

retrieCommandName :: T.Text
retrieCommandName = "retrieCommand"

retrieCommand :: PluginCommand IdeState
retrieCommand =
  PluginCommand (coerce retrieCommandName) "run the refactoring" runRetrieCmd

data Restriction
    = NoRestriction
    | OriginatingFile
    | RangeInOriginatingFile Range
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

rangeInRestriction :: Restriction -> Range -> Bool
rangeInRestriction (RangeInOriginatingFile (Range sRestrict eRestrict)) (Range sEdit eEdit) =
    sRestrict <= sEdit && sEdit <= eRestrict &&
    sRestrict <= eEdit && eEdit <= eRestrict
rangeInRestriction OriginatingFile _ = True
rangeInRestriction NoRestriction _ = True

restrictToOriginatingFile :: Restriction -> Bool
restrictToOriginatingFile OriginatingFile          = True
restrictToOriginatingFile RangeInOriginatingFile{} = True
restrictToOriginatingFile NoRestriction            = False

describeRestriction :: IsString p => Restriction -> p
describeRestriction NoRestriction              = ""
describeRestriction OriginatingFile            = " in current file"
-- TODO: Find a better description for this action
describeRestriction (RangeInOriginatingFile r) = " at site"

-- | Parameters for the runRetrie PluginCommand.
data RunRetrieParams = RunRetrieParams
  { description     :: T.Text,
    rewrites        :: [RewriteSpec],
    originatingFile :: Uri,
    restriction     :: Restriction
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

runRetrieCmd ::
  IdeState ->
  RunRetrieParams ->
  LspM c (Either ResponseError Value)
runRetrieCmd state RunRetrieParams{originatingFile = uri, ..} =
  withIndefiniteProgress description Cancellable $ do
    runMaybeT $ do
        nfp <- MaybeT $ return $ uriToNormalizedFilePath $ toNormalizedUri uri
        (session, _) <- MaybeT $ liftIO $
            runAction "Retrie.GhcSessionDeps" state $
                useWithStale GhcSessionDeps
                nfp
        (ms, binds, _, _, _, _) <- MaybeT $ liftIO $ runAction "Retrie.getBinds" state $ getBinds nfp
        let importRewrites = concatMap (extractImports ms binds) rewrites
        (errors, edits) <- liftIO $
            callRetrie
                state
                (hscEnv session)
                (map Right rewrites <> map Left importRewrites)
                nfp
                restriction
        unless (null errors) $
            lift $ sendNotification SWindowShowMessage $
                    ShowMessageParams MtWarning $
                    T.unlines $
                        "## Found errors during rewrite:" :
                        ["-" <> T.pack (show e) | e <- errors]
        lift $ sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edits) (\_ -> pure ())
        return ()
    return $ Right Null

extractImports :: ModSummary -> [HsBindLR GhcRn GhcRn] -> RewriteSpec -> [ImportSpec]
extractImports ModSummary{ms_mod} topLevelBinds (Unfold thing)
  | Just FunBind {fun_matches}
  <- find (\case FunBind{fun_id = L _ n} -> prettyPrint n == thing ; _ -> False) topLevelBinds
  , names <- listify p fun_matches
  =
    [ AddImport {..}
    | let ideclSource = False,
        name <- names,
        let r = nameRdrName name,
        let ideclQualifiedBool = isQual r,
        let ideclAsString = moduleNameString . fst <$> isQual_maybe r,
        let ideclThing = Just (IEVar $ occNameString $ rdrNameOcc r),
        Just ideclNameString <-
        [moduleNameString . GHC.moduleName <$> nameModule_maybe name]
    ]
    where
        p name = nameModule_maybe name /= Just ms_mod
-- TODO handle imports for all rewrites
extractImports _ _ _ = []

-------------------------------------------------------------------------------

provider :: PluginMethodHandler IdeState TextDocumentCodeAction
provider state plId (CodeActionParams _ _ (TextDocumentIdentifier uri) range ca) = response $ do
  let (J.CodeActionContext _diags _monly) = ca
      nuri = toNormalizedUri uri
  nfp <- handleMaybe "uri" $ uriToNormalizedFilePath nuri

  (ModSummary{ms_mod}, topLevelBinds, posMapping, hs_ruleds, hs_tyclds, signatures)
    <- handleMaybeM "typecheck" $ liftIO $ runAction "retrie" state $ getBinds nfp

  typeSynonyms <- liftIO $ runAction "retrie" state $ getTypeSynonyms nfp
  resolvedSignatures <- liftIO $ runAction "retrie" state $ catMaybes <$> (resolveSignatureType nfp `mapM` signatures)

  pos <- handleMaybe "pos" $ _start <$> fromCurrentRange posMapping range

  let rewrites =
        concatMap (suggestBindRewrites uri pos ms_mod) topLevelBinds
          ++ concatMap (suggestRuleRewrites uri pos ms_mod) hs_ruleds
          ++ [ r
               | TyClGroup {group_tyclds} <- hs_tyclds,
                 L l g <- group_tyclds,
                 pos `isInsideSrcSpan` l,
                 r <- suggestTypeRewrites uri ms_mod g
             ]
          ++ concatMap (suggestSignatureRewrites uri pos ms_mod typeSynonyms) resolvedSignatures

  commands <- lift $
    forM rewrites $ \(title, kind, params) -> liftIO $ do
      let c = mkLspCommand plId (coerce retrieCommandName) title (Just [toJSON params])
      return $ CodeAction title (Just kind) Nothing Nothing Nothing Nothing (Just c)

  return $ J.List [InR c | c <- commands]

getBinds :: NormalizedFilePath -> Action (Maybe (ModSummary, [HsBindLR GhcRn GhcRn], PositionMapping, [LRuleDecls GhcRn], [TyClGroup GhcRn], [LSig GhcRn]))
getBinds nfp = runMaybeT $ do
  (tm, posMapping) <- MaybeT $ useWithStale TypeCheck nfp
  -- we use the typechecked source instead of the parsed source
  -- to be able to extract module names from the Ids,
  -- so that we can include adding the required imports in the retrie command
  let rn = tmrRenamed tm
      ( HsGroup
          { hs_valds =
              XValBindsLR
                (NValBinds binds sigs :: NHsValBindsLR GHC.GhcRn),
            hs_ruleds,
            hs_tyclds
          },
        _,
        _,
        _
        ) = rn

      topLevelBinds =
        [ decl
          | (_, bagBinds) <- binds,
            L _ decl <- GHC.bagToList bagBinds
        ]

  return (tmrModSummary tm, topLevelBinds, posMapping, hs_ruleds, hs_tyclds, sigs)

suggestBindRewrites ::
  Uri ->
  Position ->
  GHC.Module ->
  HsBindLR GhcRn GhcRn ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestBindRewrites originatingFile pos ms_mod FunBind {fun_id = L l' rdrName}
  | pos `isInsideSrcSpan` l' =
    let pprName = prettyPrint rdrName
        pprNameText = T.pack pprName
        unfoldRewrite restriction =
            let rewrites = [Unfold (qualify ms_mod pprName)]
                description = "Unfold " <> pprNameText <> describeRestriction restriction
            in (description, CodeActionRefactorInline, RunRetrieParams {..})
        foldRewrite restriction =
          let rewrites = [Fold (qualify ms_mod pprName)]
              description = "Fold " <> pprNameText <> describeRestriction restriction
           in (description, CodeActionRefactorExtract, RunRetrieParams {..})
     in [unfoldRewrite NoRestriction, unfoldRewrite OriginatingFile, foldRewrite NoRestriction, foldRewrite OriginatingFile]
suggestBindRewrites _ _ _ _ = []

suggestTypeRewrites ::
  (Outputable (IdP pass)) =>
  Uri ->
  GHC.Module ->
  TyClDecl pass ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestTypeRewrites originatingFile ms_mod SynDecl {tcdLName = L _ rdrName} =
    let pprName = prettyPrint rdrName
        pprNameText = T.pack pprName
        unfoldRewrite restriction =
            let rewrites = [TypeForward (qualify ms_mod pprName)]
                description = "Unfold " <> pprNameText <> describeRestriction restriction
           in (description, CodeActionRefactorInline, RunRetrieParams {..})
        foldRewrite restriction =
          let rewrites = [TypeBackward (qualify ms_mod pprName)]
              description = "Fold " <> pprNameText <> describeRestriction restriction
           in (description, CodeActionRefactorExtract, RunRetrieParams {..})
     in [unfoldRewrite NoRestriction, unfoldRewrite OriginatingFile, foldRewrite NoRestriction, foldRewrite OriginatingFile]
suggestTypeRewrites _ _ _ = []

suggestRuleRewrites ::
  Uri ->
  Position ->
  GHC.Module ->
  LRuleDecls pass ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestRuleRewrites originatingFile pos ms_mod (L _ HsRules {rds_rules}) =
    concat
        [ [ forwardRewrite   ruleName OriginatingFile
          , forwardRewrite   ruleName NoRestriction
          , backwardsRewrite ruleName OriginatingFile
          , backwardsRewrite ruleName NoRestriction
          ]
        | L l r  <- rds_rules,
          pos `isInsideSrcSpan` l,
#if MIN_GHC_API_VERSION(8,8,0)
          let HsRule {rd_name = L _ (_, rn)} = r,
#else
          let HsRule _ (L _ (_,rn)) _ _ _ _ = r,
#endif
          let ruleName = unpackFS rn
      ]
  where
    forwardRewrite ruleName restriction =
        let rewrites = [RuleForward (qualify ms_mod ruleName)]
            description = "Apply rule " <> T.pack ruleName <> " forward" <>
                            describeRestriction restriction

        in ( description,
            CodeActionRefactor,
            RunRetrieParams {..}
            )
    backwardsRewrite ruleName restriction =
          let rewrites = [RuleBackward (qualify ms_mod ruleName)]
              description = "Apply rule " <> T.pack ruleName <> " backwards" <>
                              describeRestriction restriction
           in ( description,
                CodeActionRefactor,
                RunRetrieParams {..}
              )

suggestRuleRewrites _ _ _ _ = []

resolveSignatureType :: NormalizedFilePath -> LSig GhcRn -> Action (Maybe (GHC.Located Type))
resolveSignatureType nfp (L (RealSrcSpan span) (TypeSig _ _ (HsWC _ (HsIB _ hsTy)))) = do
    hscEnv <- hscEnv <$> use_ GhcSession nfp
    tcMod <- tmrTypechecked <$> use_ TypeCheck nfp
    (_, mType) <- liftIO $ initTcWithGbl hscEnv tcMod span $ tcLHsType hsTy
    case mType of
        Nothing      -> pure Nothing
        Just (ty, _) -> pure $ Just (L (RealSrcSpan span) ty)
resolveSignatureType _ _ = pure Nothing

getTypeSynonyms :: NormalizedFilePath -> Action [(GHC.Name, Type)]
getTypeSynonyms nfp = do
  tcg_type_env <- tcg_type_env . tmrTypechecked <$> use_ TypeCheck nfp
  return
    [ (tyConName tyCon, tyConRhs)
      | tyCon <- typeEnvTyCons tcg_type_env,
        isTypeSynonymTyCon tyCon,
        Just tyConRhs <- [synTyConRhs_maybe tyCon]
    ]

suggestSignatureRewrites ::
  Uri ->
  Position ->
  GHC.Module ->
  [(GHC.Name, Type)] ->
  GHC.Located Type ->
  [(T.Text, CodeActionKind, RunRetrieParams)]
suggestSignatureRewrites originatingFile pos ms_mod tySynsInScope (L (RealSrcSpan span) sigTy)
  | pos `isInsideSrcSpan` RealSrcSpan span =
      [ (description, CodeActionRefactor, RunRetrieParams { .. })
      | (tySynName, tySynRhs) <- tySynsInScope,
        tcMatchTyPart tySynRhs sigTy,
        let pprName = prettyPrint tySynName,
        let description = "Use " <> T.pack pprName <> " type synonym",
        let rewrites = [TypeBackward (qualify ms_mod pprName)],
        let restriction = RangeInOriginatingFile (realSrcSpanToRange span)
      ]
suggestSignatureRewrites _ _ _ _ _ = []

tcMatchTyPart ::
    -- | The "small" type, which should appear in the "big" type
    Type ->
    -- | The "big" type, that will be searched for instances of the "small" type
    Type ->
    Bool
tcMatchTyPart smallTy = everything (||) (mkQ False (isJust . tcMatchTy smallTy))

qualify :: GHC.Module -> String -> String
qualify ms_mod x = prettyPrint ms_mod <> "." <> x

-------------------------------------------------------------------------------
-- Retrie driving code

data CallRetrieError
  = CallRetrieInternalError String NormalizedFilePath
  | NoParse NormalizedFilePath
  | GHCParseError NormalizedFilePath String
  | NoTypeCheck NormalizedFilePath
  deriving (Eq, Typeable)

instance Show CallRetrieError where
  show (CallRetrieInternalError msg f) = msg <> " - " <> fromNormalizedFilePath f
  show (NoParse f) = "Cannot parse: " <> fromNormalizedFilePath f
  show (GHCParseError f m) = "Cannot parse " <> fromNormalizedFilePath f <> " : " <> m
  show (NoTypeCheck f) = "File does not typecheck: " <> fromNormalizedFilePath f

instance Exception CallRetrieError

callRetrie ::
  IdeState ->
  HscEnv ->
  [Either ImportSpec RewriteSpec] ->
  NormalizedFilePath ->
  Restriction ->
  IO ([CallRetrieError], WorkspaceEdit)
callRetrie state session rewrites origin restriction = do
  knownFiles <- toKnownFiles . unhashed <$> readVar (knownTargetsVar $ shakeExtras state)
  let reuseParsedModule f = do
        pm <-
          useOrFail "GetParsedModule" NoParse GetParsedModule f
        (fixities, pm) <- fixFixities f (fixAnns pm)
        return (fixities, pm)
      getCPPmodule t = do
        nt <- toNormalizedFilePath' <$> makeAbsolute t
        let getParsedModule f contents = do
              modSummary <- msrModSummary <$>
                useOrFail "GetModSummary" (CallRetrieInternalError "file not found") GetModSummary nt
              let ms' =
                    modSummary
                      { ms_hspp_buf =
                          Just (stringToStringBuffer contents)
                      }
              logPriority (ideLogger state) Info $ T.pack $ "Parsing module: " <> t
              parsed <-
                evalGhcEnv session (parseModule ms')
                  `catch` \e -> throwIO (GHCParseError nt (show @SomeException e))
              (fixities, parsed) <- fixFixities f (fixAnns parsed)
              return (fixities, parsed)

        contents <- do
          (_, mbContentsVFS) <-
            runAction "Retrie.GetFileContents" state $ getFileContents nt
          case mbContentsVFS of
            Just contents -> return contents
            Nothing       -> T.readFile (fromNormalizedFilePath nt)
        if any (T.isPrefixOf "#if" . T.toLower) (T.lines contents)
          then do
            fixitiesRef <- newIORef mempty
            let parseModule x = do
                  (fix, res) <- getParsedModule nt x
                  atomicModifyIORef'_ fixitiesRef (fix <>)
                  return res
            res <- parseCPP parseModule contents
            fixities <- readIORef fixitiesRef
            return (fixities, res)
          else do
            (fixities, pm) <- reuseParsedModule nt
            return (fixities, NoCPP pm)

      -- TODO cover all workspaceFolders
      target = "."

      retrieOptions :: Retrie.Options
      retrieOptions = (defaultOptions target)
        {Retrie.verbosity = Loud
        ,Retrie.targetFiles = map fromNormalizedFilePath $
            if restrictToOriginatingFile restriction
                then [origin]
                else Set.toList knownFiles
        }

      (theImports, theRewrites) = partitionEithers rewrites

      annotatedImports =
        unsafeMkA (map (GHC.noLoc . toImportDecl) theImports) mempty 0

  (originFixities, originParsedModule) <- reuseParsedModule origin
  retrie <-
    (\specs -> apply specs >> addImports annotatedImports)
      <$> parseRewriteSpecs
        (\_f -> return $ NoCPP originParsedModule)
        originFixities
        theRewrites

  targets <- getTargetFiles retrieOptions (getGroundTerms retrie)

  results <- forM targets $ \t -> runExceptT $ do
    (fixityEnv, cpp) <- ExceptT $ try $ getCPPmodule t
    -- TODO add the imports to the resulting edits
    (_user, ast, change@(Change _replacements _imports)) <-
      lift $ runRetrie fixityEnv retrie cpp
    return $ asTextEdits change

  let (errors :: [CallRetrieError], allReplacements) = partitionEithers results
      restrictedReplacements = fmap (filter (rangeInRestriction restriction . (\TextEdit{_range}->_range) . snd)) allReplacements
      editParams :: WorkspaceEdit
      editParams =
        WorkspaceEdit (Just $ asEditMap restrictedReplacements) Nothing

  return (errors, editParams)
  where
    useOrFail ::
      IdeRule r v =>
      String ->
      (NormalizedFilePath -> CallRetrieError) ->
      r ->
      NormalizedFilePath ->
      IO (RuleResult r)
    useOrFail lbl mkException rule f =
      useRule lbl state rule f >>= maybe (liftIO $ throwIO $ mkException f) return
    fixityEnvFromModIface modIface =
      mkFixityEnv
        [ (fs, (fs, fixity))
          | (n, fixity) <- mi_fixities modIface,
            let fs = occNameFS n
        ]
    fixFixities f pm = do
      HiFileResult {hirHomeMod} <-
        useOrFail "GetModIface" NoTypeCheck GetModIface f
      let fixities = fixityEnvFromModIface $ hm_iface hirHomeMod
      res <- transformA pm (fix fixities)
      return (fixities, res)
    fixAnns ParsedModule {..} =
      let ranns = relativiseApiAnns pm_parsed_source pm_annotations
       in unsafeMkA pm_parsed_source ranns 0

asEditMap :: [[(Uri, TextEdit)]] -> WorkspaceEditMap
asEditMap = coerce . HM.fromListWith (++) . concatMap (map (second pure))

asTextEdits :: Change -> [(Uri, TextEdit)]
asTextEdits NoChange = []
asTextEdits (Change reps _imports) =
  [ (filePathToUri spanLoc, edit)
    | Replacement {..} <- nubOrdOn replLocation reps,
      (RealSrcSpan rspan) <- [replLocation],
      let spanLoc = unpackFS $ srcSpanFile rspan,
      let edit = TextEdit (realSrcSpanToRange rspan) (T.pack replReplacement)
  ]

-------------------------------------------------------------------------------
-- Rule wrappers

_useRuleBlocking,
  _useRuleStale,
  useRule ::
    (IdeRule k v) =>
    String ->
    IdeState ->
    k ->
    NormalizedFilePath ->
    IO (Maybe (RuleResult k))
_useRuleBlocking label state rule f = runAction label state (use rule f)
_useRuleStale label state rule f =
  fmap fst
    <$> runIdeAction label (shakeExtras state) (useWithStaleFast rule f)

-- | Chosen approach for calling ghcide Shake rules
useRule label = _useRuleStale ("Retrie." <> label)

-------------------------------------------------------------------------------
-- Error handling combinators

handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
handleMaybeM msg act = maybeM (throwE msg) return $ lift act

response :: Monad m => ExceptT String m a -> m (Either ResponseError a)
response =
  fmap (first (\msg -> ResponseError InternalError (fromString msg) Nothing))
    . runExceptT

-------------------------------------------------------------------------------
-- Serialization wrappers and instances

deriving instance Eq RewriteSpec

deriving instance Show RewriteSpec

deriving instance Generic RewriteSpec

deriving instance FromJSON RewriteSpec

deriving instance ToJSON RewriteSpec

data QualName = QualName {qual, name :: String}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data IE name
  = IEVar name
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ImportSpec = AddImport
  { ideclNameString    :: String,
    ideclSource        :: Bool,
    ideclQualifiedBool :: Bool,
    ideclAsString      :: Maybe String,
    ideclThing         :: Maybe (IE String)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

toImportDecl :: ImportSpec -> GHC.ImportDecl GHC.GhcPs
toImportDecl AddImport {..} = GHC.ImportDecl {..}
  where
    toMod = GHC.noLoc . GHC.mkModuleName
    ideclName = toMod ideclNameString
    ideclPkgQual = Nothing
    ideclSafe = False
    ideclImplicit = False
    ideclHiding = Nothing
    ideclSourceSrc = NoSourceText
    ideclExt = GHC.noExtField
    ideclAs = toMod <$> ideclAsString
#if MIN_GHC_API_VERSION(8,10,0)
    ideclQualified = if ideclQualifiedBool then GHC.QualifiedPre else GHC.NotQualified
#else
    ideclQualified = ideclQualifiedBool
#endif
