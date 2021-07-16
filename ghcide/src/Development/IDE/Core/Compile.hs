-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP        #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | Based on https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/API.
--   Given a list of paths to find libraries, and a file to compile, produce a list of 'CoreModule' values.
module Development.IDE.Core.Compile
  ( TcModuleResult(..)
  , RunSimplifier(..)
  , compileModule
  , parseModule
  , typecheckModule
  , computePackageDeps
  , addRelativeImport
  , mkHiFileResultCompile
  , mkHiFileResultNoCompile
  , generateObjectCode
  , generateByteCode
  , generateHieAsts
  , writeAndIndexHieFile
  , indexHieFile
  , writeHiFile
  , getModSummaryFromImports
  , loadHieFile
  , loadInterface
  , loadModulesHome
  , setupFinderCache
  , getDocsBatch
  , lookupName
  ) where

import           Development.IDE.Core.Preprocessor
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Orphans       ()
import           Development.IDE.GHC.Util
import           Development.IDE.GHC.Warnings
import           Development.IDE.Spans.Common
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           Outputable                        hiding ((<>))

import           HieDb

import           Language.LSP.Types                (DiagnosticTag (..))

import           DriverPhases
import           DriverPipeline                    hiding (unP)
import           HscTypes
import           LoadIface                         (loadModuleInterface)

import           Lexer
import qualified Parser
#if MIN_VERSION_ghc(8,10,0)
import           Control.DeepSeq                   (force, rnf)
#else
import           Control.DeepSeq                   (rnf)
import           ErrUtils
#endif

import           Development.IDE.GHC.Compat        hiding (parseModule,
                                                    typecheckModule,
                                                    writeHieFile)
import qualified Development.IDE.GHC.Compat        as Compat
import qualified Development.IDE.GHC.Compat        as GHC
import           Finder
import           GhcMonad
import           GhcPlugins                        as GHC hiding (fst3, (<>))
import           Hooks
import           HscMain                           (hscDesugar, hscGenHardCode,
                                                    hscInteractive, hscSimplify,
                                                    hscTypecheckRename,
                                                    makeSimpleDetails)
import           MkIface
import           StringBuffer                      as SB
import           TcIface                           (typecheckIface)
import           TcRnMonad                         hiding (newUnique)
#if MIN_VERSION_ghc(9,0,1)
import           GHC.Builtin.Names
import           GHC.Iface.Recomp
import           GHC.Tc.Gen.Splice
import           GHC.Tc.Types.Evidence             (EvBind)
#else
import           PrelNames
import           TcSplice
#endif
import           TidyPgm

import           Bag
import           Control.Exception                 (evaluate)
import           Control.Exception.Safe
import           Control.Lens                      hiding (List)
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.Trans.Except
import           Data.Bifunctor                    (first, second)
import qualified Data.ByteString                   as BS
import qualified Data.DList                        as DL
import           Data.IORef
import           Data.List.Extra
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import qualified Data.Text                         as T
import           Data.Time                         (UTCTime, getCurrentTime)
import qualified GHC.LanguageExtensions            as LangExt
import           HeaderInfo
import           Linker                            (unload)
import           Maybes                            (orElse)
import           System.Directory
import           System.FilePath
import           System.IO.Extra                   (fixIO, newTempFileWithin)
import           TcEnv                             (tcLookup)

import           Control.Concurrent.Extra
import           Control.Concurrent.STM            hiding (orElse)
import           Data.Aeson                        (toJSON)
import           Data.Binary
import           Data.Coerce
import           Data.Functor
import qualified Data.HashMap.Strict               as HashMap
import           Data.Tuple.Extra                  (dupe)
import           Data.Unique
import           GHC.Fingerprint
import qualified Language.LSP.Server               as LSP
import qualified Language.LSP.Types                as LSP

-- | Given a string buffer, return the string (after preprocessing) and the 'ParsedModule'.
parseModule
    :: IdeOptions
    -> HscEnv
    -> FilePath
    -> ModSummary
    -> IO (IdeResult ParsedModule)
parseModule IdeOptions{..} env filename ms =
    fmap (either (, Nothing) id) $
    runExceptT $ do
        (diag, modu) <- parseFileContents env optPreprocessor filename ms
        return (diag, Just modu)


-- | Given a package identifier, what packages does it depend on
computePackageDeps
    :: HscEnv
    -> InstalledUnitId
    -> IO (Either [FileDiagnostic] [InstalledUnitId])
computePackageDeps env pkg = do
    let dflags = hsc_dflags env
    case oldLookupInstalledPackage dflags pkg of
        Nothing -> return $ Left [ideErrorText (toNormalizedFilePath' noFilePath) $
            T.pack $ "unknown package: " ++ show pkg]
        Just pkgInfo -> return $ Right $ unitDepends pkgInfo

typecheckModule :: IdeDefer
                -> HscEnv
                -> [Linkable] -- ^ linkables not to unload
                -> ParsedModule
                -> IO (IdeResult TcModuleResult)
typecheckModule (IdeDefer defer) hsc keep_lbls pm = do
    fmap (either (,Nothing) id) $
      catchSrcErrors (hsc_dflags hsc) "typecheck" $ do

        let modSummary = pm_mod_summary pm
            dflags = ms_hspp_opts modSummary

        modSummary' <- initPlugins hsc modSummary
        (warnings, tcm) <- withWarnings "typecheck" $ \tweak ->
            tcRnModule hsc keep_lbls $ demoteIfDefer pm{pm_mod_summary = tweak modSummary'}
        let errorPipeline = unDefer . hideDiag dflags . tagDiag
            diags = map errorPipeline warnings
            deferedError = any fst diags
        return (map snd diags, Just $ tcm{tmrDeferedError = deferedError})
    where
        demoteIfDefer = if defer then demoteTypeErrorsToWarnings else id

-- | Add a Hook to the DynFlags which captures and returns the
-- typechecked splices before they are run. This information
-- is used for hover.
captureSplices :: DynFlags -> (DynFlags -> IO a) -> IO (a, Splices)
captureSplices dflags k = do
  splice_ref <- newIORef mempty
  res <- k (dflags { hooks = addSpliceHook splice_ref (hooks dflags)})
  splices <- readIORef splice_ref
  return (res, splices)
  where
    addSpliceHook :: IORef Splices -> Hooks -> Hooks
    addSpliceHook var h = h { runMetaHook = Just (splice_hook (runMetaHook h) var) }

    splice_hook :: Maybe (MetaHook TcM) -> IORef Splices -> MetaHook TcM
    splice_hook (fromMaybe defaultRunMeta -> hook) var metaReq e = case metaReq of
        (MetaE f) -> do
            expr' <- metaRequestE hook e
            liftIO $ modifyIORef' var $ exprSplicesL %~ ((e, expr') :)
            pure $ f expr'
        (MetaP f) -> do
            pat' <- metaRequestP hook e
            liftIO $ modifyIORef' var $ patSplicesL %~ ((e, pat') :)
            pure $ f pat'
        (MetaT f) -> do
            type' <- metaRequestT hook e
            liftIO $ modifyIORef' var $ typeSplicesL %~ ((e, type') :)
            pure $ f type'
        (MetaD f) -> do
            decl' <- metaRequestD hook e
            liftIO $ modifyIORef' var $ declSplicesL %~ ((e, decl') :)
            pure $ f decl'
        (MetaAW f) -> do
            aw' <- metaRequestAW hook e
            liftIO $ modifyIORef' var $ awSplicesL %~ ((e, aw') :)
            pure $ f aw'


tcRnModule :: HscEnv -> [Linkable] -> ParsedModule -> IO TcModuleResult
tcRnModule hsc_env keep_lbls pmod = do
  let ms = pm_mod_summary pmod
      hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }

  unload hsc_env_tmp keep_lbls

  ((tc_gbl_env, mrn_info), splices)
      <- liftIO $ captureSplices (ms_hspp_opts ms) $ \dflags ->
             do  let hsc_env_tmp = hsc_env { hsc_dflags = dflags }
                 hscTypecheckRename hsc_env_tmp ms $
                          HsParsedModule { hpm_module = parsedSource pmod,
                                           hpm_src_files = pm_extra_src_files pmod,
                                           hpm_annotations = pm_annotations pmod }
  let rn_info = case mrn_info of
        Just x  -> x
        Nothing -> error "no renamed info tcRnModule"
  pure (TcModuleResult pmod rn_info tc_gbl_env splices False)

mkHiFileResultNoCompile :: HscEnv -> TcModuleResult -> IO HiFileResult
mkHiFileResultNoCompile session tcm = do
  let hsc_env_tmp = session { hsc_dflags = ms_hspp_opts ms }
      ms = pm_mod_summary $ tmrParsed tcm
      tcGblEnv = tmrTypechecked tcm
  details <- makeSimpleDetails hsc_env_tmp tcGblEnv
  sf <- finalSafeMode (ms_hspp_opts ms) tcGblEnv
#if MIN_VERSION_ghc(8,10,0)
  iface <- mkIfaceTc hsc_env_tmp sf details tcGblEnv
#else
  (iface, _) <- mkIfaceTc hsc_env_tmp Nothing sf details tcGblEnv
#endif
  mod_info <- mkDetailsFromIface session iface Nothing
  pure $! mkHiFileResult ms mod_info

mkHiFileResultCompile
    :: HscEnv
    -> TcModuleResult
    -> ModGuts
    -> LinkableType -- ^ use object code or byte code?
    -> IO (IdeResult HiFileResult)
mkHiFileResultCompile session' tcm simplified_guts ltype = catchErrs $ do
  let session = session' { hsc_dflags = ms_hspp_opts ms }
      ms = pm_mod_summary $ tmrParsed tcm
      tcGblEnv = tmrTypechecked tcm

  let genLinkable = case ltype of
        ObjectLinkable -> generateObjectCode
        BCOLinkable    -> generateByteCode

  (linkable, details, diags) <-
    if mg_hsc_src simplified_guts == HsBootFile
    then do
        -- give variables unique OccNames
        details <- mkBootModDetailsTc session tcGblEnv
        pure (Nothing, details, [])
    else do
        -- give variables unique OccNames
        (guts, details) <- tidyProgram session simplified_guts
        (diags, linkable) <- genLinkable session ms guts
        pure (linkable, details, diags)
#if MIN_VERSION_ghc(9,0,1)
  let !partial_iface = force (mkPartialIface session details simplified_guts)
  final_iface <- mkFullIface session partial_iface Nothing
#elif MIN_VERSION_ghc(8,10,0)
  let !partial_iface = force (mkPartialIface session details simplified_guts)
  final_iface <- mkFullIface session partial_iface
#else
  (final_iface,_) <- mkIface session Nothing details simplified_guts
#endif
  -- ignore the 'details' from 'tidyProgram', Matthew claims they leak memory
  -- instead, regenerate them using 'mkDetailsFromIface'
  mod_info <- mkDetailsFromIface session final_iface linkable
  pure (diags, Just $! mkHiFileResult ms mod_info)

  where
    dflags = hsc_dflags session'
    source = "compile"
    catchErrs x = x `catches`
      [ Handler $ return . (,Nothing) . diagFromGhcException source dflags
      , Handler $ return . (,Nothing) . diagFromString source DsError (noSpan "<internal>")
      . (("Error during " ++ T.unpack source) ++) . show @SomeException
      ]

initPlugins :: HscEnv -> ModSummary -> IO ModSummary
initPlugins session modSummary = do
    dflags <- liftIO $ initializePlugins session $ ms_hspp_opts modSummary
    return modSummary{ms_hspp_opts = dflags}

-- | Whether we should run the -O0 simplifier when generating core.
--
-- This is required for template Haskell to work but we disable this in DAML.
-- See #256
newtype RunSimplifier = RunSimplifier Bool

-- | Compile a single type-checked module to a 'CoreModule' value, or
-- provide errors.
compileModule
    :: RunSimplifier
    -> HscEnv
    -> ModSummary
    -> TcGblEnv
    -> IO (IdeResult ModGuts)
compileModule (RunSimplifier simplify) session ms tcg =
    fmap (either (, Nothing) (second Just)) $
        catchSrcErrors (hsc_dflags session) "compile" $ do
            (warnings,desugared_guts) <- withWarnings "compile" $ \tweak -> do
               let ms' = tweak ms
                   session' = session{ hsc_dflags = ms_hspp_opts ms'}
               desugar <- hscDesugar session' ms' tcg
               if simplify
               then do
                 plugins <- readIORef (tcg_th_coreplugins tcg)
                 hscSimplify session' plugins desugar
               else pure desugar
            return (map snd warnings, desugared_guts)

generateObjectCode :: HscEnv -> ModSummary -> CgGuts -> IO (IdeResult Linkable)
generateObjectCode session summary guts = do
    fmap (either (, Nothing) (second Just)) $
          catchSrcErrors (hsc_dflags session) "object" $ do
              let dot_o =  ml_obj_file (ms_location summary)
                  mod = ms_mod summary
                  fp = replaceExtension dot_o "s"
              createDirectoryIfMissing True (takeDirectory fp)
              (warnings, dot_o_fp) <-
                withWarnings "object" $ \_tweak -> do
                      let summary' = _tweak summary
#if MIN_VERSION_ghc(8,10,0)
                          target = defaultObjectTarget $ hsc_dflags session
#else
                          target = defaultObjectTarget $ targetPlatform $ hsc_dflags session
#endif
                          session' = session { hsc_dflags = updOptLevel 0 $ (ms_hspp_opts summary') { outputFile = Just dot_o , hscTarget = target}}
#if MIN_VERSION_ghc(9,0,1)
                      (outputFilename, _mStub, _foreign_files, _cinfos) <- hscGenHardCode session' guts
#else
                      (outputFilename, _mStub, _foreign_files) <- hscGenHardCode session' guts
#endif
#if MIN_VERSION_ghc(8,10,0)
                                (ms_location summary')
#else
                                summary'
#endif
                                fp
                      compileFile session' StopLn (outputFilename, Just (As False))
              let unlinked = DotO dot_o_fp
              -- Need time to be the modification time for recompilation checking
              t <- liftIO $ getModificationTime dot_o_fp
              let linkable = LM t mod [unlinked]

              pure (map snd warnings, linkable)

generateByteCode :: HscEnv -> ModSummary -> CgGuts -> IO (IdeResult Linkable)
generateByteCode hscEnv summary guts = do
    fmap (either (, Nothing) (second Just)) $
          catchSrcErrors (hsc_dflags hscEnv) "bytecode" $ do
              (warnings, (_, bytecode, sptEntries)) <-
                withWarnings "bytecode" $ \_tweak -> do
                      let summary' = _tweak summary
                          session = hscEnv { hsc_dflags = ms_hspp_opts summary' }
                      hscInteractive session guts
#if MIN_VERSION_ghc(8,10,0)
                                (ms_location summary')
#else
                                summary'
#endif
              let unlinked = BCOs bytecode sptEntries
              time <- liftIO getCurrentTime
              let linkable = LM time (ms_mod summary) [unlinked]

              pure (map snd warnings, linkable)

demoteTypeErrorsToWarnings :: ParsedModule -> ParsedModule
demoteTypeErrorsToWarnings =
  (update_pm_mod_summary . update_hspp_opts) demoteTEsToWarns where

  demoteTEsToWarns :: DynFlags -> DynFlags
  -- convert the errors into warnings, and also check the warnings are enabled
  demoteTEsToWarns = (`wopt_set` Opt_WarnDeferredTypeErrors)
                   . (`wopt_set` Opt_WarnTypedHoles)
                   . (`wopt_set` Opt_WarnDeferredOutOfScopeVariables)
                   . (`gopt_set` Opt_DeferTypeErrors)
                   . (`gopt_set` Opt_DeferTypedHoles)
                   . (`gopt_set` Opt_DeferOutOfScopeVariables)

update_hspp_opts :: (DynFlags -> DynFlags) -> ModSummary -> ModSummary
update_hspp_opts up ms = ms{ms_hspp_opts = up $ ms_hspp_opts ms}

update_pm_mod_summary :: (ModSummary -> ModSummary) -> ParsedModule -> ParsedModule
update_pm_mod_summary up pm =
  pm{pm_mod_summary = up $ pm_mod_summary pm}

unDefer :: (WarnReason, FileDiagnostic) -> (Bool, FileDiagnostic)
unDefer (Reason Opt_WarnDeferredTypeErrors         , fd) = (True, upgradeWarningToError fd)
unDefer (Reason Opt_WarnTypedHoles                 , fd) = (True, upgradeWarningToError fd)
unDefer (Reason Opt_WarnDeferredOutOfScopeVariables, fd) = (True, upgradeWarningToError fd)
unDefer ( _                                        , fd) = (False, fd)

upgradeWarningToError :: FileDiagnostic -> FileDiagnostic
upgradeWarningToError (nfp, sh, fd) =
  (nfp, sh, fd{_severity = Just DsError, _message = warn2err $ _message fd}) where
  warn2err :: T.Text -> T.Text
  warn2err = T.intercalate ": error:" . T.splitOn ": warning:"

hideDiag :: DynFlags -> (WarnReason, FileDiagnostic) -> (WarnReason, FileDiagnostic)
hideDiag originalFlags (Reason warning, (nfp, _sh, fd))
  | not (wopt warning originalFlags)
  = (Reason warning, (nfp, HideDiag, fd))
hideDiag _originalFlags t = t

-- | Warnings which lead to a diagnostic tag
unnecessaryDeprecationWarningFlags :: [WarningFlag]
unnecessaryDeprecationWarningFlags
  = [ Opt_WarnUnusedTopBinds
    , Opt_WarnUnusedLocalBinds
    , Opt_WarnUnusedPatternBinds
    , Opt_WarnUnusedImports
    , Opt_WarnUnusedMatches
    , Opt_WarnUnusedTypePatterns
    , Opt_WarnUnusedForalls
#if MIN_VERSION_ghc(8,10,0)
    , Opt_WarnUnusedRecordWildcards
#endif
    , Opt_WarnInaccessibleCode
    , Opt_WarnWarningsDeprecations
    ]

-- | Add a unnecessary/deprecated tag to the required diagnostics.
tagDiag :: (WarnReason, FileDiagnostic) -> (WarnReason, FileDiagnostic)
tagDiag (Reason warning, (nfp, sh, fd))
  | Just tag <- requiresTag warning
  = (Reason warning, (nfp, sh, fd { _tags = addTag tag (_tags fd) }))
  where
    requiresTag :: WarningFlag -> Maybe DiagnosticTag
    requiresTag Opt_WarnWarningsDeprecations
      = Just DtDeprecated
    requiresTag wflag  -- deprecation was already considered above
      | wflag `elem` unnecessaryDeprecationWarningFlags
      = Just DtUnnecessary
    requiresTag _ = Nothing
    addTag :: DiagnosticTag -> Maybe (List DiagnosticTag) -> Maybe (List DiagnosticTag)
    addTag t Nothing          = Just (List [t])
    addTag t (Just (List ts)) = Just (List (t : ts))
-- other diagnostics are left unaffected
tagDiag t = t

addRelativeImport :: NormalizedFilePath -> ModuleName -> DynFlags -> DynFlags
addRelativeImport fp modu dflags = dflags
    {importPaths = nubOrd $ maybeToList (moduleImportPath fp modu) ++ importPaths dflags}

atomicFileWrite :: FilePath -> (FilePath -> IO a) -> IO ()
atomicFileWrite targetPath write = do
  let dir = takeDirectory targetPath
  createDirectoryIfMissing True dir
  (tempFilePath, cleanUp) <- newTempFileWithin dir
  (write tempFilePath >> renameFile tempFilePath targetPath) `onException` cleanUp

generateHieAsts :: HscEnv -> TcModuleResult -> IO ([FileDiagnostic], Maybe (HieASTs Type))
generateHieAsts hscEnv tcm =
  handleGenerationErrors' dflags "extended interface generation" $ runHsc hscEnv $ do
    -- These varBinds use unitDataConId but it could be anything as the id name is not used
    -- during the hie file generation process. It's a workaround for the fact that the hie modules
    -- don't export an interface which allows for additional information to be added to hie files.
    let fake_splice_binds = listToBag (map (mkVarBind unitDataConId) (spliceExpresions $ tmrTopLevelSplices tcm))
        real_binds = tcg_binds $ tmrTypechecked tcm
#if MIN_VERSION_ghc(9,0,1)
        ts = tmrTypechecked tcm :: TcGblEnv
        top_ev_binds = tcg_ev_binds ts :: Bag EvBind
        insts = tcg_insts ts :: [ClsInst]
        tcs = tcg_tcs ts :: [TyCon]
    Just <$> GHC.enrichHie (fake_splice_binds `unionBags` real_binds) (tmrRenamed tcm) top_ev_binds insts tcs
#else
    Just <$> GHC.enrichHie (fake_splice_binds `unionBags` real_binds) (tmrRenamed tcm)
#endif
  where
    dflags = hsc_dflags hscEnv

spliceExpresions :: Splices -> [LHsExpr GhcTc]
spliceExpresions Splices{..} =
    DL.toList $ mconcat
        [ DL.fromList $ map fst exprSplices
        , DL.fromList $ map fst patSplices
        , DL.fromList $ map fst typeSplices
        , DL.fromList $ map fst declSplices
        , DL.fromList $ map fst awSplices
        ]

-- | In addition to indexing the `.hie` file, this function is responsible for
-- maintaining the 'IndexQueue' state and notfiying the user about indexing
-- progress.
--
-- We maintain a record of all pending index operations in the 'indexPending'
-- TVar.
-- When 'indexHieFile' is called, it must check to ensure that the file hasn't
-- already be queued up for indexing. If it has, then we can just skip it
--
-- Otherwise, we record the current file as pending and write an indexing
-- operation to the queue
--
-- When the indexing operation is picked up and executed by the worker thread,
-- the first thing it does is ensure that a newer index for the same file hasn't
-- been scheduled by looking at 'indexPending'. If a newer index has been
-- scheduled, we can safely skip this one
--
-- Otherwise, we start or continue a progress reporting session, telling it
-- about progress so far and the current file we are attempting to index. Then
-- we can go ahead and call in to hiedb to actually do the indexing operation
--
-- Once this completes, we have to update the 'IndexQueue' state. First, we
-- must remove the just indexed file from 'indexPending' Then we check if
-- 'indexPending' is now empty. In that case, we end the progress session and
-- report the total number of file indexed. We also set the 'indexCompleted'
-- TVar to 0 in order to set it up for a fresh indexing session. Otherwise, we
-- can just increment the 'indexCompleted' TVar and exit.
--
indexHieFile :: ShakeExtras -> ModSummary -> NormalizedFilePath -> Fingerprint -> Compat.HieFile -> IO ()
indexHieFile se mod_summary srcPath !hash hf = do
 IdeOptions{optProgressStyle} <- getIdeOptionsIO se
 atomically $ do
  pending <- readTVar indexPending
  case HashMap.lookup srcPath pending of
    Just pendingHash | pendingHash == hash -> pure () -- An index is already scheduled
    _ -> do
      -- hiedb doesn't use the Haskell src, so we clear it to avoid unnecessarily keeping it around
      let !hf' = hf{hie_hs_src = mempty}
      modifyTVar' indexPending $ HashMap.insert srcPath hash
      writeTQueue indexQueue $ \db -> do
        -- We are now in the worker thread
        -- Check if a newer index of this file has been scheduled, and if so skip this one
        newerScheduled <- atomically $ do
          pending <- readTVar indexPending
          pure $ case HashMap.lookup srcPath pending of
            Nothing          -> False
            -- If the hash in the pending list doesn't match the current hash, then skip
            Just pendingHash -> pendingHash /= hash
        unless newerScheduled $ do
          pre optProgressStyle
          addRefsFromLoaded db targetPath (RealFile $ fromNormalizedFilePath srcPath) hash hf'
          post
  where
    mod_location    = ms_location mod_summary
    targetPath      = Compat.ml_hie_file mod_location
    HieDbWriter{..} = hiedbWriter se

    -- Get a progress token to report progress and update it for the current file
    pre style = do
      tok <- modifyVar indexProgressToken $ fmap dupe . \case
        x@(Just _) -> pure x
        -- Create a token if we don't already have one
        Nothing -> do
          case lspEnv se of
            Nothing -> pure Nothing
            Just env -> LSP.runLspT env $ do
              u <- LSP.ProgressTextToken . T.pack . show . hashUnique <$> liftIO newUnique
              -- TODO: Wait for the progress create response to use the token
              _ <- LSP.sendRequest LSP.SWindowWorkDoneProgressCreate (LSP.WorkDoneProgressCreateParams u) (const $ pure ())
              LSP.sendNotification LSP.SProgress $ LSP.ProgressParams u $
                LSP.Begin $ LSP.WorkDoneProgressBeginParams
                  { _title = "Indexing"
                  , _cancellable = Nothing
                  , _message = Nothing
                  , _percentage = Nothing
                  }
              pure (Just u)

      (!done, !remaining) <- atomically $ do
        done <- readTVar indexCompleted
        remaining <- HashMap.size <$> readTVar indexPending
        pure (done, remaining)

      whenJust (lspEnv se) $ \env -> whenJust tok $ \tok -> LSP.runLspT env $
        LSP.sendNotification LSP.SProgress $ LSP.ProgressParams tok $
          LSP.Report $
            case style of
                Percentage -> LSP.WorkDoneProgressReportParams
                    { _cancellable = Nothing
                    , _message = Nothing
                    , _percentage = Just (100 * fromIntegral done / fromIntegral (done + remaining) )
                    }
                Explicit -> LSP.WorkDoneProgressReportParams
                    { _cancellable = Nothing
                    , _message = Just $
                        T.pack " (" <> T.pack (show done) <> "/" <> T.pack (show $ done + remaining) <> ")..."
                    , _percentage = Nothing
                    }
                NoProgress -> LSP.WorkDoneProgressReportParams
                  { _cancellable = Nothing
                  , _message = Nothing
                  , _percentage = Nothing
                  }

    -- Report the progress once we are done indexing this file
    post = do
      mdone <- atomically $ do
        -- Remove current element from pending
        pending <- stateTVar indexPending $
          dupe . HashMap.update (\pendingHash -> guard (pendingHash /= hash) $> pendingHash) srcPath
        modifyTVar' indexCompleted (+1)
        -- If we are done, report and reset completed
        whenMaybe (HashMap.null pending) $
          swapTVar indexCompleted 0
      whenJust (lspEnv se) $ \env -> LSP.runLspT env $
        when (coerce $ ideTesting se) $
          LSP.sendNotification (LSP.SCustomMethod "ghcide/reference/ready") $
            toJSON $ fromNormalizedFilePath srcPath
      whenJust mdone $ \done ->
        modifyVar_ indexProgressToken $ \tok -> do
          whenJust (lspEnv se) $ \env -> LSP.runLspT env $
            whenJust tok $ \tok ->
              LSP.sendNotification LSP.SProgress $ LSP.ProgressParams tok $
                LSP.End $ LSP.WorkDoneProgressEndParams
                  { _message = Just $ "Finished indexing " <> T.pack (show done) <> " files"
                  }
          -- We are done with the current indexing cycle, so destroy the token
          pure Nothing

writeAndIndexHieFile :: HscEnv -> ShakeExtras -> ModSummary -> NormalizedFilePath -> [GHC.AvailInfo] -> HieASTs Type -> BS.ByteString -> IO [FileDiagnostic]
writeAndIndexHieFile hscEnv se mod_summary srcPath exports ast source =
  handleGenerationErrors dflags "extended interface write/compression" $ do
    hf <- runHsc hscEnv $
      GHC.mkHieFile' mod_summary exports ast source
    atomicFileWrite targetPath $ flip GHC.writeHieFile hf
    hash <- getFileHash targetPath
    indexHieFile se mod_summary srcPath hash hf
  where
    dflags       = hsc_dflags hscEnv
    mod_location = ms_location mod_summary
    targetPath   = Compat.ml_hie_file mod_location

writeHiFile :: HscEnv -> HiFileResult -> IO [FileDiagnostic]
writeHiFile hscEnv tc =
  handleGenerationErrors dflags "interface write" $ do
    atomicFileWrite targetPath $ \fp ->
      writeIfaceFile dflags fp modIface
  where
    modIface = hm_iface $ hirHomeMod tc
    targetPath = ml_hi_file $ ms_location $ hirModSummary tc
    dflags = hsc_dflags hscEnv

handleGenerationErrors :: DynFlags -> T.Text -> IO () -> IO [FileDiagnostic]
handleGenerationErrors dflags source action =
  action >> return [] `catches`
    [ Handler $ return . diagFromGhcException source dflags
    , Handler $ return . diagFromString source DsError (noSpan "<internal>")
    . (("Error during " ++ T.unpack source) ++) . show @SomeException
    ]

handleGenerationErrors' :: DynFlags -> T.Text -> IO (Maybe a) -> IO ([FileDiagnostic], Maybe a)
handleGenerationErrors' dflags source action =
  fmap ([],) action `catches`
    [ Handler $ return . (,Nothing) . diagFromGhcException source dflags
    , Handler $ return . (,Nothing) . diagFromString source DsError (noSpan "<internal>")
    . (("Error during " ++ T.unpack source) ++) . show @SomeException
    ]

-- | Initialise the finder cache, dependencies should be topologically
-- sorted.
setupFinderCache :: [ModSummary] -> HscEnv -> IO HscEnv
setupFinderCache mss session = do

    -- Make modules available for others that import them,
    -- by putting them in the finder cache.
    let ims  = map (installedModule (thisInstalledUnitId $ hsc_dflags session) . moduleName . ms_mod) mss
        ifrs = zipWith (\ms -> InstalledFound (ms_location ms)) mss ims
    -- set the target and module graph in the session
        graph = mkModuleGraph mss

    -- We have to create a new IORef here instead of modifying the existing IORef as
    -- it is shared between concurrent compilations.
    prevFinderCache <- readIORef $ hsc_FC session
    let newFinderCache =
            foldl'
                (\fc (im, ifr) -> GHC.extendInstalledModuleEnv fc im ifr) prevFinderCache
                $ zip ims ifrs
    newFinderCacheVar <- newIORef $! newFinderCache

    pure $ session { hsc_FC = newFinderCacheVar, hsc_mod_graph = graph }


-- | Load modules, quickly. Input doesn't need to be desugared.
-- A module must be loaded before dependent modules can be typechecked.
-- This variant of loadModuleHome will *never* cause recompilation, it just
-- modifies the session.
-- The order modules are loaded is important when there are hs-boot files.
-- In particular you should make sure to load the .hs version of a file after the
-- .hs-boot version.
loadModulesHome
    :: [HomeModInfo]
    -> HscEnv
    -> HscEnv
loadModulesHome mod_infos e =
    e { hsc_HPT = addListToHpt (hsc_HPT e) [(mod_name x, x) | x <- mod_infos]
      , hsc_type_env_var = Nothing }
    where
      mod_name = moduleName . mi_module . hm_iface

withBootSuffix :: HscSource -> ModLocation -> ModLocation
withBootSuffix HsBootFile = addBootSuffixLocnOut
withBootSuffix _          = id

-- | Given a buffer, env and filepath, produce a module summary by parsing only the imports.
--   Runs preprocessors as needed.
getModSummaryFromImports
  :: HscEnv
  -> FilePath
  -> UTCTime
  -> Maybe SB.StringBuffer
  -> ExceptT [FileDiagnostic] IO ModSummaryResult
getModSummaryFromImports env fp modTime contents = do
    (contents, opts, dflags) <- preprocessor env fp contents

    -- The warns will hopefully be reported when we actually parse the module
    (_warns, L main_loc hsmod) <- parseHeader dflags fp contents

    -- Copied from `HeaderInfo.getImports`, but we also need to keep the parsed imports
    let mb_mod = hsmodName hsmod
        imps = hsmodImports hsmod

        mod = fmap unLoc mb_mod `orElse` mAIN_NAME

        (src_idecls, ord_idecls) = partition ((== IsBoot) . ideclSource.unLoc) imps

        -- GHC.Prim doesn't exist physically, so don't go looking for it.
        ordinary_imps = filter ((/= moduleName gHC_PRIM) . unLoc
                                . ideclName . unLoc)
                               ord_idecls

        implicit_prelude = xopt LangExt.ImplicitPrelude dflags
        implicit_imports = mkPrelImports mod main_loc
                                         implicit_prelude imps
        convImport (L _ i) = (fmap sl_fs (ideclPkgQual i)
                                         , ideclName i)

        srcImports = map convImport src_idecls
        textualImports = map convImport (implicit_imports ++ ordinary_imps)

        msrImports = implicit_imports ++ imps

    -- Force bits that might keep the string buffer and DynFlags alive unnecessarily
    liftIO $ evaluate $ rnf srcImports
    liftIO $ evaluate $ rnf textualImports

    modLoc <- liftIO $ mkHomeModLocation dflags mod fp

    let modl = mkModule (thisPackage dflags) mod
        sourceType = if "-boot" `isSuffixOf` takeExtension fp then HsBootFile else HsSrcFile
        msrModSummary =
            ModSummary
                { ms_mod          = modl
#if MIN_VERSION_ghc(8,8,0)
                , ms_hie_date     = Nothing
#endif
                , ms_hs_date      = modTime
                , ms_hsc_src      = sourceType
                -- The contents are used by the GetModSummary rule
                , ms_hspp_buf     = Just contents
                , ms_hspp_file    = fp
                , ms_hspp_opts    = dflags
                , ms_iface_date   = Nothing
                , ms_location     = withBootSuffix sourceType modLoc
                , ms_obj_date     = Nothing
                , ms_parsed_mod   = Nothing
                , ms_srcimps      = srcImports
                , ms_textual_imps = textualImports
                }

    msrFingerprint <- liftIO $ computeFingerprint opts msrModSummary
    return ModSummaryResult{..}
    where
        -- Compute a fingerprint from the contents of `ModSummary`,
        -- eliding the timestamps, the preprocessed source and other non relevant fields
        computeFingerprint opts ModSummary{..} = do
            fingerPrintImports <- fingerprintFromPut $ do
                  put $ uniq $ moduleNameFS $ moduleName ms_mod
                  forM_ (ms_srcimps ++ ms_textual_imps) $ \(mb_p, m) -> do
                    put $ uniq $ moduleNameFS $ unLoc m
                    whenJust mb_p $ put . uniq
            return $! fingerprintFingerprints $
                    [ fingerprintString fp
                    , fingerPrintImports
                    ] ++ map fingerprintString opts


-- | Parse only the module header
parseHeader
       :: Monad m
       => DynFlags -- ^ flags to use
       -> FilePath  -- ^ the filename (for source locations)
       -> SB.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
#if MIN_VERSION_ghc(9,0,1)
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], Located(HsModule))
#else
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], Located(HsModule GhcPs))
#endif
parseHeader dflags filename contents = do
   let loc  = mkRealSrcLoc (mkFastString filename) 1 1
   case unP Parser.parseHeader (mkPState dflags contents loc) of
#if MIN_VERSION_ghc(8,10,0)
     PFailed pst ->
        throwE $ diagFromErrMsgs "parser" dflags $ getErrorMessages pst dflags
#else
     PFailed _ locErr msgErr ->
        throwE $ diagFromErrMsg "parser" dflags $ mkPlainErrMsg dflags locErr msgErr
#endif
     POk pst rdr_module -> do
        let (warns, errs) = getMessages pst dflags
        -- Just because we got a `POk`, it doesn't mean there
        -- weren't errors! To clarify, the GHC parser
        -- distinguishes between fatal and non-fatal
        -- errors. Non-fatal errors are the sort that don't
        -- prevent parsing from continuing (that is, a parse
        -- tree can still be produced despite the error so that
        -- further errors/warnings can be collected). Fatal
        -- errors are those from which a parse tree just can't
        -- be produced.
        unless (null errs) $
            throwE $ diagFromErrMsgs "parser" dflags errs

        let warnings = diagFromErrMsgs "parser" dflags warns
        return (warnings, rdr_module)

-- | Given a buffer, flags, and file path, produce a
-- parsed module (or errors) and any parse warnings. Does not run any preprocessors
-- ModSummary must contain the (preprocessed) contents of the buffer
parseFileContents
       :: HscEnv
       -> (GHC.ParsedSource -> IdePreprocessedSource)
       -> FilePath  -- ^ the filename (for source locations)
       -> ModSummary
       -> ExceptT [FileDiagnostic] IO ([FileDiagnostic], ParsedModule)
parseFileContents env customPreprocessor filename ms = do
   let loc  = mkRealSrcLoc (mkFastString filename) 1 1
       dflags = ms_hspp_opts ms
       contents = fromJust $ ms_hspp_buf ms
   case unP Parser.parseModule (mkPState dflags contents loc) of
#if MIN_VERSION_ghc(8,10,0)
     PFailed pst -> throwE $ diagFromErrMsgs "parser" dflags $ getErrorMessages pst dflags
#else
     PFailed _ locErr msgErr ->
      throwE $ diagFromErrMsg "parser" dflags $ mkPlainErrMsg dflags locErr msgErr
#endif
     POk pst rdr_module ->
         let hpm_annotations :: ApiAnns
             hpm_annotations =
#if MIN_VERSION_ghc(9,0,1)
               -- Copied from GHC.Driver.Main
               ApiAnns {
                      apiAnnItems = Map.fromListWith (++) $ annotations pst,
                      apiAnnEofPos = eof_pos pst,
                      apiAnnComments = Map.fromList (annotations_comments pst),
                      apiAnnRogueComments = comment_q pst
                   }
#else
               (Map.fromListWith (++) $ annotations pst,
                 Map.fromList ((noSrcSpan,comment_q pst)
                                  :annotations_comments pst))
#endif
             (warns, errs) = getMessages pst dflags
         in
           do
               -- Just because we got a `POk`, it doesn't mean there
               -- weren't errors! To clarify, the GHC parser
               -- distinguishes between fatal and non-fatal
               -- errors. Non-fatal errors are the sort that don't
               -- prevent parsing from continuing (that is, a parse
               -- tree can still be produced despite the error so that
               -- further errors/warnings can be collected). Fatal
               -- errors are those from which a parse tree just can't
               -- be produced.
               unless (null errs) $
                 throwE $ diagFromErrMsgs "parser" dflags errs

               -- Ok, we got here. It's safe to continue.
               let IdePreprocessedSource preproc_warns errs parsed = customPreprocessor rdr_module

               unless (null errs) $
                  throwE $ diagFromStrings "parser" DsError errs

               let preproc_warnings = diagFromStrings "parser" DsWarning preproc_warns
               parsed' <- liftIO $ applyPluginsParsedResultAction env dflags ms hpm_annotations parsed

               -- To get the list of extra source files, we take the list
               -- that the parser gave us,
               --   - eliminate files beginning with '<'.  gcc likes to use
               --     pseudo-filenames like "<built-in>" and "<command-line>"
               --   - normalise them (eliminate differences between ./f and f)
               --   - filter out the preprocessed source file
               --   - filter out anything beginning with tmpdir
               --   - remove duplicates
               --   - filter out the .hs/.lhs source filename if we have one
               --
               let n_hspp  = normalise filename
                   srcs0 = nubOrd $ filter (not . (tmpDir dflags `isPrefixOf`))
                                  $ filter (/= n_hspp)
                                  $ map normalise
                                  $ filter (not . isPrefixOf "<")
                                  $ map unpackFS
                                  $ srcfiles pst
                   srcs1 = case ml_hs_file (ms_location ms) of
                             Just f  -> filter (/= normalise f) srcs0
                             Nothing -> srcs0

               -- sometimes we see source files from earlier
               -- preprocessing stages that cannot be found, so just
               -- filter them out:
               srcs2 <- liftIO $ filterM doesFileExist srcs1

               let pm =
                     ParsedModule {
                         pm_mod_summary = ms
                       , pm_parsed_source = parsed'
                       , pm_extra_src_files = srcs2
                       , pm_annotations = hpm_annotations
                      }
                   warnings = diagFromErrMsgs "parser" dflags warns
               pure (warnings ++ preproc_warnings, pm)

loadHieFile :: Compat.NameCacheUpdater -> FilePath -> IO GHC.HieFile
loadHieFile ncu f = do
  GHC.hie_file_result <$> GHC.readHieFile ncu f

-- | Retuns an up-to-date module interface, regenerating if needed.
--   Assumes file exists.
--   Requires the 'HscEnv' to be set up with dependencies
loadInterface
  :: MonadIO m => HscEnv
  -> ModSummary
  -> SourceModified
  -> Maybe LinkableType
  -> (Maybe LinkableType -> m ([FileDiagnostic], Maybe HiFileResult)) -- ^ Action to regenerate an interface
  -> m ([FileDiagnostic], Maybe HiFileResult)
loadInterface session ms sourceMod linkableNeeded regen = do
    let sessionWithMsDynFlags = session{hsc_dflags = ms_hspp_opts ms}
    res <- liftIO $ checkOldIface sessionWithMsDynFlags ms sourceMod Nothing
    case res of
          (UpToDate, Just iface)
            -- If the module used TH splices when it was last
            -- compiled, then the recompilation check is not
            -- accurate enough (https://gitlab.haskell.org/ghc/ghc/-/issues/481)
            -- and we must ignore
            -- it.  However, if the module is stable (none of
            -- the modules it depends on, directly or
            -- indirectly, changed), then we *can* skip
            -- recompilation. This is why the SourceModified
            -- type contains SourceUnmodifiedAndStable, and
            -- it's pretty important: otherwise ghc --make
            -- would always recompile TH modules, even if
            -- nothing at all has changed. Stability is just
            -- the same check that make is doing for us in
            -- one-shot mode.
            | not (mi_used_th iface) || SourceUnmodifiedAndStable == sourceMod
            -> do
             linkable <- case linkableNeeded of
               Just ObjectLinkable -> liftIO $ findObjectLinkableMaybe (ms_mod ms) (ms_location ms)
               _ -> pure Nothing

             -- We don't need to regenerate if the object is up do date, or we don't need one
             let objUpToDate = isNothing linkableNeeded || case linkable of
                   Nothing                -> False
                   Just (LM obj_time _ _) -> obj_time > ms_hs_date ms
             if objUpToDate
             then do
               hmi <- liftIO $ mkDetailsFromIface sessionWithMsDynFlags iface linkable
               return ([], Just $ mkHiFileResult ms hmi)
             else regen linkableNeeded
          (_reason, _) -> regen linkableNeeded

mkDetailsFromIface :: HscEnv -> ModIface -> Maybe Linkable -> IO HomeModInfo
mkDetailsFromIface session iface linkable = do
  details <- liftIO $ fixIO $ \details -> do
    let hsc' = session { hsc_HPT = addToHpt (hsc_HPT session) (moduleName $ mi_module iface) (HomeModInfo iface details linkable) }
    initIfaceLoad hsc' (typecheckIface iface)
  return (HomeModInfo iface details linkable)

-- | Non-interactive, batch version of 'InteractiveEval.getDocs'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
getDocsBatch
  :: HscEnv
  -> Module  -- ^ a moudle where the names are in scope
  -> [Name]
  -> IO [Either String (Maybe HsDocString, Map.Map Int HsDocString)]
getDocsBatch hsc_env _mod _names = do
    ((_warns,errs), res) <- initTc hsc_env HsSrcFile False _mod fakeSpan $ forM _names $ \name ->
        case nameModule_maybe name of
            Nothing -> return (Left $ NameHasNoModule name)
            Just mod -> do
             ModIface { mi_doc_hdr = mb_doc_hdr
                      , mi_decl_docs = DeclDocMap dmap
                      , mi_arg_docs = ArgDocMap amap
                      } <- loadModuleInterface "getModuleInterface" mod
             if isNothing mb_doc_hdr && Map.null dmap && Map.null amap
               then pure (Left (NoDocsInIface mod $ compiled name))
               else pure (Right ( Map.lookup name dmap
                                , Map.findWithDefault Map.empty name amap))
    case res of
        Just x  -> return $ map (first $ T.unpack . showGhc) x
        Nothing -> throwErrors errs
  where
    throwErrors = liftIO . throwIO . mkSrcErr
    compiled n =
      -- TODO: Find a more direct indicator.
      case nameSrcLoc n of
        RealSrcLoc {}   -> False
        UnhelpfulLoc {} -> True

fakeSpan :: RealSrcSpan
fakeSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<ghcide>") 1 1

-- | Non-interactive, batch version of 'InteractiveEval.lookupNames'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
lookupName :: HscEnv
           -> Module -- ^ A module where the Names are in scope
           -> Name
           -> IO (Maybe TyThing)
lookupName hsc_env mod name = do
    (_messages, res) <- initTc hsc_env HsSrcFile False mod fakeSpan $ do
        tcthing <- tcLookup name
        case tcthing of
            AGlobal thing    -> return thing
            ATcId{tct_id=id} -> return (AnId id)
            _                -> panic "tcRnLookupName'"
    return res
