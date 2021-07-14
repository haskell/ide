{-# LANGUAGE TypeFamilies #-}

{-|
The logic for setting up a ghcide session by tapping into hie-bios.
-}
module Development.IDE.Session
  (SessionLoadingOptions(..)
  ,CacheDirs(..)
  ,loadSession
  ,loadSessionWithOptions
  ,setInitialDynFlags
  ,getHieDbLoc
  ,runWithDb
  ) where

-- Unfortunately, we cannot use loadSession with ghc-lib since hie-bios uses
-- the real GHC library and the types are incompatible. Furthermore, when
-- building with ghc-lib we need to make this Haskell agnostic, so no hie-bios!

import           Control.Concurrent.Async
import           Control.Concurrent.Strict
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1                     as H
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Char8                as B
import           Data.Default
import           Data.Either.Extra
import           Data.Function
import qualified Data.HashMap.Strict                  as HM
import           Data.Hashable
import           Data.IORef
import           Data.List
import qualified Data.Map.Strict                      as Map
import           Data.Maybe
import qualified Data.Text                            as T
import           Data.Time.Clock
import           Data.Version
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat           hiding (Target,
                                                       TargetFile, TargetModule)
import qualified Development.IDE.GHC.Compat           as GHC
import           Development.IDE.GHC.Util
import           Development.IDE.Graph                (Action)
import           Development.IDE.Session.VersionCheck
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq       (HscEnvEq, newHscEnvEq,
                                                       newHscEnvEqPreserveImportPaths)
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.IDE.Types.Options
import           GHC.Check
import qualified HIE.Bios                             as HieBios
import           HIE.Bios.Environment                 hiding (getCacheDir)
import           HIE.Bios.Types
import           Hie.Implicit.Cradle                  (loadImplicitHieCradle)
import           Language.LSP.Server
import           Language.LSP.Types
import           System.Directory
import qualified System.Directory.Extra               as IO
import           System.FilePath
import           System.IO
import           System.Info

import           Control.Applicative                  (Alternative ((<|>)))
import           Control.Exception                    (evaluate)
import           Data.Void
import           GHCi
import           HscTypes                             (hsc_IC, hsc_NC,
                                                       hsc_dflags, ic_dflags)
import           Linker
import           Module
import           NameCache

import           Control.Concurrent.STM               (atomically)
import           Control.Concurrent.STM.TQueue
import qualified Data.HashSet                         as Set
import           Database.SQLite.Simple
import           HieDb.Create
import           HieDb.Types
import           HieDb.Utils
import           Ide.Types                            (dynFlagsModifyGlobal)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

-- | Bump this version number when making changes to the format of the data stored in hiedb
hiedbDataVersion :: String
hiedbDataVersion = "1"

data CacheDirs = CacheDirs
  { hiCacheDir, hieCacheDir, oCacheDir :: Maybe FilePath}

data SessionLoadingOptions = SessionLoadingOptions
  { findCradle             :: FilePath -> IO (Maybe FilePath)
  -- | Load the cradle with an optional 'hie.yaml' location.
  -- If a 'hie.yaml' is given, use it to load the cradle.
  -- Otherwise, use the provided project root directory to determine the cradle type.
  , loadCradle             :: Maybe FilePath -> FilePath -> IO (HieBios.Cradle Void)
  -- | Given the project name and a set of command line flags,
  --   return the path for storing generated GHC artifacts,
  --   or 'Nothing' to respect the cradle setting
  , getCacheDirs           :: String -> [String] -> IO CacheDirs
  -- | Return the GHC lib dir to use for the 'unsafeGlobalDynFlags'
  , getInitialGhcLibDir    :: FilePath -> IO (Maybe LibDir)
  , fakeUid                :: GHC.InstalledUnitId
    -- ^ unit id used to tag the internal component built by ghcide
    --   To reuse external interface files the unit ids must match,
    --   thus make sure to build them with `--this-unit-id` set to the
    --   same value as the ghcide fake uid
  }

instance Default SessionLoadingOptions where
    def = SessionLoadingOptions
        {findCradle = HieBios.findCradle
        ,loadCradle = loadWithImplicitCradle
        ,getCacheDirs = getCacheDirsDefault
        ,getInitialGhcLibDir = getInitialGhcLibDirDefault
        ,fakeUid = GHC.toInstalledUnitId (GHC.stringToUnit "main")
        }

-- | Find the cradle for a given 'hie.yaml' configuration.
--
-- If a 'hie.yaml' is given, the cradle is read from the config.
--  If this config does not comply to the "hie.yaml"
-- specification, an error is raised.
--
-- If no location for "hie.yaml" is provided, the implicit config is used
-- using the provided root directory for discovering the project.
-- The implicit config uses different heuristics to determine the type
-- of the project that may or may not be accurate.
loadWithImplicitCradle :: Maybe FilePath
                          -- ^ Optional 'hie.yaml' location. Will be used if given.
                          -> FilePath
                          -- ^ Root directory of the project. Required as a fallback
                          -- if no 'hie.yaml' location is given.
                          -> IO (HieBios.Cradle Void)
loadWithImplicitCradle mHieYaml rootDir = do
  case mHieYaml of
    Just yaml -> HieBios.loadCradle yaml
    Nothing   -> loadImplicitHieCradle $ addTrailingPathSeparator rootDir

getInitialGhcLibDirDefault :: FilePath -> IO (Maybe LibDir)
getInitialGhcLibDirDefault rootDir = do
  hieYaml <- findCradle def rootDir
  cradle <- loadCradle def hieYaml rootDir
  hPutStrLn stderr $ "setInitialDynFlags cradle: " ++ show cradle
  libDirRes <- getRuntimeGhcLibDir cradle
  case libDirRes of
      CradleSuccess libdir -> pure $ Just $ LibDir libdir
      CradleFail err -> do
        hPutStrLn stderr $ "Couldn't load cradle for libdir: " ++ show (err,rootDir,hieYaml,cradle)
        pure Nothing
      CradleNone -> do
        hPutStrLn stderr "Couldn't load cradle (CradleNone)"
        pure Nothing

-- | Sets `unsafeGlobalDynFlags` on using the hie-bios cradle and returns the GHC libdir
setInitialDynFlags :: FilePath -> SessionLoadingOptions -> IO (Maybe LibDir)
setInitialDynFlags rootDir SessionLoadingOptions{..} = do
  libdir <- getInitialGhcLibDir rootDir
  dynFlags <- mapM dynFlagsForPrinting libdir
  mapM_ setUnsafeGlobalDynFlags dynFlags
  pure libdir

-- | Wraps `withHieDb` to provide a database connection for reading, and a `HieWriterChan` for
-- writing. Actions are picked off one by one from the `HieWriterChan` and executed in serial
-- by a worker thread using a dedicated database connection.
-- This is done in order to serialize writes to the database, or else SQLite becomes unhappy
runWithDb :: FilePath -> (HieDb -> IndexQueue -> IO ()) -> IO ()
runWithDb fp k = do
  -- Delete the database if it has an incompatible schema version
  withHieDb fp (const $ pure ())
    `catch` \IncompatibleSchemaVersion{} -> removeFile fp
  withHieDb fp $ \writedb -> do
    initConn writedb
    chan <- newTQueueIO
    withAsync (writerThread writedb chan) $ \_ -> do
      withHieDb fp (flip k chan)
  where
    writerThread db chan = do
      -- Clear the index of any files that might have been deleted since the last run
      deleteMissingRealFiles db
      _ <- garbageCollectTypeNames db
      forever $ do
        k <- atomically $ readTQueue chan
        k db
          `catch` \e@SQLError{} -> do
            hPutStrLn stderr $ "SQLite error in worker, ignoring: " ++ show e
          `catchAny` \e -> do
            hPutStrLn stderr $ "Uncaught error in database worker, ignoring: " ++ show e


getHieDbLoc :: FilePath -> IO FilePath
getHieDbLoc dir = do
  let db = intercalate "-" [dirHash, takeBaseName dir, ghcVersionStr, hiedbDataVersion] <.> "hiedb"
      dirHash = B.unpack $ B16.encode $ H.hash $ B.pack dir
  cDir <- IO.getXdgDirectory IO.XdgCache cacheDir
  createDirectoryIfMissing True cDir
  pure (cDir </> db)

-- | Given a root directory, return a Shake 'Action' which setups an
-- 'IdeGhcSession' given a file.
-- Some of the many things this does:
--
-- * Find the cradle for the file
-- * Get the session options,
-- * Get the GHC lib directory
-- * Make sure the GHC compiletime and runtime versions match
-- * Restart the Shake session
--
-- This is the key function which implements multi-component support. All
-- components mapping to the same hie.yaml file are mapped to the same
-- HscEnv which is updated as new components are discovered.
loadSession :: FilePath -> IO (Action IdeGhcSession)
loadSession = loadSessionWithOptions def

loadSessionWithOptions :: SessionLoadingOptions -> FilePath -> IO (Action IdeGhcSession)
loadSessionWithOptions SessionLoadingOptions{..} dir = do
  -- Mapping from hie.yaml file to HscEnv, one per hie.yaml file
  hscEnvs <- newVar Map.empty :: IO (Var HieMap)
  -- Mapping from a Filepath to HscEnv
  fileToFlags <- newVar Map.empty :: IO (Var FlagsMap)
  -- Mapping from a Filepath to its 'hie.yaml' location.
  -- Should hold the same Filepaths as 'fileToFlags', otherwise
  -- they are inconsistent. So, everywhere you modify 'fileToFlags',
  -- you have to modify 'filesMap' as well.
  filesMap <- newVar HM.empty :: IO (Var FilesMap)
  -- Version of the mappings above
  version <- newVar 0
  let returnWithVersion :: (FilePath -> IO (IdeResult HscEnvEq, [FilePath]))
                  -> Action IdeGhcSession
      returnWithVersion fun = (IdeGhcSession fun) <$> liftIO (readVar version)
  -- This caches the mapping from Mod.hs -> hie.yaml
  cradleLoc <- liftIO $ memoIO $ \v -> do
      res <- findCradle v
      -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
      -- try and normalise that
      -- e.g. see https://github.com/haskell/ghcide/issues/126
      res' <- traverse makeAbsolute res
      return $ normalise <$> res'

  dummyAs <- async $ return (error "Uninitialised")
  runningCradle <- newVar dummyAs :: IO (Var (Async (IdeResult HscEnvEq,[FilePath])))

  return $ do
    extras@ShakeExtras{logger, restartShakeSession, ideNc, knownTargetsVar, lspEnv
                      } <- getShakeExtras
    let invalidateShakeCache = do
            void $ modifyVar' version succ
            recordDirtyKeys extras GhcSessionIO [emptyFilePath]

    IdeOptions{ optTesting = IdeTesting optTesting
              , optCheckProject = getCheckProject
              , optModifyDynFlags
              , optExtensions
              } <- getIdeOptions

        -- populate the knownTargetsVar with all the
        -- files in the project so that `knownFiles` can learn about them and
        -- we can generate a complete module graph
    let extendKnownTargets newTargets = do
          knownTargets <- forM newTargets $ \TargetDetails{..} ->
            case targetTarget of
              TargetFile f -> pure (targetTarget, [f])
              TargetModule _ -> do
                found <- filterM (IO.doesFileExist . fromNormalizedFilePath) targetLocations
                return (targetTarget, found)
          modifyVarIO' knownTargetsVar $ traverseHashed $ \known -> do
            let known' = HM.unionWith (<>) known $ HM.fromList $ map (second Set.fromList) knownTargets
            when (known /= known') $
                logDebug logger $ "Known files updated: " <>
                    T.pack(show $ (HM.map . Set.map) fromNormalizedFilePath known')
            pure known'

    -- Create a new HscEnv from a hieYaml root and a set of options
    -- If the hieYaml file already has an HscEnv, the new component is
    -- combined with the components in the old HscEnv into a new HscEnv
    -- which contains the union.
    let packageSetup :: (Maybe FilePath, NormalizedFilePath, NonEmpty ComponentOptions, FilePath)
                     -> IO [(HscEnv, ComponentInfo, [ComponentInfo])]
        packageSetup (hieYaml, cfp, allOpts, libDir) = concatForM (NE.toList allOpts) $ \opts -> do
          -- Parse DynFlags for the newly discovered component
          hscEnv <- emptyHscEnv ideNc libDir
          (df, targets) <- evalGhcEnv hscEnv $
              first (dynFlagsModifyGlobal optModifyDynFlags) <$> setOptions opts (hsc_dflags hscEnv)
          let deps = componentDependencies opts ++ maybeToList hieYaml
          dep_info <- getDependencyInfo deps
          -- Now lookup to see whether we are combining with an existing HscEnv
          -- or making a new one. The lookup returns the HscEnv and a list of
          -- information about other components loaded into the HscEnv
          -- (unitId, DynFlag, Targets)
          r <- modifyVar hscEnvs $ \m -> do
              -- Just deps if there's already an HscEnv
              -- Nothing is it's the first time we are making an HscEnv
              let oldDeps = Map.lookup hieYaml m
              let -- Add the raw information about this component to the list
                  -- We will modify the unitId and DynFlags used for
                  -- compilation but these are the true source of
                  -- information.
                  new_deps = RawComponentInfo (thisInstalledUnitId df) df targets cfp opts dep_info
                                : maybe [] snd oldDeps
                  -- Get all the unit-ids for things in this component
                  inplace = map rawComponentUnitId new_deps

              new_deps' <- forM new_deps $ \RawComponentInfo{..} -> do
                  -- Remove all inplace dependencies from package flags for
                  -- components in this HscEnv
                  let (df2, uids) = removeInplacePackages fakeUid inplace rawComponentDynFlags
                  let prefix = show rawComponentUnitId
                  -- See Note [Avoiding bad interface files]
                  let hscComponents = sort $ map show uids
                      cacheDirOpts = hscComponents ++ componentOptions opts
                  cacheDirs <- liftIO $ getCacheDirs prefix cacheDirOpts
                  processed_df <- setCacheDirs logger cacheDirs df2
                  -- The final component information, mostly the same but the DynFlags don't
                  -- contain any packages which are also loaded
                  -- into the same component.
                  pure $ ComponentInfo rawComponentUnitId
                                       processed_df
                                       uids
                                       rawComponentTargets
                                       rawComponentFP
                                       rawComponentCOptions
                                       rawComponentDependencyInfo
              -- Make a new HscEnv, we have to recompile everything from
              -- scratch again (for now)
              -- It's important to keep the same NameCache though for reasons
              -- that I do not fully understand
              logInfo logger (T.pack ("Making new HscEnv" ++ show inplace))
              hscEnv <- emptyHscEnv ideNc libDir
              newHscEnv <-
                -- Add the options for the current component to the HscEnv
                evalGhcEnv hscEnv $ do
                  _ <- setSessionDynFlags df
                  getSession

              -- Modify the map so the hieYaml now maps to the newly created
              -- HscEnv
              -- Returns
              -- . the new HscEnv so it can be used to modify the
              --   FilePath -> HscEnv map (fileToFlags)
              -- . The information for the new component which caused this cache miss
              -- . The modified information (without -inplace flags) for
              --   existing packages
              pure (Map.insert hieYaml (newHscEnv, new_deps) m, (newHscEnv, head new_deps', tail new_deps'))
          pure [r]


    let session :: (Maybe FilePath, NormalizedFilePath, NonEmpty ComponentOptions, FilePath)
                -> IO (IdeResult HscEnvEq,[FilePath])
        session args@(hieYaml, cfp, _opts, _libDir) = do
          setupInfos <- packageSetup args -- (hscEnvs, new, old_deps)

          -- Whenever we spin up a session on Linux, dynamically load libm.so.6
          -- in. We need this in case the binary is statically linked, in which
          -- case the interactive session will fail when trying to load
          -- ghc-prim, which happens whenever Template Haskell is being
          -- evaluated or haskell-language-server's eval plugin tries to run
          -- some code. If the binary is dynamically linked, then this will have
          -- no effect.
          -- See https://github.com/haskell/haskell-language-server/issues/221
          forM_ setupInfos $ \(hscEnv, new, old_deps) -> do
            when (os == "linux") $ do
              initObjLinker hscEnv
              res <- loadDLL hscEnv "libm.so.6"
              case res of
                Nothing -> pure ()
                Just err -> hPutStrLn stderr $
                  "Error dynamically loading libm.so.6:\n" <> err

            -- Make a map from unit-id to DynFlags, this is used when trying to
            -- resolve imports. (especially PackageImports)
            let uids = map (\ci -> (componentUnitId ci, componentDynFlags ci)) (new : old_deps)

            -- For each component, now make a new HscEnvEq which contains the
            -- HscEnv for the hie.yaml file but the DynFlags for that component

            -- New HscEnv for the component in question, returns the new HscEnvEq and
            -- a mapping from FilePath to the newly created HscEnvEq.
            let new_cache = newComponentCache logger optExtensions hieYaml cfp hscEnv uids
            (cs, _res) <- new_cache new
            -- Modified cache targets for everything else in the hie.yaml file
            -- which now uses the same EPS and so on
            cached_targets <- concatMapM (fmap fst . new_cache) old_deps

            let all_targets = cs ++ cached_targets

            void $ modifyVar' fileToFlags $
                Map.insert hieYaml (HM.fromList (concatMap toFlagsMap all_targets))
            void $ modifyVar' filesMap $
                flip HM.union (HM.fromList (zip (map fst $ concatMap toFlagsMap all_targets) (repeat hieYaml)))

            void $ extendKnownTargets all_targets

            -- Invalidate all the existing GhcSession build nodes by restarting the Shake session
            invalidateShakeCache
            restartShakeSession []

            -- Typecheck all files in the project on startup
            checkProject <- getCheckProject
            unless (null cs || not checkProject) $ do
                  cfps' <- liftIO $ filterM (IO.doesFileExist . fromNormalizedFilePath) (concatMap targetLocations cs)
                  void $ shakeEnqueue extras $ mkDelayedAction "InitialLoad" Debug $ void $ do
                      mmt <- uses GetModificationTime cfps'
                      let cs_exist = catMaybes (zipWith (<$) cfps' mmt)
                      modIfaces <- uses GetModIface cs_exist
                      -- update exports map
                      extras <- getShakeExtras
                      let !exportsMap' = createExportsMap $ mapMaybe (fmap hirModIface) modIfaces
                      liftIO $ modifyVar_ (exportsMap extras) $ evaluate . (exportsMap' <>)

          yamlMap <- readVar fileToFlags
          let (hscEnvEq, depInfo) = (yamlMap Map.! hieYaml) HM.! cfp
          pure (hscEnvEq, Map.keys depInfo)

            -- Let's be insanely hacky!
            -- Go through all new components and find the one that the given fp most likely
            -- belongs to.
            -- Since all ComponentInfo's carry a proof for which file caused their creation,
            -- and add this file to their targets, we need to do some post-processing.
            -- It is an implementation detail that the first known target is the normalized
            -- filepath of the proof file.
            -- Let's strip it away and let's see whether the targets still contain 'cfp'!
            -- If they do, it must be the home component of cfp.
            -- If we find it, remove it as a target from all other 'HscEnvEq' to avoid recompilation
            -- issues.

    let consultCradle :: Maybe FilePath -> FilePath -> IO (IdeResult HscEnvEq,[FilePath])
        consultCradle hieYaml cfp = do
           lfp <- flip makeRelative cfp <$> getCurrentDirectory
           logInfo logger $ T.pack ("Consulting the cradle for " <> show lfp)

           when (isNothing hieYaml) $
             logWarning logger $ implicitCradleWarning lfp

           cradle <- loadCradle hieYaml dir
           lfp <- flip makeRelative cfp <$> getCurrentDirectory

           when optTesting $ mRunLspT lspEnv $
            sendNotification (SCustomMethod "ghcide/cradle/loaded") (toJSON cfp)

           -- Display a user friendly progress message here: They probably don't know what a cradle is
           let progMsg = "Setting up " <> T.pack (takeBaseName (cradleRootDir cradle))
                         <> " (for " <> T.pack lfp <> ")"
           eopts <- mRunLspTCallback lspEnv (withIndefiniteProgress progMsg NotCancellable) $
              cradleToOptsAndLibDir cradle cfp

           logDebug logger $ T.pack ("Session loading result: " <> show eopts)
           case eopts of
             -- The cradle gave us some options so get to work turning them
             -- into and HscEnv.
             Right (opts, libDir) -> do
               installationCheck <- ghcVersionChecker libDir
               case installationCheck of
                 InstallationNotFound{..} ->
                     error $ "GHC installation not found in libdir: " <> libdir
                 InstallationMismatch{..} ->
                     return (([renderPackageSetupException cfp GhcVersionMismatch{..}], Nothing),[])
                 InstallationChecked _compileTime _ghcLibCheck ->
                     session (hieYaml, toNormalizedFilePath' cfp, opts, libDir)
             -- Failure case, either a cradle error or the none cradle
             Left err -> do
               dep_info <- getDependencyInfo (maybeToList hieYaml)
               let ncfp = toNormalizedFilePath' cfp
               let res = (map (renderCradleError ncfp) err, Nothing)
               void $ modifyVar' fileToFlags $
                    Map.insertWith HM.union hieYaml (HM.singleton ncfp (res, dep_info))
               void $ modifyVar' filesMap $ HM.insert ncfp hieYaml
               return (res, maybe [] pure hieYaml ++ concatMap cradleErrorDependencies err)

    -- This caches the mapping from hie.yaml + Mod.hs -> [String]
    -- Returns the Ghc session and the cradle dependencies
    let sessionOpts :: (Maybe FilePath, FilePath)
                    -> IO (IdeResult HscEnvEq, [FilePath])
        sessionOpts (hieYaml, file) = do
          v <- fromMaybe HM.empty . Map.lookup hieYaml <$> readVar fileToFlags
          cfp <- canonicalizePath file
          case HM.lookup (toNormalizedFilePath' cfp) v of
            Just (opts, old_di) -> do
              deps_ok <- checkDependencyInfo old_di
              if not deps_ok
                then do
                  -- If the dependencies are out of date then clear both caches and start
                  -- again.
                  modifyVar_ fileToFlags (const (return Map.empty))
                  -- Keep the same name cache
                  modifyVar_ hscEnvs (return . Map.adjust (\(h, _) -> (h, [])) hieYaml )
                  consultCradle hieYaml cfp
                else return (opts, Map.keys old_di)
            Nothing -> consultCradle hieYaml cfp

    -- The main function which gets options for a file. We only want one of these running
    -- at a time. Therefore the IORef contains the currently running cradle, if we try
    -- to get some more options then we wait for the currently running action to finish
    -- before attempting to do so.
    let getOptions :: FilePath -> IO (IdeResult HscEnvEq, [FilePath])
        getOptions file = do
            ncfp <- toNormalizedFilePath' <$> canonicalizePath file
            cachedHieYamlLocation <- HM.lookup ncfp <$> readVar filesMap
            hieYaml <- cradleLoc file
            sessionOpts (join cachedHieYamlLocation <|> hieYaml, file) `catch` \e ->
                return (([renderPackageSetupException file e], Nothing), maybe [] pure hieYaml)

    returnWithVersion $ \file -> do
      opts <- liftIO $ join $ mask_ $ modifyVar runningCradle $ \as -> do
        -- If the cradle is not finished, then wait for it to finish.
        void $ wait as
        as <- async $ getOptions file
        return (as, wait as)
      pure opts

-- | Run the specific cradle on a specific FilePath via hie-bios.
-- This then builds dependencies or whatever based on the cradle, gets the
-- GHC options/dynflags needed for the session and the GHC library directory

cradleToOptsAndLibDir :: Show a => Cradle a -> FilePath
                      -> IO (Either [CradleError] (NonEmpty ComponentOptions, FilePath))
cradleToOptsAndLibDir cradle file = do
    -- Start off by getting the session options
    let showLine s = hPutStrLn stderr ("> " ++ s)
    hPutStrLn stderr $ "Output from setting up the cradle " <> show cradle
    cradleRes <- runCradle (cradleOptsProg cradle) showLine file
    case cradleRes of
        CradleSuccess r -> do
            -- Now get the GHC lib dir
            libDirRes <- getRuntimeGhcLibDir cradle
            case libDirRes of
                -- This is the successful path
                CradleSuccess libDir -> pure (Right (r, libDir))
                CradleFail err       -> return (Left [err])
                -- For the None cradle perhaps we still want to report an Info
                -- message about the fact that the file is being ignored.
                CradleNone           -> return (Left [])

        CradleFail err -> return (Left [err])
        -- Same here
        CradleNone -> return (Left [])

emptyHscEnv :: IORef NameCache -> FilePath -> IO HscEnv
emptyHscEnv nc libDir = do
    env <- runGhc (Just libDir) getSession
    when (ghcVersion < GHC90) $
        -- This causes ghc9 to crash with the error:
        -- Couldn't find a target code interpreter. Try with -fexternal-interpreter
        initDynLinker env
    pure $ setNameCache nc env{ hsc_dflags = (hsc_dflags env){useUnicode = True } }

data TargetDetails = TargetDetails
  {
      targetTarget    :: !Target,
      targetEnv       :: !(IdeResult HscEnvEq),
      targetDepends   :: !DependencyInfo,
      targetLocations :: ![NormalizedFilePath]
  }

fromTargetId :: [FilePath]          -- ^ import paths
             -> [String]            -- ^ extensions to consider
             -> TargetId
             -> IdeResult HscEnvEq
             -> DependencyInfo
             -> IO [TargetDetails]
-- For a target module we consider all the import paths
fromTargetId is exts (GHC.TargetModule mod) env dep = do
    let fps = [i </> moduleNameSlashes mod -<.> ext <> boot
              | ext <- exts
              , i <- is
              , boot <- ["", "-boot"]
              ]
    locs <- mapM (fmap toNormalizedFilePath' . canonicalizePath) fps
    return [TargetDetails (TargetModule mod) env dep locs]
-- For a 'TargetFile' we consider all the possible module names
fromTargetId _ _ (GHC.TargetFile f _) env deps = do
    nf <- toNormalizedFilePath' <$> canonicalizePath f
    return [TargetDetails (TargetFile nf) env deps [nf]]

toFlagsMap :: TargetDetails -> [(NormalizedFilePath, (IdeResult HscEnvEq, DependencyInfo))]
toFlagsMap TargetDetails{..} =
    [ (l, (targetEnv, targetDepends)) | l <-  targetLocations]


setNameCache :: IORef NameCache -> HscEnv -> HscEnv
setNameCache nc hsc = hsc { hsc_NC = nc }

-- | Create a mapping from FilePaths to HscEnvEqs
newComponentCache
         :: Logger
         -> [String]       -- File extensions to consider
         -> Maybe FilePath -- Path to cradle
         -> NormalizedFilePath -- Path to file that caused the creation of this component
         -> HscEnv
         -> [(InstalledUnitId, DynFlags)]
         -> ComponentInfo
         -> IO ( [TargetDetails], (IdeResult HscEnvEq, DependencyInfo))
newComponentCache logger exts cradlePath _cfp hsc_env uids ci = do
    let df = componentDynFlags ci
    let hscEnv' = hsc_env { hsc_dflags = df
                          , hsc_IC = (hsc_IC hsc_env) { ic_dflags = df } }

    let newFunc = maybe newHscEnvEqPreserveImportPaths newHscEnvEq cradlePath
    henv <- newFunc hscEnv' uids
    let targetEnv = ([], Just henv)
        targetDepends = componentDependencyInfo ci
        res = (targetEnv, targetDepends)
    logDebug logger ("New Component Cache HscEnvEq: " <> T.pack (show res))

    let mk t = fromTargetId (importPaths df) exts (targetId t) targetEnv targetDepends
    ctargets <- concatMapM mk (componentTargets ci)

    -- A special target for the file which caused this wonderful
    -- component to be created. In case the cradle doesn't list all the targets for
    -- the component, in which case things will be horribly broken anyway.
    -- Otherwise, we will immediately attempt to reload this module which
    -- causes an infinite loop and high CPU usage.
    -- let special_target = TargetDetails (TargetFile cfp) targetEnv targetDepends [componentFP ci]
    -- return (special_target:ctargets, res)
    return (ctargets, res)

{- Note [Avoiding bad interface files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Originally, we set the cache directory for the various components once
on the first occurrence of the component.
This works fine if these components have no references to each other,
but you have components that depend on each other, the interface files are
updated for each component.
After restarting the session and only opening the component that depended
on the other, suddenly the interface files of this component are stale.
However, from the point of view of `ghcide`, they do not look stale,
thus, not regenerated and the IDE shows weird errors such as:
```
typecheckIface
Declaration for Rep_ClientRunFlags
Axiom branches Rep_ClientRunFlags:
  Failed to load interface for ‘Distribution.Simple.Flag’
  Use -v to see a list of the files searched for.
```
and
```
expectJust checkFamInstConsistency
CallStack (from HasCallStack):
  error, called at compiler\\utils\\Maybes.hs:55:27 in ghc:Maybes
  expectJust, called at compiler\\typecheck\\FamInst.hs:461:30 in ghc:FamInst
```

To mitigate this, we set the cache directory for each component dependent
on the components of the current `HscEnv`, additionally to the component options
of the respective components.
Assume two components, c1, c2, where c2 depends on c1, and the options of the
respective components are co1, co2.
If we want to load component c2, followed by c1, we set the cache directory for
each component in this way:

  * Load component c2
    * (Cache Directory State)
        - name of c2 + co2
  * Load component c1
    * (Cache Directory State)
        - name of c2 + name of c1 + co2
        - name of c2 + name of c1 + co1

Overall, we created three cache directories. If we opened c1 first, then we
create a fourth cache directory.
This makes sure that interface files are always correctly updated.

Since this causes a lot of recompilation, we only update the cache-directory,
if the dependencies of a component have really changed.
E.g. when you load two executables, they can not depend on each other. They
should be filtered out, such that we dont have to re-compile everything.
-}

-- | Set the cache-directory based on the ComponentOptions and a list of
-- internal packages.
-- For the exact reason, see Note [Avoiding bad interface files].
setCacheDirs :: MonadIO m => Logger -> CacheDirs -> DynFlags -> m DynFlags
setCacheDirs logger CacheDirs{..} dflags = do
    liftIO $ logInfo logger $ "Using interface files cache dir: " <> T.pack (fromMaybe cacheDir hiCacheDir)
    pure $ dflags
          & maybe id setHiDir hiCacheDir
          & maybe id setHieDir hieCacheDir
          & maybe id setODir oCacheDir


renderCradleError :: NormalizedFilePath -> CradleError -> FileDiagnostic
renderCradleError nfp (CradleError _ _ec t) =
  ideErrorWithSource (Just "cradle") (Just DsError) nfp (T.unlines (map T.pack t))

-- See Note [Multi Cradle Dependency Info]
type DependencyInfo = Map.Map FilePath (Maybe UTCTime)
type HieMap = Map.Map (Maybe FilePath) (HscEnv, [RawComponentInfo])
-- | Maps a "hie.yaml" location to all its Target Filepaths and options.
type FlagsMap = Map.Map (Maybe FilePath) (HM.HashMap NormalizedFilePath (IdeResult HscEnvEq, DependencyInfo))
-- | Maps a Filepath to its respective "hie.yaml" location.
-- It aims to be the reverse of 'FlagsMap'.
type FilesMap = HM.HashMap NormalizedFilePath (Maybe FilePath)

-- This is pristine information about a component
data RawComponentInfo = RawComponentInfo
  { rawComponentUnitId         :: InstalledUnitId
  -- | Unprocessed DynFlags. Contains inplace packages such as libraries.
  -- We do not want to use them unprocessed.
  , rawComponentDynFlags       :: DynFlags
  -- | All targets of this components.
  , rawComponentTargets        :: [GHC.Target]
  -- | Filepath which caused the creation of this component
  , rawComponentFP             :: NormalizedFilePath
  -- | Component Options used to load the component.
  , rawComponentCOptions       :: ComponentOptions
  -- | Maps cradle dependencies, such as `stack.yaml`, or `.cabal` file
  -- to last modification time. See Note [Multi Cradle Dependency Info].
  , rawComponentDependencyInfo :: DependencyInfo
  }

-- This is processed information about the component, in particular the dynflags will be modified.
data ComponentInfo = ComponentInfo
  { componentUnitId         :: InstalledUnitId
  -- | Processed DynFlags. Does not contain inplace packages such as local
  -- libraries. Can be used to actually load this Component.
  , componentDynFlags       :: DynFlags
  -- | Internal units, such as local libraries, that this component
  -- is loaded with. These have been extracted from the original
  -- ComponentOptions.
  , _componentInternalUnits :: [InstalledUnitId]
  -- | All targets of this components.
  , componentTargets        :: [GHC.Target]
  -- | Filepath which caused the creation of this component
  , componentFP             :: NormalizedFilePath
  -- | Component Options used to load the component.
  , _componentCOptions      :: ComponentOptions
  -- | Maps cradle dependencies, such as `stack.yaml`, or `.cabal` file
  -- to last modification time. See Note [Multi Cradle Dependency Info]
  , componentDependencyInfo :: DependencyInfo
  }

-- | Check if any dependency has been modified lately.
checkDependencyInfo :: DependencyInfo -> IO Bool
checkDependencyInfo old_di = do
  di <- getDependencyInfo (Map.keys old_di)
  return (di == old_di)

-- Note [Multi Cradle Dependency Info]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Why do we implement our own file modification tracking here?
-- The primary reason is that the custom caching logic is quite complicated and going into shake
-- adds even more complexity and more indirection. I did try for about 5 hours to work out how to
-- use shake rules rather than IO but eventually gave up.

-- | Computes a mapping from a filepath to its latest modification date.
-- See Note [Multi Cradle Dependency Info] why we do this ourselves instead
-- of letting shake take care of it.
getDependencyInfo :: [FilePath] -> IO DependencyInfo
getDependencyInfo fs = Map.fromList <$> mapM do_one fs

  where
    tryIO :: IO a -> IO (Either IOException a)
    tryIO = try

    do_one :: FilePath -> IO (FilePath, Maybe UTCTime)
    do_one fp = (fp,) . eitherToMaybe <$> tryIO (getModificationTime fp)

-- | This function removes all the -package flags which refer to packages we
-- are going to deal with ourselves. For example, if a executable depends
-- on a library component, then this function will remove the library flag
-- from the package flags for the executable
--
-- There are several places in GHC (for example the call to hptInstances in
-- tcRnImports) which assume that all modules in the HPT have the same unit
-- ID. Therefore we create a fake one and give them all the same unit id.
removeInplacePackages
    :: InstalledUnitId     -- ^ fake uid to use for our internal component
    -> [InstalledUnitId]
    -> DynFlags
    -> (DynFlags, [InstalledUnitId])
removeInplacePackages fake_uid us df = (setThisInstalledUnitId fake_uid $
                                       df { packageFlags = ps }, uids)
  where
    (uids, ps) = partitionEithers (map go (packageFlags df))
    go p@(ExposePackage _ (UnitIdArg u) _) = if GHC.toInstalledUnitId u `elem` us
                                                  then Left (GHC.toInstalledUnitId u)
                                                  else Right p
    go p = Right p

-- | Memoize an IO function, with the characteristics:
--
--   * If multiple people ask for a result simultaneously, make sure you only compute it once.
--
--   * If there are exceptions, repeatedly reraise them.
--
--   * If the caller is aborted (async exception) finish computing it anyway.
memoIO :: Ord a => (a -> IO b) -> IO (a -> IO b)
memoIO op = do
    ref <- newVar Map.empty
    return $ \k -> join $ mask_ $ modifyVar ref $ \mp ->
        case Map.lookup k mp of
            Nothing -> do
                res <- onceFork $ op k
                return (Map.insert k res mp, res)
            Just res -> return (mp, res)

-- | Throws if package flags are unsatisfiable
setOptions :: GhcMonad m => ComponentOptions -> DynFlags -> m (DynFlags, [GHC.Target])
setOptions (ComponentOptions theOpts compRoot _) dflags = do
    (dflags', targets') <- addCmdOpts theOpts dflags
    let targets = makeTargetsAbsolute compRoot targets'
    let dflags'' =
          disableWarningsAsErrors $
          -- disabled, generated directly by ghcide instead
          flip gopt_unset Opt_WriteInterface $
          -- disabled, generated directly by ghcide instead
          -- also, it can confuse the interface stale check
          dontWriteHieFiles $
          setIgnoreInterfacePragmas $
          setLinkerOptions $
          disableOptimisation $
          setUpTypedHoles $
          makeDynFlagsAbsolute compRoot dflags'
    -- initPackages parses the -package flags and
    -- sets up the visibility for each component.
    -- Throws if a -package flag cannot be satisfied.
    final_df <- liftIO $ wrapPackageSetupException $ initUnits dflags''
    return (final_df, targets)

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscNothing
  , ghcMode = CompManager
  }

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df =
    gopt_set (gopt_set df Opt_IgnoreInterfacePragmas) Opt_IgnoreOptimChanges

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { hiDir      = Just f}

setODir :: FilePath -> DynFlags -> DynFlags
setODir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { objectDir = Just f}

getCacheDirsDefault :: String -> [String] -> IO CacheDirs
getCacheDirsDefault prefix opts = do
    dir <- Just <$> getXdgDirectory XdgCache (cacheDir </> prefix ++ "-" ++ opts_hash)
    return $ CacheDirs dir dir dir
    where
        -- Create a unique folder per set of different GHC options, assuming that each different set of
        -- GHC options will create incompatible interface files.
        opts_hash = B.unpack $ B16.encode $ H.finalize $ H.updates H.init (map B.pack opts)

-- | Sub directory for the cache path
cacheDir :: String
cacheDir = "ghcide"

implicitCradleWarning :: FilePath -> T.Text
implicitCradleWarning fp =
  "No [cradle](https://github.com/mpickering/hie-bios#hie-bios) found for "
  <> T.pack fp <>
  ".\n Proceeding with [implicit cradle](https://hackage.haskell.org/package/implicit-hie).\n"<>
  "You should ignore this message, unless you see a 'Multi Cradle: No prefixes matched' error."
----------------------------------------------------------------------------------------------------

data PackageSetupException
    = PackageSetupException
        { message     :: !String
        }
    | GhcVersionMismatch
        { compileTime :: !Version
        , runTime     :: !Version
        }
    | PackageCheckFailed !NotCompatibleReason
    deriving (Eq, Show, Typeable)

instance Exception PackageSetupException

-- | Wrap any exception as a 'PackageSetupException'
wrapPackageSetupException :: IO a -> IO a
wrapPackageSetupException = handleAny $ \case
  e | Just (pkgE :: PackageSetupException) <- fromException e -> throwIO pkgE
  e -> (throwIO . PackageSetupException . show) e

showPackageSetupException :: PackageSetupException -> String
showPackageSetupException GhcVersionMismatch{..} = unwords
    ["ghcide compiled against GHC"
    ,showVersion compileTime
    ,"but currently using"
    ,showVersion runTime
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC version as the project."
    ]
showPackageSetupException PackageSetupException{..} = unwords
    [ "ghcide compiled by GHC", showVersion compilerVersion
    , "failed to load packages:", message <> "."
    , "\nPlease ensure that ghcide is compiled with the same GHC installation as the project."]
showPackageSetupException (PackageCheckFailed PackageVersionMismatch{..}) = unwords
    ["ghcide compiled with package "
    , packageName <> "-" <> showVersion compileTime
    ,"but project uses package"
    , packageName <> "-" <> showVersion runTime
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC installation as the project."
    ]
showPackageSetupException (PackageCheckFailed BasePackageAbiMismatch{..}) = unwords
    ["ghcide compiled with base-" <> showVersion compileTime <> "-" <> compileTimeAbi
    ,"but project uses base-" <> showVersion compileTime <> "-" <> runTimeAbi
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC installation as the project."
    ]

renderPackageSetupException :: FilePath -> PackageSetupException -> (NormalizedFilePath, ShowDiagnostic, Diagnostic)
renderPackageSetupException fp e =
    ideErrorWithSource (Just "cradle") (Just DsError) (toNormalizedFilePath' fp) (T.pack $ showPackageSetupException e)
