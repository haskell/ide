-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS -Wno-dodgy-imports -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -Wno-missing-signatures #-} -- TODO: Remove!

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    HieFileResult(..),
    HieFile(..),
    NameCacheUpdater(..),
    hieExportNames,
    mkHieFile,
    mkHieFile',
    enrichHie,
    writeHieFile,
    readHieFile,
    supportsHieFiles,
    setHieDir,
    dontWriteHieFiles,
#if !MIN_VERSION_ghc(8,8,0)
    ml_hie_file,
    addBootSuffixLocnOut,
    getRealSrcSpan,
#endif
    hPutStringBuffer,
    addIncludePathsQuote,
    getModuleHash,
    getPackageName,
    setUpTypedHoles,
    GHC.ModLocation,
    Module.addBootSuffix,
    pattern ModLocation,
    pattern ExposePackage,
    HasSrcSpan,
    getLoc,
    upNameCache,
    disableWarningsAsErrors,
    AvailInfo,
    tcg_exports,
    pattern FunTy,

#if MIN_VERSION_ghc(8,10,0)
    module GHC.Hs.Extension,
    module LinkerTypes,
#else
    module HsExtension,
    noExtField,
    linkableTime,
#endif

#if MIN_VERSION_ghc(9,0,1)
    -- Reexports from GHC
    UnitId,
    moduleUnitId,
    pkgState,
    thisInstalledUnitId,
    -- Reexports from DynFlags
    thisPackage,
    writeIfaceFile,

    gcatch,
#else
    RefMap,
    Unit,
#endif
    -- Linear
    Scaled,
    scaledThing,

    lookupUnit',
    preloadClosureUs,
    -- Reexports from Package
    InstalledUnitId,
    PackageConfig,
    getPackageConfigMap,
    getPackageIncludePath,
    installedModule,

    pattern DefiniteUnitId,
    packageName,
    packageNameString,
    packageVersion,
    toInstalledUnitId,
    lookupPackage,
    -- lookupPackage',
    explicitPackages,
    exposedModules,
    packageConfigId,
    setThisInstalledUnitId,
    initUnits,
    lookupInstalledPackage,
    oldLookupInstalledPackage,
    unitDepends,

    haddockInterfaces,

    oldUnhelpfulSpan ,
    pattern IsBoot,
    pattern NotBoot,
    pattern OldRealSrcSpan,

    oldRenderWithStyle,
    oldMkUserStyle,
    oldMkErrStyle,
    oldFormatErrDoc,
    oldListVisibleModuleNames,
    oldLookupModuleWithSuggestions,

    nodeInfo',
    getNodeIds,
    stringToUnit,
    rtsUnit,

    LogActionCompat,
    logActionCompat,

    pprSigmaType,

    module GHC,
    module DynFlags,
    initializePlugins,
    applyPluginsParsedResultAction,
    module Compat.HieTypes,
    module Compat.HieUtils,
    dropForAll,
    isQualifiedImport,
    GhcVersion(..),
    ghcVersion,
    ghcVersionStr
    ) where

#if MIN_VERSION_ghc(8,10,0)
import           LinkerTypes
#endif

import           DynFlags               hiding (ExposePackage)
import qualified DynFlags
import qualified ErrUtils               as Err
import           Fingerprint            (Fingerprint)
import qualified Module
import qualified Outputable             as Out
import           StringBuffer
#if MIN_VERSION_ghc(9,0,1)
import           Control.Exception.Safe as Safe (Exception, MonadCatch, catch)
import qualified Data.Set               as S
import           GHC.Core.TyCo.Ppr      (pprSigmaType)
import           GHC.Core.TyCo.Rep      (Scaled, scaledThing)
import           GHC.Iface.Load
import           GHC.Types.Unique.Set   (emptyUniqSet)
import qualified SrcLoc
#else
import           Module                 (InstalledUnitId,
                                         UnitId (DefiniteUnitId),
                                         toInstalledUnitId)
import           TcType                 (pprSigmaType)
#endif
import           Compat.HieAst          (enrichHie, mkHieFile)
import           Compat.HieBin
import           Compat.HieTypes
import           Compat.HieUtils
import qualified Data.ByteString        as BS
import           Data.IORef
import           HscTypes
import           MkIface
import           NameCache
import           Packages
import           TcRnTypes

#if MIN_VERSION_ghc(8,10,0)
import           GHC.Hs.Extension
#else
import           HsExtension
#endif

import           Avail
import           GHC                    hiding (HasSrcSpan, ModLocation, getLoc,
                                         lookupName)
import qualified GHC
import qualified TyCoRep
#if MIN_VERSION_ghc(8,8,0)
import           Data.List              (foldl')
#else
import           Data.List              (foldl', isSuffixOf)
#endif

import           Control.Applicative    ((<|>))
import qualified Data.Map               as M
import           DynamicLoading
import           Plugins                (Plugin (parsedResultAction),
                                         withPlugins)

#if !MIN_VERSION_ghc(8,8,0)
import           SrcLoc                 (RealLocated)
import           System.FilePath        ((-<.>))
#endif

#if !MIN_VERSION_ghc(8,8,0)
import qualified EnumSet

import           Foreign.ForeignPtr
import           System.IO


hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl (StringBuffer buf len cur)
    = withForeignPtr (plusForeignPtr buf cur) $ \ptr ->
             hPutBuf hdl ptr len

#endif

#if !MIN_VERSION_ghc(8,10,0)
noExtField :: NoExt
noExtField = noExt
#endif

supportsHieFiles :: Bool
supportsHieFiles = True

hieExportNames :: HieFile -> [(SrcSpan, Name)]
hieExportNames = nameListFromAvails . hie_exports

#if !MIN_VERSION_ghc(8,8,0)
ml_hie_file :: GHC.ModLocation -> FilePath
ml_hie_file ml
  | "boot" `isSuffixOf ` ml_hi_file ml = ml_hi_file ml -<.> ".hie-boot"
  | otherwise  = ml_hi_file ml -<.> ".hie"
#endif

upNameCache :: IORef NameCache -> (NameCache -> (NameCache, c)) -> IO c
#if !MIN_VERSION_ghc(8,8,0)
upNameCache ref upd_fn
  = atomicModifyIORef' ref upd_fn
#else
upNameCache = updNameCache
#endif


#if !MIN_VERSION_ghc(9,0,1)
type RefMap a = M.Map Identifier [(Span, IdentifierDetails a)]
#endif

mkHieFile' :: ModSummary
           -> [AvailInfo]
           -> HieASTs Type
           -> BS.ByteString
           -> Hsc HieFile
mkHieFile' ms exports asts src = do
  let Just src_file = ml_hs_file $ ms_location ms
      (asts',arr) = compressTypes asts
  return $ HieFile
      { hie_hs_file = src_file
      , hie_module = ms_mod ms
      , hie_types = arr
      , hie_asts = asts'
      -- mkIfaceExports sorts the AvailInfos for stability
      , hie_exports = mkIfaceExports exports
      , hie_hs_src = src
      }

addIncludePathsQuote :: FilePath -> DynFlags -> DynFlags
addIncludePathsQuote path x = x{includePaths = f $ includePaths x}
    where f i = i{includePathsQuote = path : includePathsQuote i}

pattern ModLocation :: Maybe FilePath -> FilePath -> FilePath -> GHC.ModLocation
#if MIN_VERSION_ghc(8,8,0)
pattern ModLocation a b c <-
    GHC.ModLocation a b c _ where ModLocation a b c = GHC.ModLocation a b c ""
#else
pattern ModLocation a b c <-
    GHC.ModLocation a b c where ModLocation a b c = GHC.ModLocation a b c
#endif

setHieDir :: FilePath -> DynFlags -> DynFlags
setHieDir _f d =
#if MIN_VERSION_ghc(8,8,0)
    d { hieDir     = Just _f}
#else
    d
#endif

dontWriteHieFiles :: DynFlags -> DynFlags
dontWriteHieFiles d =
#if MIN_VERSION_ghc(8,8,0)
    gopt_unset d Opt_WriteHie
#else
    d
#endif

setUpTypedHoles ::DynFlags -> DynFlags
setUpTypedHoles df
  = flip gopt_unset Opt_AbstractRefHoleFits    -- too spammy
#if MIN_VERSION_ghc(8,8,0)
  $ flip gopt_unset Opt_ShowDocsOfHoleFits     -- not used
#endif
  $ flip gopt_unset Opt_ShowMatchesOfHoleFits  -- nice but broken (forgets module qualifiers)
  $ flip gopt_unset Opt_ShowProvOfHoleFits     -- not used
  $ flip gopt_unset Opt_ShowTypeAppOfHoleFits  -- not used
  $ flip gopt_unset Opt_ShowTypeAppVarsOfHoleFits -- not used
  $ flip gopt_unset Opt_ShowTypeOfHoleFits     -- massively simplifies parsing
  $ flip gopt_set   Opt_SortBySubsumHoleFits   -- very nice and fast enough in most cases
  $ flip gopt_unset Opt_SortValidHoleFits
  $ flip gopt_unset Opt_UnclutterValidHoleFits
  $ df
  { refLevelHoleFits = refLevelHoleFits df <|> Just 1   -- becomes slow at higher levels
  , maxRefHoleFits   = maxRefHoleFits df <|> Just 10  -- quantity does not impact speed
  , maxValidHoleFits = maxValidHoleFits df <|> Just 10  -- quantity does not impact speed
  }


nameListFromAvails :: [AvailInfo] -> [(SrcSpan, Name)]
nameListFromAvails as =
  map (\n -> (nameSrcSpan n, n)) (concatMap availNames as)

#if MIN_VERSION_ghc(9,0,0)
-- type HasSrcSpan x a = (GenLocated SrcSpan a ~ x)
-- type HasSrcSpan x = () :: Constraint

class HasSrcSpan a where
  getLoc :: a -> SrcSpan

instance HasSrcSpan (GenLocated SrcSpan a) where
  getLoc = GHC.getLoc

-- getLoc :: GenLocated l a -> l
-- getLoc = GHC.getLoc

#elif MIN_VERSION_ghc(8,8,0)
type HasSrcSpan = GHC.HasSrcSpan
getLoc :: HasSrcSpan a => a -> SrcSpan
getLoc = GHC.getLoc

#else

class HasSrcSpan a where
    getLoc :: a -> SrcSpan
instance HasSrcSpan Name where
    getLoc = nameSrcSpan
instance HasSrcSpan (GenLocated SrcSpan a) where
    getLoc = GHC.getLoc

-- | Add the @-boot@ suffix to all output file paths associated with the
-- module, not including the input file itself
addBootSuffixLocnOut :: GHC.ModLocation -> GHC.ModLocation
addBootSuffixLocnOut locn
  = locn { ml_hi_file  = Module.addBootSuffix (ml_hi_file locn)
         , ml_obj_file = Module.addBootSuffix (ml_obj_file locn)
         }
#endif

getModuleHash :: ModIface -> Fingerprint
#if MIN_VERSION_ghc(8,10,0)
getModuleHash = mi_mod_hash . mi_final_exts
#else
getModuleHash = mi_mod_hash
#endif

-- type PackageName = Packages.PackageName
#if MIN_VERSION_ghc(9,0,0)
-- NOTE: Since both the new and old version uses UnitId with different meaning,
-- we try to avoid it and instead use InstalledUnitId and Unit, since it is unambiguous.
type UnitId            = Module.Unit
type InstalledUnitId   = Module.UnitId
type PackageConfig     = Packages.UnitInfo
pattern DefiniteUnitId x = Module.RealUnit x
definiteUnitId         = Module.RealUnit
defUnitId              = Module.Definite
installedModule        = Module.Module
-- pattern InstalledModule a b = Module.Module a b
packageName            = Packages.unitPackageName
lookupPackage          = Packages.lookupUnit . unitState
-- lookupPackage'         = undefined
-- lookupPackage' b pm u  = Packages.lookupUnit' b pm undefined u
-- lookupPackage' b pm u  = Packages.lookupUnit' b pm emptyUniqSet u -- TODO: Is this correct?
-- lookupPackage'         = fmap Packages.lookupUnit' . unitState
getPackageConfigMap    = Packages.unitInfoMap . unitState
preloadClosureUs         = Packages.preloadClosure . unitState
-- getPackageConfigMap    = unitState
-- getPackageIncludePath  = undefined
getPackageIncludePath  = Packages.getUnitIncludePath
explicitPackages       = Packages.explicitUnits
pkgState               = GHC.unitState
packageNameString      = Packages.unitPackageNameString
packageVersion         = Packages.unitPackageVersion
-- toInstalledUnitId      = id -- Module.toUnitId -- TODO: This is probably wrong
toInstalledUnitId      = Module.toUnitId
exposedModules         = Packages.unitExposedModules
packageConfigId        = Packages.mkUnit
moduleUnitId           = Module.moduleUnit
lookupInstalledPackage = Packages.lookupUnitId
oldLookupInstalledPackage = Packages.lookupUnitId . unitState
-- initUnits              = Packages.initUnits
-- initPackages           = initPackagesx
haddockInterfaces      = unitHaddockInterfaces

thisInstalledUnitId    = GHC.homeUnitId
thisPackage            = DynFlags.homeUnit
setThisInstalledUnitId uid df = df { homeUnitId = uid}

oldUnhelpfulSpan  = UnhelpfulSpan . SrcLoc.UnhelpfulOther
-- unhelpfulOther = unhelpfulOther . _
pattern OldRealSrcSpan :: RealSrcSpan -> SrcSpan
pattern OldRealSrcSpan x <- RealSrcSpan x _ where
    OldRealSrcSpan x = RealSrcSpan x Nothing
{-# COMPLETE OldRealSrcSpan, UnhelpfulSpan #-}

oldListVisibleModuleNames = Packages.listVisibleModuleNames . unitState
oldLookupModuleWithSuggestions = Packages.lookupModuleWithSuggestions . unitState
-- oldLookupInPackageDB = Packages.lookupInPackageDB . unitState

oldRenderWithStyle dflags sdoc sty = Out.renderWithStyle (initSDocContext dflags sty) sdoc
oldMkUserStyle _ = Out.mkUserStyle
oldMkErrStyle _ = Out.mkErrStyle

-- TODO: This is still a mess!
oldFormatErrDoc :: DynFlags -> Err.ErrDoc -> Out.SDoc
oldFormatErrDoc dflags = Err.formatErrDoc dummySDocContext
  where dummySDocContext = initSDocContext dflags Out.defaultUserStyle
-- oldFormatErrDoc = Err.formatErrDoc . undefined
writeIfaceFile = writeIface

type LogActionCompat = DynFlags -> WarnReason -> Severity -> SrcSpan -> PrintUnqualified -> Out.SDoc -> IO ()

-- alwaysQualify seems to still do the right thing here, according to the "unqualified warnings" test.
logActionCompat :: LogActionCompat -> LogAction
logActionCompat logAction dynFlags wr severity loc = logAction dynFlags wr severity loc alwaysQualify

-- We are using Safe here, which is not equivalent, but probably what we want.
gcatch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
gcatch = Safe.catch

#else

type LogActionCompat = DynFlags -> WarnReason -> Severity -> SrcSpan -> PrintUnqualified -> Out.SDoc -> IO ()

logActionCompat :: LogActionCompat -> LogAction
logActionCompat logAction dynFlags wr severity loc style = logAction dynFlags wr severity loc (Out.queryQual style)

type Unit = Module.UnitId
-- type PackageConfig = Packages.PackageConfig
definiteUnitId :: Module.DefUnitId -> UnitId
definiteUnitId = Module.DefiniteUnitId
defUnitId :: InstalledUnitId -> Module.DefUnitId
defUnitId = Module.DefUnitId
installedModule :: InstalledUnitId -> ModuleName -> Module.InstalledModule
installedModule = Module.InstalledModule
oldLookupInstalledPackage :: DynFlags -> InstalledUnitId -> Maybe PackageConfig
oldLookupInstalledPackage = Packages.lookupInstalledPackage
-- packageName = Packages.packageName
-- lookupPackage = Packages.lookupPackage
-- getPackageConfigMap = Packages.getPackageConfigMap
setThisInstalledUnitId :: InstalledUnitId -> DynFlags -> DynFlags
setThisInstalledUnitId uid df = df { thisInstalledUnitId = uid}

lookupUnit' :: Bool -> PackageConfigMap -> p -> UnitId -> Maybe PackageConfig
lookupUnit' b pcm _ = Packages.lookupPackage' b pcm
preloadClosureUs = const ()

oldUnhelpfulSpan  = UnhelpfulSpan
pattern OldRealSrcSpan :: RealSrcSpan -> SrcSpan
pattern OldRealSrcSpan x = RealSrcSpan x
{-# COMPLETE OldRealSrcSpan, UnhelpfulSpan #-}

pattern NotBoot, IsBoot :: IsBootInterface
pattern NotBoot = False
pattern IsBoot = True

initUnits              = fmap fst . Packages.initPackages

unitDepends            = depends

oldListVisibleModuleNames = Packages.listVisibleModuleNames
oldLookupModuleWithSuggestions = Packages.lookupModuleWithSuggestions
-- oldLookupInPackageDB = Packages.lookupInPackageDB

oldRenderWithStyle = Out.renderWithStyle
oldMkUserStyle = Out.mkUserStyle
oldMkErrStyle = Out.mkErrStyle
oldFormatErrDoc = Err.formatErrDoc

-- Linear Haskell
type Scaled a = a
scaledThing :: Scaled a -> a
scaledThing = id
#endif

getPackageName :: DynFlags -> InstalledUnitId -> Maybe PackageName
getPackageName dfs i = packageName <$> lookupPackage dfs (definiteUnitId (defUnitId i))

disableWarningsAsErrors :: DynFlags -> DynFlags
disableWarningsAsErrors df =
    flip gopt_unset Opt_WarnIsError $ foldl' wopt_unset_fatal df [toEnum 0 ..]

#if !MIN_VERSION_ghc(8,8,0)
wopt_unset_fatal :: DynFlags -> WarningFlag -> DynFlags
wopt_unset_fatal dfs f
    = dfs { fatalWarningFlags = EnumSet.delete f (fatalWarningFlags dfs) }

getRealSrcSpan :: RealLocated a -> RealSrcSpan
getRealSrcSpan = GHC.getLoc
#endif

applyPluginsParsedResultAction :: HscEnv -> DynFlags -> ModSummary -> ApiAnns -> ParsedSource -> IO ParsedSource
applyPluginsParsedResultAction env dflags ms hpm_annotations parsed = do
  -- Apply parsedResultAction of plugins
  let applyPluginAction p opts = parsedResultAction p opts ms
  fmap hpm_module $
    runHsc env $ withPlugins dflags applyPluginAction
      (HsParsedModule parsed [] hpm_annotations)

pattern ExposePackage :: String -> PackageArg -> ModRenaming -> PackageFlag
-- https://github.com/facebook/fbghc
#ifdef __FACEBOOK_HASKELL__
pattern ExposePackage s a mr <- DynFlags.ExposePackage s a _ mr
#else
pattern ExposePackage s a mr = DynFlags.ExposePackage s a mr
#endif

-- | Take AST representation of type signature and drop `forall` part from it (if any), returning just type's body
dropForAll :: LHsType pass -> LHsType pass
#if MIN_VERSION_ghc(8,10,0)
dropForAll = snd . GHC.splitLHsForAllTyInvis
#else
dropForAll = snd . GHC.splitLHsForAllTy
#endif

pattern FunTy :: Type -> Type -> Type
#if MIN_VERSION_ghc(8,10,0)
pattern FunTy arg res <- TyCoRep.FunTy {ft_arg = arg, ft_res = res}
#else
pattern FunTy arg res <- TyCoRep.FunTy arg res
#endif

isQualifiedImport :: ImportDecl a -> Bool
#if MIN_VERSION_ghc(8,10,0)
isQualifiedImport ImportDecl{ideclQualified = NotQualified} = False
isQualifiedImport ImportDecl{}                              = True
#else
isQualifiedImport ImportDecl{ideclQualified}                = ideclQualified
#endif
isQualifiedImport _                                         = False



#if MIN_VERSION_ghc(9,0,0)
getNodeIds :: HieAST a -> M.Map Identifier (IdentifierDetails a)
getNodeIds = M.foldl' combineNodeIds M.empty . getSourcedNodeInfo . sourcedNodeInfo

ad `combineNodeIds` (NodeInfo _ _ bd) = M.unionWith (<>) ad bd

--  Copied from GHC and adjusted to accept TypeIndex instead of Type
-- nodeInfo' :: Ord a => HieAST a -> NodeInfo a
nodeInfo' :: HieAST TypeIndex -> NodeInfo TypeIndex
nodeInfo' = M.foldl' combineNodeInfo' emptyNodeInfo . getSourcedNodeInfo . sourcedNodeInfo

combineNodeInfo' :: Ord a => NodeInfo a -> NodeInfo a -> NodeInfo a
(NodeInfo as ai ad) `combineNodeInfo'` (NodeInfo bs bi bd) =
  NodeInfo (S.union as bs) (mergeSorted ai bi) (M.unionWith (<>) ad bd)
  where
    mergeSorted :: Ord a => [a] -> [a] -> [a]
    mergeSorted la@(a:as) lb@(b:bs) = case compare a b of
                                        LT -> a : mergeSorted as lb
                                        EQ -> a : mergeSorted as bs
                                        GT -> b : mergeSorted la bs
    mergeSorted as [] = as
    mergeSorted [] bs = bs

stringToUnit = Module.stringToUnit
rtsUnit = Module.rtsUnit
#else

getNodeIds = nodeIdentifiers . nodeInfo
-- import qualified FastString as FS

-- nodeInfo' :: HieAST TypeIndex -> NodeInfo TypeIndex
nodeInfo' :: Ord a => HieAST a -> NodeInfo a
nodeInfo' = nodeInfo
-- type Unit = UnitId
-- unitString :: Unit -> String
-- unitString = unitIdString
stringToUnit :: String -> Unit
stringToUnit = Module.stringToUnitId
-- moduleUnit :: Module -> Unit
-- moduleUnit = moduleUnitId
-- unhelpfulSpanFS :: FS.FastString -> FS.FastString
-- unhelpfulSpanFS = id
rtsUnit = Module.rtsUnitId
#endif

data GhcVersion
  = GHC86
  | GHC88
  | GHC810
  | GHC90
  deriving (Eq, Ord, Show)

ghcVersionStr :: String
ghcVersionStr = VERSION_ghc

ghcVersion :: GhcVersion
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
ghcVersion = GHC90
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
ghcVersion = GHC810
#elif MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
ghcVersion = GHC88
#elif MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
ghcVersion = GHC86
#endif
