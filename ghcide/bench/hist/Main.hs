{-  Bench history

    A Shake script to analyze the performance of ghcide over the git history of the project

    Driven by a config file `bench/config.yaml` containing the list of Git references to analyze.

    Builds each one of them and executes a set of experiments using the ghcide-bench suite.

    The results of the benchmarks and the analysis are recorded in the file
    system with the following structure:

    bench-results
    ├── <git-reference>
    │   ├── ghc.path                          - path to ghc used to build the binary
    │   ├── ghcide                            - binary for this version
    ├─ <example>
    │   ├── results.csv                           - aggregated results for all the versions
    │   └── <git-reference>
    │       ├── <experiment>.gcStats.log          - RTS -s output
    │       ├── <experiment>.csv                  - stats for the experiment
    │       ├── <experiment>.svg                  - Graph of bytes over elapsed time
    │       ├── <experiment>.diff.svg             - idem, including the previous version
    │       ├── <experiment>.log                  - ghcide-bench output
    │       └── results.csv                       - results of all the experiments for the example
    ├── results.csv        - aggregated results of all the experiments and versions
    └── <experiment>.svg   - graph of bytes over elapsed time, for all the included versions

   For diff graphs, the "previous version" is the preceding entry in the list of versions
   in the config file. A possible improvement is to obtain this info via `git rev-list`.

   To execute the script:

   > cabal/stack bench

   To build a specific analysis, enumerate the desired file artifacts

   > stack bench --ba "bench-results/HEAD/results.csv bench-results/HEAD/edit.diff.svg"
   > cabal bench --benchmark-options "bench-results/HEAD/results.csv bench-results/HEAD/edit.diff.svg"

 -}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingStrategies#-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -Wno-orphans #-}

import Data.Foldable (find)
import Data.Yaml (FromJSON (..), decodeFileThrow)
import Development.Benchmark.Rules
import Development.Shake
import Experiments.Types (Example, exampleToOptions)
import qualified Experiments.Types as E
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Development.Shake.Classes
import System.Console.GetOpt
import Data.Maybe
import Control.Monad.Extra
import System.FilePath
import System.Directory as IO
import System.IO.Extra


configPath :: FilePath
configPath = "bench/config.yaml"

configOpt :: OptDescr (Either String FilePath)
configOpt = Option [] ["config"] (ReqArg Right configPath) "config file"

-- | Read the config without dependency
readConfigIO :: FilePath -> IO (Config BuildSystem)
readConfigIO = decodeFileThrow

instance IsExample Example where getExampleName = E.getExampleName
type instance RuleResult GetExample = Maybe Example
type instance RuleResult GetExamples = [Example]

shakeOpts :: ShakeOptions
shakeOpts =
    shakeOptions{shakeChange = ChangeModtimeAndDigestInput, shakeThreads = 0}

main :: IO ()
main = shakeArgsWith shakeOpts [configOpt] $ \configs wants -> pure $ Just $ do
  let config = fromMaybe configPath $ listToMaybe configs
  _configStatic <- createBuildSystem config
  case wants of
      [] -> want ["all"]
      _ -> want wants

ghcideBuildRules :: MkBuildRules BuildSystem
ghcideBuildRules = MkBuildRules findGhcForBuildSystem "ghcide" projectDepends buildGhcide
  where
      projectDepends = do
        need . map ("src" </>) =<< getDirectoryFiles "src" ["//*.hs"]
        need . map ("session-loader" </>) =<< getDirectoryFiles "session-loader" ["//*.hs"]
        need =<< getDirectoryFiles "." ["*.cabal"]

--------------------------------------------------------------------------------

data Config buildSystem = Config
  { experiments :: [Unescaped String],
    examples :: [Example],
    samples :: Natural,
    versions :: [GitCommit],
    -- | Output folder ('foo' works, 'foo/bar' does not)
    outputFolder :: String,
    buildTool :: buildSystem,
    profileInterval :: Maybe Double
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

createBuildSystem :: FilePath -> Rules (Config BuildSystem )
createBuildSystem config = do
  readConfig <- newCache $ \fp -> need [fp] >> liftIO (readConfigIO fp)

  _ <- addOracle $ \GetExperiments {} -> experiments <$> readConfig config
  _ <- addOracle $ \GetVersions {} -> versions <$> readConfig config
  _ <- versioned 1 $ addOracle $ \GetExamples{} -> examples <$> readConfig config
  _ <- versioned 1 $ addOracle $ \(GetExample name) -> find (\e -> getExampleName e == name) . examples <$> readConfig config
  _ <- addOracle $ \GetBuildSystem {} -> buildTool <$> readConfig config
  _ <- addOracle $ \GetSamples{} -> samples <$> readConfig config

  configStatic <- liftIO $ readConfigIO config
  let build = outputFolder configStatic

  buildRules build ghcideBuildRules
  benchRules build (MkBenchRules (askOracle $ GetSamples ()) benchGhcide warmupGhcide "ghcide")
  csvRules build
  svgRules build
  heapProfileRules build
  phonyRules "" "ghcide" NoProfiling build (examples configStatic)

  whenJust (profileInterval configStatic) $ \i -> do
    phonyRules "profiled-" "ghcide" (CheapHeapProfiling i) build (examples configStatic)

  return configStatic

newtype GetSamples = GetSamples () deriving newtype (Binary, Eq, Hashable, NFData, Show)
type instance RuleResult GetSamples = Natural

--------------------------------------------------------------------------------

buildGhcide
    :: BuildSystem
    -> Maybe FilePath  -- ^ working directory
    -> FilePath  -- ^ output folder
    -> Action ()
buildGhcide Cabal cwd out = do
    let cabalLocal = fromMaybe ".." cwd </> "cabal.project.local"
    previousValue <- liftIO $ do
        itExisted <- IO.doesFileExist cabalLocal
        whenMaybe itExisted $ System.IO.Extra.readFile' cabalLocal
    liftIO $ writeFile cabalLocal $ unlines
      [ "package ghcide"
      , "  ghc-options: -eventlog -rtsopts"]
    command_ (maybe [] (pure . Cwd) cwd) "cabal"
        ["install"
        ,"exe:ghcide"
        ,"--installdir=" ++ out
        ,"--install-method=copy"
        ,"--overwrite-policy=always"
        ]
        `actionFinally` do
            liftIO $ removeFile cabalLocal
            liftIO $ whenJust previousValue $ writeFile cabalLocal

buildGhcide Stack cwd out =
    command_ (maybe [] (pure . Cwd) cwd) "stack"
        ["--local-bin-path=" <> out
        ,"build"
        ,"ghcide:ghcide"
        ,"--copy-bins"
        ,"--ghc-options=-rtsopts"
        ,"--ghc-options=-eventlog"
        ]

benchGhcide
  :: Natural -> BuildSystem -> [CmdOption] -> BenchProject Example -> Action ()
benchGhcide samples buildSystem args BenchProject{..} = do
  command_ args "ghcide-bench" $
    [ "--timeout=300",
      "--no-clean",
        "-v",
        "--samples=" <> show samples,
        "--csv="     <> outcsv,
        "--ghcide="  <> exePath,
        "--ghcide-options=" <> unwords exeExtraArgs,
        "--select",
        unescaped (unescapeExperiment experiment)
    ] ++
    exampleToOptions example ++
    [ "--stack" | Stack == buildSystem
    ]

warmupGhcide :: BuildSystem -> FilePath -> [CmdOption] -> Example -> Action ()
warmupGhcide buildSystem exePath args example = do
  command args "ghcide-bench" $
    [ "--timeout=300",
      "--no-clean",
      "-v",
      "--samples=1",
      "--ghcide=" <> exePath,
      "--select=hover"
    ] ++
    exampleToOptions example ++
    [ "--stack" | Stack == buildSystem
    ]
