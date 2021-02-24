{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}

{- |
    This module provides a bunch of Shake rules to build multiple revisions of a
    project and analyse their performance.

    It assumes a project bench suite composed of examples that runs a fixed set
    of experiments on every example

    Your code must implement all of the GetFoo oracles and the IsExample class,
    instantiate the Shake rules, and probably 'want' a set of targets.

    The results of the benchmarks and the analysis are recorded in the file
    system, using the following structure:

    <build-folder>
    ├── binaries
    │   └── <git-reference>
    │        ├── ghc.path                         - path to ghc used to build the executable
    │        └── <executable>                     - binary for this version
    │        └── commitid                         - Git commit id for this reference
    ├─ <example>
    │   ├── results.csv                           - aggregated results for all the versions
    │   └── <git-reference>
    │       ├── <experiment>.gcStats.log          - RTS -s output
    │       ├── <experiment>.csv                  - stats for the experiment
    │       ├── <experiment>.svg                  - Graph of bytes over elapsed time
    │       ├── <experiment>.diff.svg             - idem, including the previous version
    │       ├── <experiment>.heap.svg             - Heap profile
    │       ├── <experiment>.log                  - bench stdout
    │       └── results.csv                       - results of all the experiments for the example
    ├── results.csv        - aggregated results of all the experiments and versions
    └── <experiment>.svg   - graph of bytes over elapsed time, for all the included versions

   For diff graphs, the "previous version" is the preceding entry in the list of versions
   in the config file. A possible improvement is to obtain this info via `git rev-list`.
 -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Development.Benchmark.Rules
  (
      buildRules, MkBuildRules(..),
      benchRules, MkBenchRules(..), BenchProject(..), ProfilingMode(..),
      csvRules,
      svgRules,
      heapProfileRules,
      phonyRules,
      allTargetsForExample,
      GetExample(..), GetExamples(..),
      IsExample(..), RuleResultForExample,
      GetExperiments(..),
      GetVersions(..),
      GetCommitId(..),
      GetBuildSystem(..),
      BuildSystem(..), findGhcForBuildSystem,
      Escaped(..), Unescaped(..), escapeExperiment, unescapeExperiment,
      GitCommit

  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson                                (FromJSON (..),
                                                            ToJSON (..),
                                                            Value (..), (.!=),
                                                            (.:?))
import Data.List (find, isInfixOf, stripPrefix, transpose)
import           Data.List.Extra                           (lower)
import Data.Maybe (fromMaybe)
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Development.Shake
import           Development.Shake.Classes                 (Binary, Hashable,
                                                            NFData, Typeable)
import           GHC.Exts                                  (IsList (toList),
                                                            fromList)
import           GHC.Generics                              (Generic)
import           GHC.Stack                                 (HasCallStack)
import qualified Graphics.Rendering.Chart.Backend.Diagrams as E
import           Graphics.Rendering.Chart.Easy             ((.=))
import qualified Graphics.Rendering.Chart.Easy             as E
import           System.Directory (createDirectoryIfMissing, findExecutable, renameFile)
import           System.FilePath
import qualified Text.ParserCombinators.ReadP              as P
import           Text.Read                                 (Read (..), get,
                                                            readMaybe,
                                                            readP_to_Prec)
import Text.Printf
import Data.Char (isDigit)
import System.Time.Extra (Seconds)

newtype GetExperiments = GetExperiments () deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetVersions = GetVersions () deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetParent = GetParent Text deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetCommitId = GetCommitId String deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetBuildSystem = GetBuildSystem () deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetExample = GetExample String deriving newtype (Binary, Eq, Hashable, NFData, Show)
newtype GetExamples = GetExamples () deriving newtype (Binary, Eq, Hashable, NFData, Show)

type instance RuleResult GetExperiments = [Unescaped String]
type instance RuleResult GetVersions = [GitCommit]
type instance RuleResult GetParent = Text
type instance RuleResult GetCommitId = String
type instance RuleResult GetBuildSystem = BuildSystem

type RuleResultForExample e =
    ( RuleResult GetExample ~ Maybe e
    , RuleResult GetExamples ~ [e]
    , IsExample e)

-- | Knowledge needed to run an example
class (Binary e, Eq e, Hashable e, NFData e, Show e, Typeable e) => IsExample e where
    getExampleName :: e -> String

--------------------------------------------------------------------------------

allTargetsForExample :: IsExample e => ProfilingMode -> FilePath -> e -> Action [FilePath]
allTargetsForExample prof baseFolder ex = do
    experiments <- askOracle $ GetExperiments ()
    versions    <- askOracle $ GetVersions ()
    let buildFolder = baseFolder </> profilingPath prof
    return $
        [buildFolder </> getExampleName ex </> "results.csv"]
        ++ [ buildFolder </> getExampleName ex </> escaped (escapeExperiment e) <.> "svg"
             | e <- experiments
           ]
        ++ [ buildFolder </>
             getExampleName ex </>
             T.unpack (humanName ver) </>
             escaped (escapeExperiment e) <.> mode
             | e <- experiments,
               ver <- versions,
               mode <- ["svg", "diff.svg"] ++ ["heap.svg" | prof /= NoProfiling]
           ]

allBinaries :: FilePath -> String -> Action [FilePath]
allBinaries buildFolder executableName = do
    versions <- askOracle $ GetVersions ()
    return $
        [ buildFolder </> "binaries" </> T.unpack (humanName ver) </> executableName
        | ver <- versions]

-- | Generate a set of phony rules:
--     * <prefix>all
--     * <prefix><example>  for each example
phonyRules
    :: (Traversable t, IsExample e)
    => String         -- ^ prefix
    -> String         -- ^ Executable name
    -> ProfilingMode
    -> FilePath
    -> t e
    -> Rules ()
phonyRules prefix executableName prof buildFolder examples = do
    forM_ examples $ \ex ->
        phony (prefix <> getExampleName ex) $ need =<<
            allTargetsForExample prof buildFolder ex
    phony (prefix <> "all") $ do
        exampleTargets <- forM examples $ \ex ->
            allTargetsForExample prof buildFolder ex
        need $ [ buildFolder </> profilingPath prof </> "results.csv" ]
             ++ concat exampleTargets
    phony (prefix <> "all-binaries") $ need =<< allBinaries buildFolder executableName
--------------------------------------------------------------------------------
type OutputFolder = FilePath
type CWD = FilePath

data MkBuildRules buildSystem = MkBuildRules
  { -- | Return the path to the GHC executable to use for the project found in the cwd
    findGhc            :: buildSystem -> Maybe CWD -> IO FilePath
    -- | Name of the binary produced by 'buildProject'
  , executableName     :: String
    -- | An action that captures the source dependencies, used for the HEAD build
  , projectDepends     :: Action ()
    -- | Build the project found in the cwd and save the build artifacts in the output folder
  , buildProject       :: buildSystem
                       -> Maybe CWD
                       -> OutputFolder
                       -> Action ()
  }

-- | Rules that drive a build system to build various revisions of a project
buildRules :: FilePattern -> MkBuildRules BuildSystem -> Rules ()
-- TODO generalize BuildSystem
buildRules build MkBuildRules{..} = do
  -- query git for the commitid for a version
  build -/- "binaries/*/commitid" %> \out -> do
      alwaysRerun

      let [_,_,ver,_] = splitDirectories out
      mbEntry <- find ((== T.pack ver) . humanName) <$> askOracle (GetVersions ())
      let gitThing :: String
          gitThing = maybe ver (T.unpack . gitName) mbEntry
      Stdout commitid <- command [] "git" ["rev-list", "-n", "1", gitThing]
      writeFileChanged out $ init commitid

  -- build rules for HEAD
  priority 10 $ [ build -/- "binaries/HEAD/" <> executableName
                , build -/- "binaries/HEAD/ghc.path"
                ]
    &%> \[out, ghcpath] -> do
      projectDepends
      liftIO $ createDirectoryIfMissing True $ dropFileName out
      buildSystem <- askOracle $ GetBuildSystem ()
      buildProject buildSystem Nothing (takeDirectory out)
      ghcLoc <- liftIO $ findGhc buildSystem Nothing
      writeFile' ghcpath ghcLoc

  -- build rules for non HEAD revisions
  [build -/- "binaries/*/" <> executableName
   ,build -/- "binaries/*/ghc.path"
   ] &%> \[out, ghcPath] -> do
      let [_, _binaries, ver, _] = splitDirectories out
      liftIO $ createDirectoryIfMissing True $ dropFileName out
      commitid <- readFile' $ takeDirectory out </> "commitid"
      cmd_ $ "git worktree add bench-temp-" ++ ver ++ " " ++ commitid
      buildSystem <- askOracle $ GetBuildSystem ()
      flip actionFinally (cmd_ ("git worktree remove bench-temp-" <> ver <> " --force" :: String)) $ do
        ghcLoc <- liftIO $ findGhc buildSystem (Just ver)
        buildProject buildSystem (Just $ "bench-temp-" <> ver) (".." </> takeDirectory out)
        writeFile' ghcPath ghcLoc

--------------------------------------------------------------------------------
data MkBenchRules buildSystem example =  forall setup. MkBenchRules
  {
  -- | Workaround for Shake not allowing to call 'askOracle' from 'benchProject
    setupProject :: Action setup
  -- | An action that invokes the executable to run the benchmark
  , benchProject :: setup -> buildSystem -> [CmdOption] -> BenchProject example -> Action ()
  -- | An action that performs any necessary warmup. Will only be invoked once
  , warmupProject :: buildSystem -> FilePath -> [CmdOption] -> example -> Action ()
  -- | Name of the executable to benchmark. Should match the one used to 'MkBuildRules'
  , executableName :: String
  }

data BenchProject example = BenchProject
    { outcsv       :: FilePath         -- ^ where to save the CSV output
    , exePath      :: FilePath         -- ^ where to find the executable for benchmarking
    , exeExtraArgs :: [String]         -- ^ extra args for the executable
    , example      :: example          -- ^ example to benchmark
    , experiment   :: Escaped String   -- ^ experiment to run
    }

data ProfilingMode = NoProfiling | CheapHeapProfiling Seconds
    deriving (Eq)

profilingP :: String -> Maybe ProfilingMode
profilingP "unprofiled" = Just NoProfiling
profilingP inp | Just delay <- stripPrefix "profiled-" inp, Just i <- readMaybe delay = Just $ CheapHeapProfiling i
profilingP _ = Nothing

profilingPath :: ProfilingMode -> FilePath
profilingPath NoProfiling = "unprofiled"
profilingPath (CheapHeapProfiling i) = "profiled-" <> show i

-- TODO generalize BuildSystem
benchRules :: RuleResultForExample example => FilePattern -> MkBenchRules BuildSystem example -> Rules ()
benchRules build MkBenchRules{..} = do

  benchResource <- newResource "ghcide-bench" 1
  -- warmup an example
  build -/- "binaries/*/*.warmup" %> \out -> do
        let [_, _, ver, exampleName] = splitDirectories (dropExtension out)
        let exePath = build </> "binaries" </> ver </> executableName
            ghcPath = build </> "binaries" </> ver </> "ghc.path"
        need [exePath, ghcPath]
        buildSystem <- askOracle  $ GetBuildSystem ()
        example <- fromMaybe (error $ "Unknown example " <> exampleName)
                    <$> askOracle (GetExample exampleName)
        let exeExtraArgs = []
            outcsv = ""
            experiment = Escaped "hover"
        withResource benchResource 1 $ warmupProject buildSystem exePath
              [ EchoStdout False,
                FileStdout out,
                RemEnv "NIX_GHC_LIBDIR",
                RemEnv "GHC_PACKAGE_PATH",
                AddPath [takeDirectory ghcPath, "."] []
              ]
              example
  -- run an experiment
  priority 0 $
    [ build -/- "*/*/*/*.csv",
      build -/- "*/*/*/*.gcStats.log",
      build -/- "*/*/*/*.output.log",
      build -/- "*/*/*/*.eventlog",
      build -/- "*/*/*/*.hp"
    ] &%> \[outcsv, outGc, outLog, outEventlog, outHp] -> do
        let [_, flavour, exampleName, ver, exp] = splitDirectories outcsv
            prof = fromMaybe (error $ "Not a valid profiling mode: " <> flavour) $ profilingP flavour
        example <- fromMaybe (error $ "Unknown example " <> exampleName)
                    <$> askOracle (GetExample exampleName)
        buildSystem <- askOracle  $ GetBuildSystem ()
        setupRes    <- setupProject
        liftIO $ createDirectoryIfMissing True $ dropFileName outcsv
        let exePath    = build </> "binaries" </> ver </> executableName
            exeExtraArgs =
                [ "+RTS"
                , "-l-au"
                , "-S" <> outGc]
             ++ concat
                [[ "-h"
                  , "-i" <> show i
                  , "-qg"]
                 | CheapHeapProfiling i <- [prof]]
             ++ ["-RTS"]
            ghcPath    = build </> "binaries" </> ver </> "ghc.path"
            warmupPath = build </> "binaries" </> ver </> exampleName <.> "warmup"
            experiment = Escaped $ dropExtension exp
        need [exePath, ghcPath, warmupPath]
        ghcPath <- readFile' ghcPath
        withResource benchResource 1 $ do
          benchProject setupRes buildSystem
              [ EchoStdout False,
                FileStdout outLog,
                RemEnv "NIX_GHC_LIBDIR",
                RemEnv "GHC_PACKAGE_PATH",
                AddPath [takeDirectory ghcPath, "."] []
              ]
              BenchProject {..}
        liftIO $ renameFile "ghcide.eventlog" outEventlog
        liftIO $ case prof of
            CheapHeapProfiling{} -> renameFile "ghcide.hp" outHp
            NoProfiling -> writeFile outHp dummyHp

        -- extend csv output with allocation data
        csvContents <- liftIO $ lines <$> readFile outcsv
        let header = head csvContents
            results = tail csvContents
            header' = header <> ", maxResidency, allocatedBytes"
        results' <- forM results $ \row -> do
            (maxResidency, allocations) <- liftIO
                    (parseMaxResidencyAndAllocations <$> readFile outGc)
            return $ printf "%s, %s, %s" row (showMB maxResidency) (showMB allocations)
        let csvContents' = header' : results'
        writeFileLines outcsv csvContents'
    where
        showMB :: Int -> String
        showMB x = show (x `div` 2^(20::Int)) <> "MB"

-- Parse the max residency and allocations in RTS -s output
parseMaxResidencyAndAllocations :: String -> (Int, Int)
parseMaxResidencyAndAllocations input =
    (f "maximum residency", f "bytes allocated in the heap")
  where
    inps = reverse $ lines input
    f label = case find (label `isInfixOf`) inps of
        Just l -> read $ filter isDigit $ head $ words l
        Nothing -> -1


--------------------------------------------------------------------------------

-- | Rules to aggregate the CSV output of individual experiments
csvRules :: forall example . RuleResultForExample example => FilePattern -> Rules ()
csvRules build = do
  -- build results for every experiment*example
  build -/- "*/*/*/results.csv" %> \out -> do
      experiments <- askOracle $ GetExperiments ()

      let allResultFiles = [takeDirectory out </> escaped (escapeExperiment e) <.> "csv" | e <- experiments]
      allResults <- traverse readFileLines allResultFiles

      let header = head $ head allResults
          results = map tail allResults
      writeFileChanged out $ unlines $ header : concat results

  -- aggregate all experiments for an example
  build -/- "*/*/results.csv" %> \out -> do
    versions <- map (T.unpack . humanName) <$> askOracle (GetVersions ())
    let allResultFiles = [takeDirectory out </> v </> "results.csv" | v <- versions]

    allResults <- traverse readFileLines allResultFiles

    let header = head $ head allResults
        results = map tail allResults
        header' = "version, " <> header
        results' = zipWith (\v -> map (\l -> v <> ", " <> l)) versions results

    writeFileChanged out $ unlines $ header' : interleave results'

  -- aggregate all examples
  build -/- "*/results.csv" %> \out -> do
    examples <- map (getExampleName @example) <$> askOracle (GetExamples ())
    let allResultFiles = [takeDirectory out </> e </> "results.csv" | e <- examples]

    allResults <- traverse readFileLines allResultFiles

    let header = head $ head allResults
        results = map tail allResults
        header' = "example, " <> header
        results' = zipWith (\e -> map (\l -> e <> ", " <> l)) examples results

    writeFileChanged out $ unlines $ header' : concat results'

--------------------------------------------------------------------------------

-- | Rules to produce charts for the GC stats
svgRules :: FilePattern -> Rules ()
svgRules build = do
  void $ addOracle $ \(GetParent name) -> findPrev name <$> askOracle (GetVersions ())
  -- chart GC stats for an experiment on a given revision
  priority 1 $
    build -/- "*/*/*/*.svg" %> \out -> do
      let [_, _, _example, ver, _exp] = splitDirectories out
      runLog <- loadRunLog (Escaped $ replaceExtension out "csv") ver
      let diagram = Diagram Live [runLog] title
          title = ver <> " live bytes over time"
      plotDiagram True diagram out

  -- chart of GC stats for an experiment on this and the previous revision
  priority 2 $
    build -/- "*/*/*/*.diff.svg" %> \out -> do
      let [b, flav, example, ver, exp_] = splitDirectories out
          exp = Escaped $ dropExtension2 exp_
      prev <- fmap T.unpack $ askOracle $ GetParent $ T.pack ver

      runLog <- loadRunLog (Escaped $ replaceExtension (dropExtension out) "csv") ver
      runLogPrev <- loadRunLog (Escaped $ joinPath [b,flav, example, prev, replaceExtension (dropExtension exp_) "csv"]) prev

      let diagram = Diagram Live [runLog, runLogPrev] title
          title = show (unescapeExperiment exp) <> " - live bytes over time compared"
      plotDiagram True diagram out

  -- aggregated chart of GC stats for all the revisions
  build -/- "*/*/*.svg" %> \out -> do
    let exp = Escaped $ dropExtension $ takeFileName out
    versions <- askOracle $ GetVersions ()

    runLogs <- forM (filter include versions) $ \v -> do
      let v' = T.unpack (humanName v)
      loadRunLog (Escaped $ takeDirectory out </> v' </> replaceExtension (takeFileName out) "csv") v'

    let diagram = Diagram Live runLogs title
        title = show (unescapeExperiment exp) <> " - live bytes over time"
    plotDiagram False diagram out

heapProfileRules :: FilePattern -> Rules ()
heapProfileRules build = do
  priority 3 $
    build -/- "*/*/*/*.heap.svg" %> \out -> do
      let hpFile = dropExtension2 out <.> "hp"
      need [hpFile]
      cmd_ ("hp2pretty" :: String) [hpFile]
      liftIO $ renameFile (dropExtension hpFile <.> "svg") out

dropExtension2 :: FilePath -> FilePath
dropExtension2 = dropExtension . dropExtension
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Default build system that handles Cabal and Stack
data BuildSystem = Cabal | Stack
  deriving (Eq, Read, Show, Generic)
  deriving (Binary, Hashable, NFData)

findGhcForBuildSystem :: BuildSystem -> Maybe FilePath -> IO FilePath
findGhcForBuildSystem Cabal _cwd =
    liftIO $ fromMaybe (error "ghc is not in the PATH") <$> findExecutable "ghc"
findGhcForBuildSystem Stack cwd = do
    Stdout ghcLoc <- cmd (maybe [] (pure . Cwd) cwd) ("stack exec which ghc" :: String)
    return ghcLoc

instance FromJSON BuildSystem where
    parseJSON x = fromString . lower <$> parseJSON x
      where
        fromString "stack" = Stack
        fromString "cabal" = Cabal
        fromString other   = error $ "Unknown build system: " <> other

instance ToJSON BuildSystem where
    toJSON = toJSON . show

--------------------------------------------------------------------------------

data GitCommit = GitCommit
  { -- | A git hash, tag or branch name (e.g. v0.1.0)
    gitName :: Text,
    -- | A human understandable name (e.g. fix-collisions-leak)
    name    :: Maybe Text,
    -- | The human understandable name of the parent, if specified explicitly
    parent  :: Maybe Text,
    -- | Whether to include this version in the top chart
    include :: Bool
  }
  deriving (Binary, Eq, Hashable, Generic, NFData, Show)

instance FromJSON GitCommit where
  parseJSON (String s) = pure $ GitCommit s Nothing Nothing True
  parseJSON (Object (toList -> [(name, String gitName)])) =
    pure $ GitCommit gitName (Just name) Nothing True
  parseJSON (Object (toList -> [(name, Object props)])) =
    GitCommit
      <$> props .:? "git"  .!= name
      <*> pure (Just name)
      <*> props .:? "parent"
      <*> props .:? "include" .!= True
  parseJSON _ = empty

instance ToJSON GitCommit where
  toJSON GitCommit {..} =
    case name of
      Nothing -> String gitName
      Just n  -> Object $ fromList [(n, String gitName)]

humanName :: GitCommit -> Text
humanName GitCommit {..} = fromMaybe gitName name

findPrev :: Text -> [GitCommit] -> Text
findPrev name (x : y : xx)
  | humanName y == name = humanName x
  | otherwise = findPrev name (y : xx)
findPrev name _ = name

--------------------------------------------------------------------------------

-- | A line in the output of -S
data Frame = Frame
  { allocated, copied, live            :: !Int,
    user, elapsed, totUser, totElapsed :: !Double,
    generation                         :: !Int
  }
  deriving (Show)

instance Read Frame where
  readPrec = do
    spaces
    allocated <- readPrec @Int <* spaces
    copied <- readPrec @Int <* spaces
    live <- readPrec @Int <* spaces
    user <- readPrec @Double <* spaces
    elapsed <- readPrec @Double <* spaces
    totUser <- readPrec @Double <* spaces
    totElapsed <- readPrec @Double <* spaces
    _ <- readPrec @Int <* spaces
    _ <- readPrec @Int <* spaces
    "(Gen:  " <- replicateM 7 get
    generation <- readPrec @Int
    ')' <- get
    return Frame {..}
    where
      spaces = readP_to_Prec $ const P.skipSpaces

-- | A file path containing the output of -S for a given run
data RunLog = RunLog
  { runVersion     :: !String,
    runFrames      :: ![Frame],
    runSuccess     :: !Bool
  }

loadRunLog :: HasCallStack => Escaped FilePath -> String -> Action RunLog
loadRunLog (Escaped csv_fp) ver = do
  let log_fp = replaceExtension csv_fp "gcStats.log"
  log <- readFileLines log_fp
  csv <- readFileLines csv_fp
  let frames =
        [ f
          | l <- log,
            Just f <- [readMaybe l],
            -- filter out gen 0 events as there are too many
            generation f == 1
        ]
      -- TODO this assumes a certain structure in the CSV file
      success = case map (T.split (== ',') . T.pack) csv of
          [_header, _name:s:_] | Just s <- readMaybe (T.unpack s) -> s
          _ -> error $ "Cannot parse: " <> csv_fp
  return $ RunLog ver frames success

--------------------------------------------------------------------------------

data TraceMetric = Allocated | Copied | Live | User | Elapsed
  deriving (Generic, Enum, Bounded, Read)

instance Show TraceMetric where
  show Allocated = "Allocated bytes"
  show Copied    = "Copied bytes"
  show Live      = "Live bytes"
  show User      = "User time"
  show Elapsed   = "Elapsed time"

frameMetric :: TraceMetric -> Frame -> Double
frameMetric Allocated = fromIntegral . allocated
frameMetric Copied    = fromIntegral . copied
frameMetric Live      = fromIntegral . live
frameMetric Elapsed   = elapsed
frameMetric User      = user

data Diagram = Diagram
  { traceMetric :: TraceMetric,
    runLogs     :: [RunLog],
    title       :: String
  }
  deriving (Generic)

plotDiagram :: Bool -> Diagram -> FilePath -> Action ()
plotDiagram includeFailed t@Diagram {traceMetric, runLogs} out = do
  let extract = frameMetric traceMetric
  liftIO $ E.toFile E.def out $ do
    E.layout_title .= title t
    E.setColors myColors
    forM_ runLogs $ \rl ->
      when (includeFailed || runSuccess rl) $ E.plot $ do
        lplot <- E.line
            (runVersion rl ++ if runSuccess rl then "" else " (FAILED)")
            [ [ (totElapsed f, extract f)
                | f <- runFrames rl
                ]
            ]
        return (lplot E.& E.plot_lines_style . E.line_width E.*~ 2)

--------------------------------------------------------------------------------

newtype Escaped a = Escaped {escaped :: a}

newtype Unescaped a = Unescaped {unescaped :: a}
  deriving newtype (Show, FromJSON, ToJSON, Eq, NFData, Binary, Hashable)

escapeExperiment :: Unescaped String -> Escaped String
escapeExperiment = Escaped . map f . unescaped
  where
    f ' '   = '_'
    f other = other

unescapeExperiment :: Escaped String -> Unescaped String
unescapeExperiment = Unescaped . map f . escaped
  where
    f '_'   = ' '
    f other = other

--------------------------------------------------------------------------------

(-/-) :: FilePattern -> FilePattern -> FilePattern
a -/- b = a <> "/" <> b

interleave :: [[a]] -> [a]
interleave = concat . transpose

--------------------------------------------------------------------------------

myColors :: [E.AlphaColour Double]
myColors = map E.opaque
  [ E.blue
  , E.green
  , E.red
  , E.orange
  , E.yellow
  , E.violet
  , E.black
  , E.gold
  , E.brown
  , E.hotpink
  , E.aliceblue
  , E.aqua
  , E.beige
  , E.bisque
  , E.blueviolet
  , E.burlywood
  , E.cadetblue
  , E.chartreuse
  , E.coral
  , E.crimson
  , E.darkblue
  , E.darkgray
  , E.darkgreen
  , E.darkkhaki
  , E.darkmagenta
  , E.deeppink
  , E.dodgerblue
  , E.firebrick
  , E.forestgreen
  , E.fuchsia
  , E.greenyellow
  , E.lightsalmon
  , E.seagreen
  , E.olive
  , E.sandybrown
  , E.sienna
  , E.peru
  ]

dummyHp :: String
dummyHp =
    "JOB \"ghcide\" \
    \DATE \"Sun Jan 31 09:30 2021\" \
    \SAMPLE_UNIT \"seconds\" \
    \VALUE_UNIT \"bytes\" \
    \BEGIN_SAMPLE 0.000000 \
    \END_SAMPLE 0.000000"
