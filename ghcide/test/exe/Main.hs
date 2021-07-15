-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-unticked-promoted-constructors #-}

module Main (main) where

import           Control.Applicative.Combinators
import           Control.Exception                        (bracket_, catch)
import qualified Control.Lens                             as Lens
import           Control.Monad
import           Control.Monad.IO.Class                   (MonadIO, liftIO)
import           Data.Aeson                               (fromJSON, toJSON)
import qualified Data.Aeson                               as A
import qualified Data.Binary                              as Binary
import           Data.Default
import           Data.Foldable
import           Data.List.Extra
import           Data.Maybe
import           Data.Rope.UTF16                          (Rope)
import qualified Data.Rope.UTF16                          as Rope
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           Development.IDE.Core.PositionMapping     (PositionResult (..),
                                                           fromCurrent,
                                                           positionResultToMaybe,
                                                           toCurrent)
import           Development.IDE.Core.Shake               (Q (..))
import           Development.IDE.GHC.Compat               (GhcVersion (..),
                                                           ghcVersion)
import           Development.IDE.GHC.Util
import qualified Development.IDE.Main                     as IDE
import           Development.IDE.Plugin.Completions.Types (extendImportCommandId)
import           Development.IDE.Plugin.TypeLenses        (typeLensCommandId)
import           Development.IDE.Spans.Common
import           Development.IDE.Test                     (Cursor,
                                                           canonicalizeUri,
                                                           diagnostic,
                                                           expectCurrentDiagnostics,
                                                           expectDiagnostics,
                                                           expectDiagnosticsWithTags,
                                                           expectMessages,
                                                           expectNoMoreDiagnostics,
                                                           flushMessages,
                                                           standardizeQuotes,
                                                           waitForAction)
import           Development.IDE.Test.Runfiles
import qualified Development.IDE.Types.Diagnostics        as Diagnostics
import           Development.IDE.Types.Location
import           Development.Shake                        (getDirectoryFilesIO)
import qualified Experiments                              as Bench
import           Ide.Plugin.Config
import           Language.LSP.Test
import           Language.LSP.Types                       hiding (mkRange)
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.Types.Lens                  as Lsp (diagnostics,
                                                                  message,
                                                                  params)
import           Language.LSP.VFS                         (applyChange)
import           Network.URI
import           System.Directory
import           System.Environment.Blank                 (getEnv, setEnv,
                                                           unsetEnv)
import           System.Exit                              (ExitCode (ExitSuccess))
import           System.FilePath
import           System.IO.Extra                          hiding (withTempDir)
import qualified System.IO.Extra
import           System.Info.Extra                        (isWindows)
import           System.Process.Extra                     (CreateProcess (cwd),
                                                           createPipe, proc,
                                                           readCreateProcessWithExitCode)
import           Test.QuickCheck
-- import Test.QuickCheck.Instances ()
import           Control.Concurrent                       (threadDelay)
import           Control.Concurrent.Async
import           Control.Lens                             ((^.))
import           Control.Monad.Extra                      (whenJust)
import           Data.IORef
import           Data.IORef.Extra                         (atomicModifyIORef_)
import           Data.String                              (IsString (fromString))
import           Data.Tuple.Extra
import           Development.IDE.Core.FileStore           (getModTime)
import           Development.IDE.Plugin.CodeAction        (matchRegExMultipleImports)
import qualified Development.IDE.Plugin.HLS.GhcIde        as Ghcide
import           Development.IDE.Plugin.Test              (TestRequest (BlockSeconds, GetInterfaceFilesDir),
                                                           WaitForIdeRuleResult (..),
                                                           blockCommandId)
import           Ide.PluginUtils                          (pluginDescToIdePlugins)
import           Ide.Types
import qualified Language.LSP.Types                       as LSP
import qualified Language.LSP.Types.Lens                  as L
import qualified Progress
import           System.Time.Extra
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.QuickCheck
import           Text.Printf                              (printf)
import           Text.Regex.TDFA                          ((=~))

waitForProgressBegin :: Session ()
waitForProgressBegin = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (Begin _))) -> Just ()
  _ -> Nothing

waitForProgressDone :: Session ()
waitForProgressDone = skipManyTill anyMessage $ satisfyMaybe $ \case
  FromServerMess SProgress (NotificationMessage _ _ (ProgressParams _ (End _))) -> Just ()
  _ -> Nothing

main :: IO ()
main = do
  -- We mess with env vars so run single-threaded.
  defaultMainWithRerun $ testGroup "ghcide"
    [ testSession "open close" $ do
        doc <- createDoc "Testing.hs" "haskell" ""
        void (skipManyTill anyMessage $ message SWindowWorkDoneProgressCreate)
        waitForProgressBegin
        closeDoc doc
        waitForProgressDone
    , initializeResponseTests
    , completionTests
    , cppTests
    , diagnosticTests
    , codeActionTests
    , codeLensesTests
    , outlineTests
    , highlightTests
    , findDefinitionAndHoverTests
    , pluginSimpleTests
    , pluginParsedResultTests
    , preprocessorTests
    , thTests
    , safeTests
    , unitTests
    , haddockTests
    , positionMappingTests
    , watchedFilesTests
    , cradleTests
    , dependentFileTest
    , nonLspCommandLine
    , benchmarkTests
    , ifaceTests
    , bootTests
    , rootUriTests
    , asyncTests
    , clientSettingsTest
    , codeActionHelperFunctionTests
    , referenceTests
    ]

initializeResponseTests :: TestTree
initializeResponseTests = withResource acquire release tests where

  -- these tests document and monitor the evolution of the
  -- capabilities announced by the server in the initialize
  -- response. Currently the server advertises almost no capabilities
  -- at all, in some cases failing to announce capabilities that it
  -- actually does provide! Hopefully this will change ...
  tests :: IO (ResponseMessage Initialize) -> TestTree
  tests getInitializeResponse =
    testGroup "initialize response capabilities"
    [ chk "   text doc sync"             _textDocumentSync  tds
    , chk "   hover"                         _hoverProvider (Just $ InL True)
    , chk "   completion"               _completionProvider (Just $ CompletionOptions Nothing (Just ["."]) Nothing (Just False))
    , chk "NO signature help"        _signatureHelpProvider Nothing
    , chk "   goto definition"          _definitionProvider (Just $ InL True)
    , chk "   goto type definition" _typeDefinitionProvider (Just $ InL True)
    -- BUG in lsp-test, this test fails, just change the accepted response
    -- for now
    , chk "NO goto implementation"  _implementationProvider (Just $ InL False)
    , chk "   find references"          _referencesProvider (Just $ InL True)
    , chk "   doc highlight"     _documentHighlightProvider (Just $ InL True)
    , chk "   doc symbol"           _documentSymbolProvider (Just $ InL True)
    , chk "   workspace symbol"    _workspaceSymbolProvider (Just True)
    , chk "   code action"             _codeActionProvider  (Just $ InL True)
    , chk "   code lens"                 _codeLensProvider  (Just $ CodeLensOptions (Just False) (Just False))
    , chk "NO doc formatting"   _documentFormattingProvider (Just $ InL False)
    , chk "NO doc range formatting"
                           _documentRangeFormattingProvider (Just $ InL False)
    , chk "NO doc formatting on typing"
                          _documentOnTypeFormattingProvider Nothing
    , chk "NO renaming"                     _renameProvider (Just $ InL False)
    , chk "NO doc link"               _documentLinkProvider Nothing
    , chk "NO color"                         _colorProvider (Just $ InL False)
    , chk "NO folding range"          _foldingRangeProvider (Just $ InL False)
    , che "   execute command"      _executeCommandProvider [extendImportCommandId, typeLensCommandId, blockCommandId]
    , chk "   workspace"                         _workspace (Just $ WorkspaceServerCapabilities (Just WorkspaceFoldersServerCapabilities{_supported = Just True, _changeNotifications = Just ( InR True )}))
    , chk "   call hierarchy"        _callHierarchyProvider (Just $ InL True)
    , chk "NO experimental"                   _experimental Nothing
    ] where

      tds = Just (InL (TextDocumentSyncOptions
                              { _openClose = Just True
                              , _change    = Just TdSyncIncremental
                              , _willSave  = Nothing
                              , _willSaveWaitUntil = Nothing
                              , _save = Just (InR $ SaveOptions {_includeText = Nothing})}))

      chk :: (Eq a, Show a) => TestName -> (ServerCapabilities -> a) -> a -> TestTree
      chk title getActual expected =
        testCase title $ getInitializeResponse >>= \ir -> expected @=? (getActual . innerCaps) ir

      che :: TestName -> (ServerCapabilities -> Maybe ExecuteCommandOptions) -> [T.Text] -> TestTree
      che title getActual expected = testCase title doTest
        where
            doTest = do
                ir <- getInitializeResponse
                let Just ExecuteCommandOptions {_commands = List commands} = getActual $ innerCaps ir
                zipWithM_ (\e o -> T.isSuffixOf e o @? show (e,o)) expected commands

  innerCaps :: ResponseMessage Initialize -> ServerCapabilities
  innerCaps (ResponseMessage _ _ (Right (InitializeResult c _))) = c
  innerCaps (ResponseMessage _ _ (Left _)) = error "Initialization error"

  acquire :: IO (ResponseMessage Initialize)
  acquire = run initializeResponse

  release :: ResponseMessage Initialize -> IO ()
  release = const $ pure ()


diagnosticTests :: TestTree
diagnosticTests = testGroup "diagnostics"
  [ testSessionWait "fix syntax error" $ do
      let content = T.unlines [ "module Testing wher" ]
      doc <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 19))
            , _rangeLength = Nothing
            , _text = "where"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [])]
  , testSessionWait "introduce syntax error" $ do
      let content = T.unlines [ "module Testing where" ]
      doc <- createDoc "Testing.hs" "haskell" content
      void $ skipManyTill anyMessage (message SWindowWorkDoneProgressCreate)
      waitForProgressBegin
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 18))
            , _rangeLength = Nothing
            , _text = "wher"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "parse error")])]
  , testSessionWait "update syntax error" $ do
      let content = T.unlines [ "module Testing(missing) where" ]
      doc <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "Not in scope: 'missing'")])]
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 15) (Position 0 16))
            , _rangeLength = Nothing
            , _text = "l"
            }
      changeDoc doc [change]
      expectDiagnostics [("Testing.hs", [(DsError, (0, 15), "Not in scope: 'lissing'")])]
  , testSessionWait "variable not in scope" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int -> Int"
            , "foo a _b = a + ab"
            , "bar :: Int -> Int -> Int"
            , "bar _a b = cd + b"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [ (DsError, (2, 15), "Variable not in scope: ab")
            , (DsError, (4, 11), "Variable not in scope: cd")
            ]
          )
        ]
  , testSessionWait "type error" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String -> Int"
            , "foo a b = a + b"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DsError, (2, 14), "Couldn't match type '[Char]' with 'Int'")]
          )
        ]
  , testSessionWait "typed hole" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> String"
            , "foo a = _ a"
            ]
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs"
          , [(DsError, (2, 8), "Found hole: _ :: Int -> String")]
          )
        ]

  , testGroup "deferral" $
    let sourceA a = T.unlines
          [ "module A where"
          , "a :: Int"
          , "a = " <> a]
        sourceB = T.unlines
          [ "module B where"
          , "import A ()"
          , "b :: Float"
          , "b = True"]
        bMessage = "Couldn't match expected type 'Float' with actual type 'Bool'"
        expectedDs aMessage =
          [ ("A.hs", [(DsError, (2,4), aMessage)])
          , ("B.hs", [(DsError, (3,4), bMessage)])]
        deferralTest title binding msg = testSessionWait title $ do
          _ <- createDoc "A.hs" "haskell" $ sourceA binding
          _ <- createDoc "B.hs" "haskell"   sourceB
          expectDiagnostics $ expectedDs msg
    in
    [ deferralTest "type error"          "True"    "Couldn't match expected type"
    , deferralTest "typed hole"          "_"       "Found hole"
    , deferralTest "out of scope var"    "unbound" "Variable not in scope"
    ]

  , testSessionWait "remove required module" $ do
      let contentA = T.unlines [ "module ModuleA where" ]
      docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      let change = TextDocumentContentChangeEvent
            { _range = Just (Range (Position 0 0) (Position 0 20))
            , _rangeLength = Nothing
            , _text = ""
            }
      changeDoc docA [change]
      expectDiagnostics [("ModuleB.hs", [(DsError, (1, 0), "Could not find module")])]
  , testSessionWait "add missing module" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA ()"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics [("ModuleB.hs", [(DsError, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      expectDiagnostics [("ModuleB.hs", [])]
  , ignoreInWindowsBecause "Broken in windows" $ testSessionWait "add missing module (non workspace)" $ do
      -- need to canonicalize in Mac Os
      tmpDir <- liftIO $ canonicalizePath =<< getTemporaryDirectory
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA ()"
            ]
      _ <- createDoc (tmpDir </> "ModuleB.hs") "haskell" contentB
      expectDiagnostics [(tmpDir </> "ModuleB.hs", [(DsError, (1, 7), "Could not find module")])]
      let contentA = T.unlines [ "module ModuleA where" ]
      _ <- createDoc (tmpDir </> "ModuleA.hs") "haskell" contentA
      expectDiagnostics [(tmpDir </> "ModuleB.hs", [])]
  , testSessionWait "cyclic module dependency" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB"
            ]
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics
        [ ( "ModuleA.hs"
          , [(DsError, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        , ( "ModuleB.hs"
          , [(DsError, (1, 7), "Cyclic module dependency between ModuleA, ModuleB")]
          )
        ]
  , testSessionWait "cyclic module dependency with hs-boot" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import {-# SOURCE #-} ModuleB"
            ]
      let contentB = T.unlines
            [ "{-# OPTIONS -Wmissing-signatures#-}"
            , "module ModuleB where"
            , "import ModuleA"
            -- introduce an artificial diagnostic
            , "foo = ()"
            ]
      let contentBboot = T.unlines
            [ "module ModuleB where"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- createDoc "ModuleB.hs-boot" "haskell" contentBboot
      expectDiagnostics [("ModuleB.hs", [(DsWarning, (3,0), "Top-level binding")])]
  , testSessionWait "correct reference used with hs-boot" $ do
      let contentB = T.unlines
            [ "module ModuleB where"
            , "import {-# SOURCE #-} ModuleA()"
            ]
      let contentA = T.unlines
            [ "module ModuleA where"
            , "import ModuleB()"
            , "x = 5"
            ]
      let contentAboot = T.unlines
            [ "module ModuleA where"
            ]
      let contentC = T.unlines
            [ "{-# OPTIONS -Wmissing-signatures #-}"
            , "module ModuleC where"
            , "import ModuleA"
            -- this reference will fail if it gets incorrectly
            -- resolved to the hs-boot file
            , "y = x"
            ]
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleA.hs-boot" "haskell" contentAboot
      _ <- createDoc "ModuleC.hs" "haskell" contentC
      expectDiagnostics [("ModuleC.hs", [(DsWarning, (3,0), "Top-level binding")])]
  , testSessionWait "redundant import" $ do
      let contentA = T.unlines ["module ModuleA where"]
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnosticsWithTags
        [ ( "ModuleB.hs"
          , [(DsWarning, (2, 0), "The import of 'ModuleA' is redundant", Just DtUnnecessary)]
          )
        ]
  , testSessionWait "redundant import even without warning" $ do
      let contentA = T.unlines ["module ModuleA where"]
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wno-unused-imports -Wmissing-signatures #-}"
            , "module ModuleB where"
            , "import ModuleA"
            -- introduce an artificial warning for testing purposes
            , "foo = ()"
            ]
      _ <- createDoc "ModuleA.hs" "haskell" contentA
      _ <- createDoc "ModuleB.hs" "haskell" contentB
      expectDiagnostics [("ModuleB.hs", [(DsWarning, (3,0), "Top-level binding")])]
  , testSessionWait "package imports" $ do
      let thisDataListContent = T.unlines
            [ "module Data.List where"
            , "x :: Integer"
            , "x = 123"
            ]
      let mainContent = T.unlines
            [ "{-# LANGUAGE PackageImports #-}"
            , "module Main where"
            , "import qualified \"this\" Data.List as ThisList"
            , "import qualified \"base\" Data.List as BaseList"
            , "useThis = ThisList.x"
            , "useBase = BaseList.map"
            , "wrong1 = ThisList.map"
            , "wrong2 = BaseList.x"
            ]
      _ <- createDoc "Data/List.hs" "haskell" thisDataListContent
      _ <- createDoc "Main.hs" "haskell" mainContent
      expectDiagnostics
        [ ( "Main.hs"
          , [(DsError, (6, 9), "Not in scope: \8216ThisList.map\8217")
            ,(DsError, (7, 9), "Not in scope: \8216BaseList.x\8217")
            ]
          )
        ]
  , testSessionWait "unqualified warnings" $ do
      let fooContent = T.unlines
            [ "{-# OPTIONS_GHC -Wredundant-constraints #-}"
            , "module Foo where"
            , "foo :: Ord a => a -> Int"
            , "foo _a = 1"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
      -- The test is to make sure that warnings contain unqualified names
      -- where appropriate. The warning should use an unqualified name 'Ord', not
      -- sometihng like 'GHC.Classes.Ord'. The choice of redundant-constraints to
      -- test this is fairly arbitrary.
          , [(DsWarning, (2, 0), "Redundant constraint: Ord a")
            ]
          )
        ]
    , testSessionWait "lower-case drive" $ do
          let aContent = T.unlines
                [ "module A.A where"
                , "import A.B ()"
                ]
              bContent = T.unlines
                [ "{-# OPTIONS_GHC -Wall #-}"
                , "module A.B where"
                , "import Data.List"
                ]
          uriB <- getDocUri "A/B.hs"
          Just pathB <- pure $ uriToFilePath uriB
          uriB <- pure $
              let (drive, suffix) = splitDrive pathB
              in filePathToUri (joinDrive (lower drive) suffix)
          liftIO $ createDirectoryIfMissing True (takeDirectory pathB)
          liftIO $ writeFileUTF8 pathB $ T.unpack bContent
          uriA <- getDocUri "A/A.hs"
          Just pathA <- pure $ uriToFilePath uriA
          uriA <- pure $
              let (drive, suffix) = splitDrive pathA
              in filePathToUri (joinDrive (lower drive) suffix)
          let itemA = TextDocumentItem uriA "haskell" 0 aContent
          let a = TextDocumentIdentifier uriA
          sendNotification STextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          NotificationMessage{_params = PublishDiagnosticsParams fileUri _ diags} <- skipManyTill anyMessage diagnostic
          -- Check that if we put a lower-case drive in for A.A
          -- the diagnostics for A.B will also be lower-case.
          liftIO $ fileUri @?= uriB
          let msg = _message (head (toList diags) :: Diagnostic)
          liftIO $ unless ("redundant" `T.isInfixOf` msg) $
              assertFailure ("Expected redundant import but got " <> T.unpack msg)
          closeDoc a
  , testSessionWait "haddock parse error" $ do
      let fooContent = T.unlines
            [ "module Foo where"
            , "foo :: Int"
            , "foo = 1 {-|-}"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      if ghcVersion >= GHC90 then
          -- Haddock parse errors are ignored on ghc-9.0.1
            pure ()
      else
        expectDiagnostics
            [ ( "Foo.hs"
              , [(DsWarning, (2, 8), "Haddock parse error on input")]
              )
            ]
  , testSessionWait "strip file path" $ do
      let
          name = "Testing"
          content = T.unlines
            [ "module " <> name <> " where"
            , "value :: Maybe ()"
            , "value = [()]"
            ]
      _ <- createDoc (T.unpack name <> ".hs") "haskell" content
      notification <- skipManyTill anyMessage diagnostic
      let
          offenders =
            Lsp.params .
            Lsp.diagnostics .
            Lens.folded .
            Lsp.message .
            Lens.filtered (T.isInfixOf ("/" <> name <> ".hs:"))
          failure msg = liftIO $ assertFailure $ "Expected file path to be stripped but got " <> T.unpack msg
      Lens.mapMOf_ offenders failure notification
  , testSession' "-Werror in cradle is ignored" $ \sessionDir -> do
      liftIO $ writeFile (sessionDir </> "hie.yaml")
        "cradle: {direct: {arguments: [\"-Wall\", \"-Werror\"]}}"
      let fooContent = T.unlines
            [ "module Foo where"
            , "foo = ()"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
          , [(DsWarning, (1, 0), "Top-level binding with no type signature:")
            ]
          )
        ]
  , testSessionWait "-Werror in pragma is ignored" $ do
      let fooContent = T.unlines
            [ "{-# OPTIONS_GHC -Wall -Werror #-}"
            , "module Foo() where"
            , "foo :: Int"
            , "foo = 1"
            ]
      _ <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics
        [ ( "Foo.hs"
          , [(DsWarning, (3, 0), "Defined but not used:")
            ]
          )
        ]
  , testCase "typecheck-all-parents-of-interest" $ runWithExtraFiles "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"
        aPath = dir </> "A.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int
    aSource <- liftIO $ readFileUtf8 aPath -- x = y :: Int

    bdoc <- createDoc bPath "haskell" bSource
    _pdoc <- createDoc pPath "haskell" pSource
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])] -- So that we know P has been loaded

    -- Change y from Int to B which introduces a type error in A (imported from P)
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing $
                    T.unlines ["module B where", "y :: Bool", "y = undefined"]]
    expectDiagnostics
      [("A.hs", [(DsError, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])
      ]

    -- Open A and edit to fix the type error
    adoc <- createDoc aPath "haskell" aSource
    changeDoc adoc [TextDocumentContentChangeEvent Nothing Nothing $
                    T.unlines ["module A where", "import B", "x :: Bool", "x = y"]]

    expectDiagnostics
      [ ( "P.hs",
          [ (DsError, (4, 6), "Couldn't match expected type 'Int' with actual type 'Bool'"),
            (DsWarning, (4, 0), "Top-level binding")
          ]
        ),
        ("A.hs", [])
      ]
    expectNoMoreDiagnostics 1

  , testSessionWait "deduplicate missing module diagnostics" $  do
      let fooContent = T.unlines [ "module Foo() where" , "import MissingModule" ]
      doc <- createDoc "Foo.hs" "haskell" fooContent
      expectDiagnostics [("Foo.hs", [(DsError, (1,7), "Could not find module 'MissingModule'")])]

      changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing "module Foo() where" ]
      expectDiagnostics []

      changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines
            [ "module Foo() where" , "import MissingModule" ] ]
      expectDiagnostics [("Foo.hs", [(DsError, (1,7), "Could not find module 'MissingModule'")])]

  , testGroup "Cancellation"
    [ cancellationTestGroup "edit header" editHeader yesDepends yesSession noParse  noTc
    , cancellationTestGroup "edit import" editImport noDepends  noSession  yesParse noTc
    , cancellationTestGroup "edit body"   editBody   yesDepends yesSession yesParse yesTc
    ]
  ]
  where
      editPair x y = let p = Position x y ; p' = Position x (y+2) in
        (TextDocumentContentChangeEvent {_range=Just (Range p p), _rangeLength=Nothing, _text="fd"}
        ,TextDocumentContentChangeEvent {_range=Just (Range p p'), _rangeLength=Nothing, _text=""})
      editHeader = editPair 0 0
      editImport = editPair 2 10
      editBody   = editPair 3 10

      noParse = False
      yesParse = True

      noDepends = False
      yesDepends = True

      noSession = False
      yesSession = True

      noTc = False
      yesTc = True

cancellationTestGroup :: TestName -> (TextDocumentContentChangeEvent, TextDocumentContentChangeEvent) -> Bool -> Bool -> Bool -> Bool -> TestTree
cancellationTestGroup name edits dependsOutcome sessionDepsOutcome parseOutcome tcOutcome = testGroup name
    [ cancellationTemplate edits Nothing
    , cancellationTemplate edits $ Just ("GetFileContents", True)
    , cancellationTemplate edits $ Just ("GhcSession", True)
      -- the outcome for GetModSummary is always True because parseModuleHeader never fails (!)
    , cancellationTemplate edits $ Just ("GetModSummary", True)
    , cancellationTemplate edits $ Just ("GetModSummaryWithoutTimestamps", True)
      -- getLocatedImports never fails
    , cancellationTemplate edits $ Just ("GetLocatedImports", True)
    , cancellationTemplate edits $ Just ("GetDependencies", dependsOutcome)
    , cancellationTemplate edits $ Just ("GhcSessionDeps", sessionDepsOutcome)
    , cancellationTemplate edits $ Just ("GetParsedModule", parseOutcome)
    , cancellationTemplate edits $ Just ("TypeCheck", tcOutcome)
    , cancellationTemplate edits $ Just ("GetHieAst", tcOutcome)
    ]

cancellationTemplate :: (TextDocumentContentChangeEvent, TextDocumentContentChangeEvent) -> Maybe (String, Bool) -> TestTree
cancellationTemplate (edit, undoEdit) mbKey = testCase (maybe "-" fst mbKey) $ runTestNoKick $ do
      doc <- createDoc "Foo.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wall #-}"
            , "module Foo where"
            , "import Data.List()"
            , "f0 x = (x,x)"
            ]

      -- for the example above we expect one warning
      let missingSigDiags = [(DsWarning, (3, 0), "Top-level binding") ]
      typeCheck doc >> expectCurrentDiagnostics doc missingSigDiags

      -- Now we edit the document and wait for the given key (if any)
      changeDoc doc [edit]
      whenJust mbKey $ \(key, expectedResult) -> do
        Right WaitForIdeRuleResult{ideResultSuccess} <- waitForAction key doc
        liftIO $ ideResultSuccess @?= expectedResult

      -- The 2nd edit cancels the active session and unbreaks the file
      -- wait for typecheck and check that the current diagnostics are accurate
      changeDoc doc [undoEdit]
      typeCheck doc >> expectCurrentDiagnostics doc missingSigDiags

      expectNoMoreDiagnostics 0.5
    where
        -- similar to run except it disables kick
        runTestNoKick s = withTempDir $ \dir -> runInDir' dir "." "." ["--test-no-kick"] s

        typeCheck doc = do
            Right WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
            liftIO $ assertBool "The file should typecheck" ideResultSuccess
            -- wait for the debouncer to publish diagnostics if the rule runs
            liftIO $ sleep 0.2
            -- flush messages to ensure current diagnostics state is updated
            flushMessages

codeActionTests :: TestTree
codeActionTests = testGroup "code actions"
  [ renameActionTests
  , typeWildCardActionTests
  , removeImportTests
  , extendImportTests
  , suggestImportClassMethodTests
  , suggestImportTests
  , suggestHideShadowTests
  , suggestImportDisambiguationTests
  , fixConstructorImportTests
  , importRenameActionTests
  , fillTypedHoleTests
  , addSigActionTests
  , insertNewDefinitionTests
  , deleteUnusedDefinitionTests
  , addInstanceConstraintTests
  , addFunctionConstraintTests
  , removeRedundantConstraintsTests
  , addTypeAnnotationsToLiteralsTest
  , exportUnusedTests
  , addImplicitParamsConstraintTests
  , removeExportTests
  ]

codeActionHelperFunctionTests :: TestTree
codeActionHelperFunctionTests = testGroup "code action helpers"
    [
    extendImportTestsRegEx
    ]


codeLensesTests :: TestTree
codeLensesTests = testGroup "code lenses"
  [ addSigLensesTests
  ]

watchedFilesTests :: TestTree
watchedFilesTests = testGroup "watched files"
  [ testSession' "workspace files" $ \sessionDir -> do
      liftIO $ writeFile (sessionDir </> "hie.yaml") "cradle: {direct: {arguments: [\"-isrc\", \"A\", \"WatchedFilesMissingModule\"]}}"
      _doc <- createDoc "A.hs" "haskell" "{-#LANGUAGE NoImplicitPrelude #-}\nmodule A where\nimport WatchedFilesMissingModule"
      watchedFileRegs <- getWatchedFilesSubscriptionsUntil STextDocumentPublishDiagnostics

      -- Expect 2 subscriptions: one for all .hs files and one for the hie.yaml cradle
      liftIO $ length watchedFileRegs @?= 2

  , testSession' "non workspace file" $ \sessionDir -> do
      tmpDir <- liftIO getTemporaryDirectory
      let yaml = "cradle: {direct: {arguments: [\"-i" <> tail(init(show tmpDir)) <> "\", \"A\", \"WatchedFilesMissingModule\"]}}"
      liftIO $ writeFile (sessionDir </> "hie.yaml") yaml
      _doc <- createDoc "A.hs" "haskell" "{-# LANGUAGE NoImplicitPrelude#-}\nmodule A where\nimport WatchedFilesMissingModule"
      watchedFileRegs <- getWatchedFilesSubscriptionsUntil STextDocumentPublishDiagnostics

      -- Expect 2 subscriptions: one for all .hs files and one for the hie.yaml cradle
      liftIO $ length watchedFileRegs @?= 2

  -- TODO add a test for didChangeWorkspaceFolder
  ]

renameActionTests :: TestTree
renameActionTests = testGroup "rename actions"
  [ testSession "change to local variable name" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argNme"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      action <- findCodeAction doc (Range (Position 2 14) (Position 2 20)) "Replace with ‘argName’"
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argName"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "change to name of imported function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybToList"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      action <- findCodeAction doc (Range (Position 3 6) (Position 3 16))  "Replace with ‘maybeToList’"
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybeToList"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "suggest multiple local variable names" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Char -> Char -> Char -> Char"
            , "foo argument1 argument2 argument3 = argumentX"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      _ <- findCodeActions doc (Range (Position 2 36) (Position 2 45))
                           ["Replace with ‘argument1’", "Replace with ‘argument2’", "Replace with ‘argument3’"]
      return()
  , testSession "change infix function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monnus` y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 3 12) (Position 3 20))
      [fixTypo] <- pure [action | InR action@CodeAction{ _title = actionTitle } <- actionsOrCommands, "monus" `T.isInfixOf` actionTitle ]
      executeCodeAction fixTypo
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monus` y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

typeWildCardActionTests :: TestTree
typeWildCardActionTests = testGroup "type wildcard actions"
  [ testSession "global signature" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: _"
            , "func x = x"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getAllCodeActions doc
      let [addSignature] = [action | InR action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                   , "Use type signature" `T.isInfixOf` actionTitle
                           ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: (p -> p)"
            , "func x = x"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "multi-line message" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: _"
            , "func x y = x + y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getAllCodeActions doc
      let [addSignature] = [action | InR action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                    , "Use type signature" `T.isInfixOf` actionTitle
                              ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: (Integer -> Integer -> Integer)"
            , "func x y = x + y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "local signature" $ do
      let content = T.unlines
            [ "module Testing where"
            , "func :: Int -> Int"
            , "func x ="
            , "  let y :: _"
            , "      y = x * 2"
            , "  in y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getAllCodeActions doc
      let [addSignature] = [action | InR action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                    , "Use type signature" `T.isInfixOf` actionTitle
                              ]
      executeCodeAction addSignature
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "func :: Int -> Int"
            , "func x ="
            , "  let y :: (Int)"
            , "      y = x * 2"
            , "  in y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

{-# HLINT ignore "Use nubOrd" #-}
removeImportTests :: TestTree
removeImportTests = testGroup "remove import actions"
  [ testSession "redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "qualified redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant binding" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "stuffA = False"
            , "stuffB :: Integer"
            , "stuffB = 123"
            , "stuffC = ()"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffA, stuffB, stuffC, stuffA)"
            , "main = print stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove stuffA, stuffC from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffB)"
            , "main = print stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant operator" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "a !! _b = a"
            , "a <?> _b = a"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA as A ((<?>), stuffB, (!!))"
            , "main = print A.stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove !!, <?> from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA as A (stuffB)"
            , "main = print A.stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant all import" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "data A = A"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (A(..), stuffB)"
            , "main = print stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove A from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffB)"
            , "main = print stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant constructor import" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "data D = A | B"
            , "data E = F"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (D(A,B), E(F))"
            , "main = B"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove A, E, F from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (D(B))"
            , "main = B"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "import containing the identifier Strict" $ do
      let contentA = T.unlines
            [ "module Strict where"
            ]
      _docA <- createDoc "Strict.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import Strict"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "remove all" $ do
      let content = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleA where"
            , "import Data.Function (fix, (&))"
            , "import qualified Data.Functor.Const"
            , "import Data.Functor.Identity"
            , "import Data.Functor.Sum (Sum (InL, InR))"
            , "import qualified Data.Kind as K (Constraint, Type)"
            , "x = InL (Identity 123)"
            , "y = fix id"
            , "type T = K.Type"
            ]
      doc <- createDoc "ModuleC.hs" "haskell" content
      _ <- waitForDiagnostics
      [_, _, _, _, InR action@CodeAction { _title = actionTitle }]
          <- nub <$> getAllCodeActions doc
      liftIO $ "Remove all redundant imports" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleA where"
            , "import Data.Function (fix)"
            , "import Data.Functor.Identity"
            , "import Data.Functor.Sum (Sum (InL))"
            , "import qualified Data.Kind as K (Type)"
            , "x = InL (Identity 123)"
            , "y = fix id"
            , "type T = K.Type"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

extendImportTests :: TestTree
extendImportTests = testGroup "extend import actions"
  [ testGroup "with checkAll" $ tests True
  , testGroup "without checkAll" $ tests False
  ]
  where
    tests overrideCheckProject =
        [ testSession "extend single line import with value" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "stuffA :: Double"
                    , "stuffA = 0.00750"
                    , "stuffB :: Integer"
                    , "stuffB = 123"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA as A (stuffB)"
                    , "main = print (stuffA, stuffB)"
                    ])
            (Range (Position 2 17) (Position 2 18))
            ["Add stuffA to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA as A (stuffB, stuffA)"
                    , "main = print (stuffA, stuffB)"
                    ])
        , testSession "extend single line import with operator" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "(.*) :: Integer -> Integer -> Integer"
                    , "x .* y = x * y"
                    , "stuffB :: Integer"
                    , "stuffB = 123"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA as A (stuffB)"
                    , "main = print (stuffB .* stuffB)"
                    ])
            (Range (Position 2 17) (Position 2 18))
            ["Add (.*) to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA as A (stuffB, (.*))"
                    , "main = print (stuffB .* stuffB)"
                    ])
        , testSession "extend single line import with type" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "type A = Double"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA ()"
                    , "b :: A"
                    , "b = 0"
                    ])
            (Range (Position 2 5) (Position 2 5))
            ["Add A to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A)"
                    , "b :: A"
                    , "b = 0"
                    ])
        , testSession "extend single line import with constructor" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data A = Constructor"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A)"
                    , "b :: A"
                    , "b = Constructor"
                    ])
            (Range (Position 3 5) (Position 3 5))
            ["Add A(Constructor) to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A (Constructor))"
                    , "b :: A"
                    , "b = Constructor"
                    ])
        , testSession "extend single line import with constructor (with comments)" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data A = Constructor"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A ({-Constructor-}))"
                    , "b :: A"
                    , "b = Constructor"
                    ])
            (Range (Position 3 5) (Position 3 5))
            ["Add A(Constructor) to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A (Constructor{-Constructor-}))"
                    , "b :: A"
                    , "b = Constructor"
                    ])
        , testSession "extend single line import with mixed constructors" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data A = ConstructorFoo | ConstructorBar"
                    , "a = 1"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A (ConstructorBar), a)"
                    , "b :: A"
                    , "b = ConstructorFoo"
                    ])
            (Range (Position 3 5) (Position 3 5))
            ["Add A(ConstructorFoo) to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A (ConstructorBar, ConstructorFoo), a)"
                    , "b :: A"
                    , "b = ConstructorFoo"
                    ])
        , testSession "extend single line qualified import with value" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "stuffA :: Double"
                    , "stuffA = 0.00750"
                    , "stuffB :: Integer"
                    , "stuffB = 123"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import qualified ModuleA as A (stuffB)"
                    , "main = print (A.stuffA, A.stuffB)"
                    ])
            (Range (Position 2 17) (Position 2 18))
            ["Add stuffA to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import qualified ModuleA as A (stuffB, stuffA)"
                    , "main = print (A.stuffA, A.stuffB)"
                    ])
        , testSession "extend multi line import with value" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "stuffA :: Double"
                    , "stuffA = 0.00750"
                    , "stuffB :: Integer"
                    , "stuffB = 123"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (stuffB"
                    , "               )"
                    , "main = print (stuffA, stuffB)"
                    ])
            (Range (Position 3 17) (Position 3 18))
            ["Add stuffA to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (stuffB, stuffA"
                    , "               )"
                    , "main = print (stuffA, stuffB)"
                    ])
        , testSession "extend single line import with method within class" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "class C a where"
                    , "  m1 :: a -> a"
                    , "  m2 :: a -> a"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(m1))"
                    , "b = m2"
                    ])
            (Range (Position 2 5) (Position 2 5))
            ["Add C(m2) to the import list of ModuleA",
             "Add m2 to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(m1, m2))"
                    , "b = m2"
                    ])
        , testSession "extend single line import with method without class" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "class C a where"
                    , "  m1 :: a -> a"
                    , "  m2 :: a -> a"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(m1))"
                    , "b = m2"
                    ])
            (Range (Position 2 5) (Position 2 5))
            ["Add m2 to the import list of ModuleA",
             "Add C(m2) to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(m1), m2)"
                    , "b = m2"
                    ])
        , testSession "extend import list with multiple choices" $ template
            [("ModuleA.hs", T.unlines
                    --  this is just a dummy module to help the arguments needed for this test
                    [  "module ModuleA (bar) where"
                    , "bar = 10"
                    ]),
            ("ModuleB.hs", T.unlines
                    --  this is just a dummy module to help the arguments needed for this test
                    [  "module ModuleB (bar) where"
                    , "bar = 10"
                    ])]
            ("ModuleC.hs", T.unlines
                    [ "module ModuleC where"
                    , "import ModuleB ()"
                    , "import ModuleA ()"
                    , "foo = bar"
                    ])
            (Range (Position 3 17) (Position 3 18))
            ["Add bar to the import list of ModuleA",
            "Add bar to the import list of ModuleB"]
            (T.unlines
                    [ "module ModuleC where"
                    , "import ModuleB ()"
                    , "import ModuleA (bar)"
                    , "foo = bar"
                    ])
        , testSession "extend import list with constructor of type operator" $ template
            []
            ("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "import Data.Type.Equality ((:~:))"
                    , "x :: (:~:) [] []"
                    , "x = Refl"
                    ])
            (Range (Position 3 17) (Position 3 18))
            ["Add type (:~:)(Refl) to the import list of Data.Type.Equality"]
            (T.unlines
                    [ "module ModuleA where"
                    , "import Data.Type.Equality ((:~:) (Refl))"
                    , "x :: (:~:) [] []"
                    , "x = Refl"
                    ])
        , expectFailBecause "importing pattern synonyms is unsupported"
          $ testSession "extend import list with pattern synonym" $ template
            [("ModuleA.hs", T.unlines
                    [ "{-# LANGUAGE PatternSynonyms #-}"
                      , "module ModuleA where"
                      , "pattern Some x = Just x"
                    ])
            ]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import A ()"
                    , "k (Some x) = x"
                    ])
            (Range (Position 2 3) (Position 2 7))
            ["Add pattern Some to the import list of A"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import A (pattern Some)"
                    , "k (Some x) = x"
                    ])
        , testSession "type constructor name same as data constructor name" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "newtype Foo = Foo Int"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA(Foo)"
                    , "f :: Foo"
                    , "f = Foo 1"
                    ])
            (Range (Position 3 4) (Position 3 6))
            ["Add Foo(Foo) to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA(Foo (Foo))"
                    , "f :: Foo"
                    , "f = Foo 1"
                    ])
        , testSession "type constructor name same as data constructor name, data constructor extraneous" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data Foo = Foo"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA()"
                    , "f :: Foo"
                    , "f = undefined"
                    ])
            (Range (Position 2 4) (Position 2 6))
            ["Add Foo to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA(Foo)"
                    , "f :: Foo"
                    , "f = undefined"
                    ])
        ]
      where
        codeActionTitle CodeAction{_title=x} = x

        template setUpModules moduleUnderTest range expectedTitles expectedContentB = do
            sendNotification SWorkspaceDidChangeConfiguration
                (DidChangeConfigurationParams $ toJSON
                  def{checkProject = overrideCheckProject})


            mapM_ (\x -> createDoc (fst x) "haskell" (snd x)) setUpModules
            docB <- createDoc (fst moduleUnderTest) "haskell" (snd moduleUnderTest)
            _  <- waitForDiagnostics
            waitForProgressDone
            actionsOrCommands <- getCodeActions docB range
            let codeActions =
                  filter
                    (T.isPrefixOf "Add" . codeActionTitle)
                    [ca | InR ca <- actionsOrCommands]
                actualTitles = codeActionTitle <$> codeActions
            -- Note that we are not testing the order of the actions, as the
            -- order of the expected actions indicates which one we'll execute
            -- in this test, i.e., the first one.
            liftIO $ sort expectedTitles @=? sort actualTitles

            -- Execute the action with the same title as the first expected one.
            -- Since we tested that both lists have the same elements (possibly
            -- in a different order), this search cannot fail.
            let firstTitle:_ = expectedTitles
                action = fromJust $
                  find ((firstTitle ==) . codeActionTitle) codeActions
            executeCodeAction action
            contentAfterAction <- documentContents docB
            liftIO $ expectedContentB @=? contentAfterAction

extendImportTestsRegEx :: TestTree
extendImportTestsRegEx = testGroup "regex parsing"
    [
      testCase "parse invalid multiple imports" $ template "foo bar foo" Nothing
    , testCase "parse malformed import list" $ template
                  "\n\8226 Perhaps you want to add \8216fromList\8217 to one of these import lists:\n    \8216Data.Map\8217)"
                  Nothing
    , testCase "parse multiple imports" $ template
                 "\n\8226 Perhaps you want to add \8216fromList\8217 to one of these import lists:\n    \8216Data.Map\8217 (app/testlsp.hs:7:1-18)\n    \8216Data.HashMap.Strict\8217 (app/testlsp.hs:8:1-29)"
                 $ Just ("fromList",[("Data.Map","app/testlsp.hs:7:1-18"),("Data.HashMap.Strict","app/testlsp.hs:8:1-29")])
    ]
    where
        template message expected = do
            liftIO $ matchRegExMultipleImports message @=? expected

suggestImportClassMethodTests :: TestTree
suggestImportClassMethodTests =
  testGroup
    "suggest import class methods"
    [ testGroup
        "new"
        [ testSession "via parent" $
            template'
            "import Data.Semigroup (Semigroup(stimes))"
            (Range (Position 5 2) (Position 5 8)),
          testSession "top level" $
            template'
              "import Data.Semigroup (stimes)"
              (Range (Position 5 2) (Position 5 8)),
          testSession "all" $
            template'
              "import Data.Semigroup"
              (Range (Position 5 2) (Position 5 8))
        ],
      testGroup
        "extend"
        [ testSession "via parent" $
            template
              [ "module A where",
                "",
                "import Data.Semigroup ()"
              ]
              (Range (Position 6 2) (Position 6 8))
              "Add Semigroup(stimes) to the import list of Data.Semigroup"
              [ "module A where",
                "",
                "import Data.Semigroup (Semigroup (stimes))"
              ],
          testSession "top level" $
            template
              [ "module A where",
                "",
                "import Data.Semigroup ()"
              ]
              (Range (Position 6 2) (Position 6 8))
              "Add stimes to the import list of Data.Semigroup"
              [ "module A where",
                "",
                "import Data.Semigroup (stimes)"
              ]
        ]
    ]
  where
    decls =
      [ "data X = X",
        "instance Semigroup X where",
        "  (<>) _ _ = X",
        "  stimes _ _ = X"
      ]
    template beforeContent range executeTitle expectedContent = do
      doc <- createDoc "A.hs" "haskell" $ T.unlines (beforeContent <> decls)
      _ <- waitForDiagnostics
      waitForProgressDone
      actions <- getCodeActions doc range
      let actions' = [x | InR x <- actions]
          titles = [_title | CodeAction {_title} <- actions']
      liftIO $ executeTitle `elem` titles @? T.unpack executeTitle <> " does not in " <> show titles
      executeCodeAction $ fromJust $ find (\CodeAction {_title} -> _title == executeTitle) actions'
      content <- documentContents doc
      liftIO $ T.unlines (expectedContent <> decls) @=? content
    template' executeTitle range = let c = ["module A where", ""] in template c range executeTitle $ c <> [executeTitle]

suggestImportTests :: TestTree
suggestImportTests = testGroup "suggest import actions"
  [ testGroup "Dont want suggestion"
    [ -- extend import
      test False ["Data.List.NonEmpty ()"] "f = nonEmpty" []                "import Data.List.NonEmpty (nonEmpty)"
      -- data constructor
    , test False []                        "f = First"    []                "import Data.Monoid (First)"
      -- internal module
    , test False []         "f :: Typeable a => a"        ["f = undefined"] "import Data.Typeable.Internal (Typeable)"
      -- package not in scope
    , test False []         "f = quickCheck"              []                "import Test.QuickCheck (quickCheck)"
      -- don't omit the parent data type of a constructor
    , test False []         "f ExitSuccess = ()"          []                "import System.Exit (ExitSuccess)"
      -- don't suggest data constructor when we only need the type
    , test False []         "f :: Bar"                    []                "import Bar (Bar(Bar))"
    ]
  , testGroup "want suggestion"
    [ wantWait  []          "f = foo"                     []                "import Foo (foo)"
    , wantWait  []          "f = Bar"                     []                "import Bar (Bar(Bar))"
    , wantWait  []          "f :: Bar"                    []                "import Bar (Bar)"
    , test True []          "f = nonEmpty"                []                "import Data.List.NonEmpty (nonEmpty)"
    , test True []          "f = (:|)"                    []                "import Data.List.NonEmpty (NonEmpty((:|)))"
    , test True []          "f :: Natural"                ["f = undefined"] "import Numeric.Natural (Natural)"
    , test True []          "f :: Natural"                ["f = undefined"] "import Numeric.Natural"
    , test True []          "f :: NonEmpty ()"            ["f = () :| []"]  "import Data.List.NonEmpty (NonEmpty)"
    , test True []          "f :: NonEmpty ()"            ["f = () :| []"]  "import Data.List.NonEmpty"
    , test True []          "f = First"                   []                "import Data.Monoid (First(First))"
    , test True []          "f = Endo"                    []                "import Data.Monoid (Endo(Endo))"
    , test True []          "f = Version"                 []                "import Data.Version (Version(Version))"
    , test True []          "f ExitSuccess = ()"          []                "import System.Exit (ExitCode(ExitSuccess))"
    , test True []          "f = AssertionFailed"         []                "import Control.Exception (AssertionFailed(AssertionFailed))"
    , test True ["Prelude"] "f = nonEmpty"                []                "import Data.List.NonEmpty (nonEmpty)"
    , test True []          "f :: Alternative f => f ()"  ["f = undefined"] "import Control.Applicative (Alternative)"
    , test True []          "f :: Alternative f => f ()"  ["f = undefined"] "import Control.Applicative"
    , test True []          "f = empty"                   []                "import Control.Applicative (Alternative(empty))"
    , test True []          "f = empty"                   []                "import Control.Applicative (empty)"
    , test True []          "f = empty"                   []                "import Control.Applicative"
    , test True []          "f = (&)"                     []                "import Data.Function ((&))"
    , test True []          "f = NE.nonEmpty"             []                "import qualified Data.List.NonEmpty as NE"
    , test True []          "f = Data.List.NonEmpty.nonEmpty" []            "import qualified Data.List.NonEmpty"
    , test True []          "f :: Typeable a => a"        ["f = undefined"] "import Data.Typeable (Typeable)"
    , test True []          "f = pack"                    []                "import Data.Text (pack)"
    , test True []          "f :: Text"                   ["f = undefined"] "import Data.Text (Text)"
    , test True []          "f = [] & id"                 []                "import Data.Function ((&))"
    , test True []          "f = (&) [] id"               []                "import Data.Function ((&))"
    , test True []          "f = (.|.)"                   []                "import Data.Bits (Bits((.|.)))"
    , test True []          "f = (.|.)"                   []                "import Data.Bits ((.|.))"
    , test True []          "f :: a ~~ b"                 []                "import Data.Type.Equality (type (~~))"
    , test True
      ["qualified Data.Text as T"
      ]                     "f = T.putStrLn"              []                "import qualified Data.Text.IO as T"
    , test True
      [ "qualified Data.Text as T"
      , "qualified Data.Function as T"
      ]                     "f = T.putStrLn"              []                "import qualified Data.Text.IO as T"
    , test True
      [ "qualified Data.Text as T"
      , "qualified Data.Function as T"
      , "qualified Data.Functor as T"
      , "qualified Data.Data as T"
      ]                     "f = T.putStrLn"              []                "import qualified Data.Text.IO as T"
    ]
    , expectFailBecause "importing pattern synonyms is unsupported" $ test True [] "k (Some x) = x" [] "import B (pattern Some)"
  ]
  where
    test = test' False
    wantWait = test' True True
    test' waitForCheckProject wanted imps def other newImp = testSessionWithExtraFiles "hover" (T.unpack def) $ \dir -> do
      let before = T.unlines $ "module A where" : ["import " <> x | x <- imps] ++ def : other
          after  = T.unlines $ "module A where" : ["import " <> x | x <- imps] ++ [newImp] ++ def : other
          cradle = "cradle: {direct: {arguments: [-hide-all-packages, -package, base, -package, text, -package-env, -, A, Bar, Foo, B]}}"
      liftIO $ writeFileUTF8 (dir </> "hie.yaml") cradle
      liftIO $ writeFileUTF8 (dir </> "B.hs") $ unlines ["{-# LANGUAGE PatternSynonyms #-}", "module B where", "pattern Some x = Just x"]
      doc <- createDoc "Test.hs" "haskell" before
      waitForProgressDone
      _diags <- waitForDiagnostics
      -- there isn't a good way to wait until the whole project is checked atm
      when waitForCheckProject $ liftIO $ sleep 0.5
      let defLine = length imps + 1
          range = Range (Position defLine 0) (Position defLine maxBound)
      actions <- getCodeActions doc range
      if wanted
         then do
             action <- liftIO $ pickActionWithTitle newImp actions
             executeCodeAction action
             contentAfterAction <- documentContents doc
             liftIO $ after @=? contentAfterAction
          else
              liftIO $ [_title | InR CodeAction{_title} <- actions, _title == newImp ] @?= []

suggestImportDisambiguationTests :: TestTree
suggestImportDisambiguationTests = testGroup "suggest import disambiguation actions"
  [ testGroup "Hiding strategy works"
    [ testGroup "fromList"
        [ testCase "AVec" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Use AVec for fromList, hiding other imports"
                "HideFunction.expected.fromList.A.hs"
        , testCase "BVec" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Use BVec for fromList, hiding other imports"
                "HideFunction.expected.fromList.B.hs"
        ]
    , testGroup "(++)"
        [ testCase "EVec" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Use EVec for ++, hiding other imports"
                "HideFunction.expected.append.E.hs"
        , testCase "Prelude" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Use Prelude for ++, hiding other imports"
                "HideFunction.expected.append.Prelude.hs"
        , testCase "AVec, indented" $
            compareTwo "HidePreludeIndented.hs" [(3,8)]
            "Use AVec for ++, hiding other imports"
            "HidePreludeIndented.expected.hs"

        ]
    , testGroup "Vec (type)"
        [ testCase "AVec" $
            compareTwo
                "HideType.hs" [(8,15)]
                "Use AVec for Vec, hiding other imports"
                "HideType.expected.A.hs"
        , testCase "EVec" $
            compareTwo
                "HideType.hs" [(8,15)]
                "Use EVec for Vec, hiding other imports"
                "HideType.expected.E.hs"
        ]
    ]
  , testGroup "Qualify strategy"
    [ testCase "won't suggest full name for qualified module" $
      withHideFunction [(8,9),(10,8)] $ \_ actions -> do
        liftIO $
            assertBool "EVec.fromList must not be suggested" $
                "Replace with qualified: EVec.fromList" `notElem`
                [ actionTitle
                | InR CodeAction { _title = actionTitle } <- actions
                ]
        liftIO $
            assertBool "EVec.++ must not be suggested" $
                "Replace with qualified: EVec.++" `notElem`
                [ actionTitle
                | InR CodeAction { _title = actionTitle } <- actions
                ]
    , testGroup "fromList"
        [ testCase "EVec" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Replace with qualified: E.fromList"
                "HideFunction.expected.qualified.fromList.E.hs"
        , testCase "Hide DuplicateRecordFields" $
            compareTwo
                "HideQualifyDuplicateRecordFields.hs" [(9, 9)]
                "Replace with qualified: AVec.fromList"
                "HideQualifyDuplicateRecordFields.expected.hs"
        , testCase "Duplicate record fields should not be imported" $ do
          withTarget ("HideQualifyDuplicateRecordFields" <.> ".hs") [(9, 9)] $
            \_ actions -> do
              liftIO $
                assertBool "Hidings should not be presented while DuplicateRecordFields exists" $
                  all not [ actionTitle =~ T.pack "Use ([A-Za-z][A-Za-z0-9]*) for fromList, hiding other imports"
                      | InR CodeAction { _title = actionTitle } <- actions]
          withTarget ("HideQualifyDuplicateRecordFieldsSelf" <.> ".hs") [(4, 4)] $
            \_ actions -> do
              liftIO $
                assertBool "ambiguity from DuplicateRecordFields should not be imported" $
                  null actions
        ]
    , testGroup "(++)"
        [ testCase "Prelude, parensed" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Replace with qualified: Prelude.++"
                "HideFunction.expected.qualified.append.Prelude.hs"
        , testCase "Prelude, infix" $
            compareTwo
                "HideQualifyInfix.hs" [(4,19)]
                "Replace with qualified: Prelude.++"
                "HideQualifyInfix.expected.hs"
        , testCase "Prelude, left section" $
            compareTwo
                "HideQualifySectionLeft.hs" [(4,15)]
                "Replace with qualified: Prelude.++"
                "HideQualifySectionLeft.expected.hs"
        , testCase "Prelude, right section" $
            compareTwo
                "HideQualifySectionRight.hs" [(4,18)]
                "Replace with qualified: Prelude.++"
                "HideQualifySectionRight.expected.hs"
        ]
    ]
  ]
  where
    hidingDir = "test/data/hiding"
    compareTwo original locs cmd expected =
        withTarget original locs $ \doc actions -> do
            expected <- liftIO $
                readFileUtf8 (hidingDir </> expected)
            action <- liftIO $ pickActionWithTitle cmd actions
            executeCodeAction action
            contentAfterAction <- documentContents doc
            liftIO $ T.replace "\r\n" "\n" expected @=? contentAfterAction
    compareHideFunctionTo = compareTwo "HideFunction.hs"
    auxFiles = ["AVec.hs", "BVec.hs", "CVec.hs", "DVec.hs", "EVec.hs", "FVec.hs"]
    withTarget file locs k = withTempDir $ \dir -> runInDir dir $ do
        liftIO $ mapM_ (\fp -> copyFile (hidingDir </> fp) $ dir </> fp)
            $ file : auxFiles
        doc <- openDoc file "haskell"
        waitForProgressDone
        void $ expectDiagnostics [(file, [(DsError, loc, "Ambiguous occurrence") | loc <- locs])]
        actions <- getAllCodeActions doc
        k doc actions
    withHideFunction = withTarget ("HideFunction" <.> "hs")

suggestHideShadowTests :: TestTree
suggestHideShadowTests =
  testGroup
    "suggest hide shadow"
    [ testGroup
        "single"
        [ testOneCodeAction
            "hide unsued"
            "Hide on from Data.Function"
            (1, 2)
            (1, 4)
            [ "import Data.Function"
            , "f on = on"
            , "g on = on"
            ]
            [ "import Data.Function hiding (on)"
            , "f on = on"
            , "g on = on"
            ]
        , testOneCodeAction
            "extend hiding unsued"
            "Hide on from Data.Function"
            (1, 2)
            (1, 4)
            [ "import Data.Function hiding ((&))"
            , "f on = on"
            ]
            [ "import Data.Function hiding (on, (&))"
            , "f on = on"
            ]
        , testOneCodeAction
            "delete unsued"
            "Hide on from Data.Function"
            (1, 2)
            (1, 4)
            [ "import Data.Function ((&), on)"
            , "f on = on"
            ]
            [ "import Data.Function ((&))"
            , "f on = on"
            ]
        , testOneCodeAction
            "hide operator"
            "Hide & from Data.Function"
            (1, 2)
            (1, 5)
            [ "import Data.Function"
            , "f (&) = (&)"
            ]
            [ "import Data.Function hiding ((&))"
            , "f (&) = (&)"
            ]
        , testOneCodeAction
            "remove operator"
            "Hide & from Data.Function"
            (1, 2)
            (1, 5)
            [ "import Data.Function ((&), on)"
            , "f (&) = (&)"
            ]
            [ "import Data.Function ( on)"
            , "f (&) = (&)"
            ]
        , noCodeAction
            "don't remove already used"
            (2, 2)
            (2, 4)
            [ "import Data.Function"
            , "g = on"
            , "f on = on"
            ]
        ]
    , testGroup
        "multi"
        [ testOneCodeAction
            "hide from B"
            "Hide ++ from B"
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B hiding ((++))"
            , "import C"
            , "f (++) = (++)"
            ]
        , testOneCodeAction
            "hide from C"
            "Hide ++ from C"
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B"
            , "import C hiding ((++))"
            , "f (++) = (++)"
            ]
        , testOneCodeAction
            "hide from Prelude"
            "Hide ++ from Prelude"
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B"
            , "import C"
            , "import Prelude hiding ((++))"
            , "f (++) = (++)"
            ]
        , testMultiCodeActions
            "manual hide all"
            [ "Hide ++ from Prelude"
            , "Hide ++ from C"
            , "Hide ++ from B"
            ]
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B hiding ((++))"
            , "import C hiding ((++))"
            , "import Prelude hiding ((++))"
            , "f (++) = (++)"
            ]
        , testOneCodeAction
            "auto hide all"
            "Hide ++ from all occurence imports"
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B hiding ((++))"
            , "import C hiding ((++))"
            , "import Prelude hiding ((++))"
            , "f (++) = (++)"
            ]
        ]
    ]
 where
  testOneCodeAction testName actionName start end origin expected =
    helper testName start end origin expected $ \cas -> do
      action <- liftIO $ pickActionWithTitle actionName cas
      executeCodeAction action
  noCodeAction testName start end origin =
    helper testName start end origin origin $ \cas -> do
      liftIO $ cas @?= []
  testMultiCodeActions testName actionNames start end origin expected =
    helper testName start end origin expected $ \cas -> do
      let r = [ca | (InR ca) <- cas, ca ^. L.title `elem` actionNames]
      liftIO $
        (length r == length actionNames)
          @? "Expected " <> show actionNames <> ", but got " <> show cas <> " which is not its superset"
      forM_ r executeCodeAction
  helper testName (line1, col1) (line2, col2) origin expected k = testSession testName $ do
    void $ createDoc "B.hs" "haskell" $ T.unlines docB
    void $ createDoc "C.hs" "haskell" $ T.unlines docC
    doc <- createDoc "A.hs" "haskell" $ T.unlines (header <> origin)
    void waitForDiagnostics
    waitForProgressDone
    cas <- getCodeActions doc (Range (Position (line1 + length header) col1) (Position (line2 + length header) col2))
    void $ k [x | x@(InR ca) <- cas, "Hide" `T.isPrefixOf` (ca ^. L.title)]
    contentAfter <- documentContents doc
    liftIO $ contentAfter @?= T.unlines (header <> expected)
  header =
    [ "{-# OPTIONS_GHC -Wname-shadowing #-}"
    , "module A where"
    , ""
    ]
  -- for multi group
  docB =
    [ "module B where"
    , "(++) = id"
    ]
  docC =
    [ "module C where"
    , "(++) = id"
    ]

insertNewDefinitionTests :: TestTree
insertNewDefinitionTests = testGroup "insert new definition actions"
  [ testSession "insert new function definition" $ do
      let txtB =
            ["foo True = select [True]"
            , ""
            ,"foo False = False"
            ]
          txtB' =
            [""
            ,"someOtherCode = ()"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" (T.unlines $ txtB ++ txtB')
      _ <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(InR CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB (R 0 0 0 50)
      liftIO $ actionTitle @?= "Define select :: [Bool] -> Bool"
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ contentAfterAction @?= T.unlines (txtB ++
        [ ""
        , "select :: [Bool] -> Bool"
        , "select = error \"not implemented\""
        ]
        ++ txtB')
  , testSession "define a hole" $ do
      let txtB =
            ["foo True = _select [True]"
            , ""
            ,"foo False = False"
            ]
          txtB' =
            [""
            ,"someOtherCode = ()"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" (T.unlines $ txtB ++ txtB')
      _ <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(InR CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB (R 0 0 0 50)
      liftIO $ actionTitle @?= "Define select :: [Bool] -> Bool"
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ contentAfterAction @?= T.unlines (
        ["foo True = select [True]"
        , ""
        ,"foo False = False"
        , ""
        , "select :: [Bool] -> Bool"
        , "select = error \"not implemented\""
        ]
        ++ txtB')
  ]


deleteUnusedDefinitionTests :: TestTree
deleteUnusedDefinitionTests = testGroup "delete unused definition action"
  [ testSession "delete unused top level binding" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
               , "module A (some) where"
               , ""
               , "f :: Int -> Int"
               , "f 1 = let a = 1"
               , "      in a"
               , "f 2 = 2"
               , ""
               , "some = ()"
               ])
    (4, 0)
    "Delete ‘f’"
    (T.unlines [
        "{-# OPTIONS_GHC -Wunused-top-binds #-}"
        , "module A (some) where"
        , ""
        , "some = ()"
        ])

  , testSession "delete unused top level binding defined in infix form" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
               , "module A (some) where"
               , ""
               , "myPlus :: Int -> Int -> Int"
               , "a `myPlus` b = a + b"
               , ""
               , "some = ()"
               ])
    (4, 2)
    "Delete ‘myPlus’"
    (T.unlines [
        "{-# OPTIONS_GHC -Wunused-top-binds #-}"
        , "module A (some) where"
        , ""
        , "some = ()"
      ])
  , testSession "delete unused binding in where clause" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (h, g) where"
               , ""
               , "h :: Int"
               , "h = 3"
               , ""
               , "g :: Int"
               , "g = 6"
               , "  where"
               , "    h :: Int"
               , "    h = 4"
               , ""
               ])
    (10, 4)
    "Delete ‘h’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (h, g) where"
               , ""
               , "h :: Int"
               , "h = 3"
               , ""
               , "g :: Int"
               , "g = 6"
               , "  where"
               , ""
               ])
  , testSession "delete unused binding with multi-oneline signatures front" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (b, c) where"
               , ""
               , "a, b, c :: Int"
               , "a = 3"
               , "b = 4"
               , "c = 5"
               ])
    (4, 0)
    "Delete ‘a’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (b, c) where"
               , ""
               , "b, c :: Int"
               , "b = 4"
               , "c = 5"
               ])
  , testSession "delete unused binding with multi-oneline signatures mid" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, c) where"
               , ""
               , "a, b, c :: Int"
               , "a = 3"
               , "b = 4"
               , "c = 5"
               ])
    (5, 0)
    "Delete ‘b’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, c) where"
               , ""
               , "a, c :: Int"
               , "a = 3"
               , "c = 5"
               ])
  , testSession "delete unused binding with multi-oneline signatures end" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, b) where"
               , ""
               , "a, b, c :: Int"
               , "a = 3"
               , "b = 4"
               , "c = 5"
               ])
    (6, 0)
    "Delete ‘c’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, b) where"
               , ""
               , "a, b :: Int"
               , "a = 3"
               , "b = 4"
               ])
  ]
  where
    testFor source pos expectedTitle expectedResult = do
      docId <- createDoc "A.hs" "haskell" source
      expectDiagnostics [ ("A.hs", [(DsWarning, pos, "not used")]) ]

      (action, title) <- extractCodeAction docId "Delete" pos

      liftIO $ title @?= expectedTitle
      executeCodeAction action
      contentAfterAction <- documentContents docId
      liftIO $ contentAfterAction @?= expectedResult

    extractCodeAction docId actionPrefix (l, c) = do
      [action@CodeAction { _title = actionTitle }]  <- findCodeActionsByPrefix docId (R l c l c) [actionPrefix]
      return (action, actionTitle)

addTypeAnnotationsToLiteralsTest :: TestTree
addTypeAnnotationsToLiteralsTest = testGroup "add type annotations to literals to satisfy contraints"
  [
    testSession "add default type to satisfy one contraint" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A (f) where"
               , ""
               , "f = 1"
               ])
    [ (DsWarning, (3, 4), "Defaulting the following constraint") ]
    "Add type annotation ‘Integer’ to ‘1’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A (f) where"
               , ""
               , "f = (1 :: Integer)"
               ])

  , testSession "add default type to satisfy one contraint in nested expressions" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A where"
               , ""
               , "f ="
               , "    let x = 3"
               , "    in x"
               ])
    [ (DsWarning, (4, 12), "Defaulting the following constraint") ]
    "Add type annotation ‘Integer’ to ‘3’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A where"
               , ""
               , "f ="
               , "    let x = (3 :: Integer)"
               , "    in x"
               ])
  , testSession "add default type to satisfy one contraint in more nested expressions" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A where"
               , ""
               , "f ="
               , "    let x = let y = 5 in y"
               , "    in x"
               ])
    [ (DsWarning, (4, 20), "Defaulting the following constraint") ]
    "Add type annotation ‘Integer’ to ‘5’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A where"
               , ""
               , "f ="
               , "    let x = let y = (5 :: Integer) in y"
               , "    in x"
               ])
  , testSession "add default type to satisfy one contraint with duplicate literals" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq \"debug\" traceShow \"debug\""
               ])
    [ (DsWarning, (6, 8), "Defaulting the following constraint")
    , (DsWarning, (6, 16), "Defaulting the following constraint")
    ]
    ("Add type annotation ‘" <> listOfChar <> "’ to ‘\"debug\"’")
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq (\"debug\" :: " <> listOfChar <> ") traceShow \"debug\""
               ])
  , testSession "add default type to satisfy two contraints" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f a = traceShow \"debug\" a"
               ])
    [ (DsWarning, (6, 6), "Defaulting the following constraint") ]
    ("Add type annotation ‘" <> listOfChar <> "’ to ‘\"debug\"’")
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f a = traceShow (\"debug\" :: " <> listOfChar <> ") a"
               ])
  , testSession "add default type to satisfy two contraints with duplicate literals" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq (\"debug\" :: [Char]) (seq (\"debug\" :: [Char]) (traceShow \"debug\"))"
               ])
    [ (DsWarning, (6, 54), "Defaulting the following constraint") ]
    ("Add type annotation ‘" <> listOfChar <> "’ to ‘\"debug\"’")
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq (\"debug\" :: [Char]) (seq (\"debug\" :: [Char]) (traceShow (\"debug\" :: " <> listOfChar <> ")))"
               ])
  ]
  where
    testFor source diag expectedTitle expectedResult = do
      docId <- createDoc "A.hs" "haskell" source
      expectDiagnostics [ ("A.hs", diag) ]

      let cursors = map snd3 diag
      (action, title) <- extractCodeAction docId "Add type annotation" (minimum cursors) (maximum cursors)

      liftIO $ title @?= expectedTitle
      executeCodeAction action
      contentAfterAction <- documentContents docId
      liftIO $ contentAfterAction @?= expectedResult

    extractCodeAction docId actionPrefix (l,c) (l', c')= do
      [action@CodeAction { _title = actionTitle }]  <- findCodeActionsByPrefix docId (R l c l' c') [actionPrefix]
      return (action, actionTitle)


fixConstructorImportTests :: TestTree
fixConstructorImportTests = testGroup "fix import actions"
  [ testSession "fix constructor import" $ template
      (T.unlines
            [ "module ModuleA where"
            , "data A = Constructor"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA(Constructor)"
            ])
      (Range (Position 1 10) (Position 1 11))
      "Fix import of A(Constructor)"
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA(A(Constructor))"
            ])
  ]
  where
    template contentA contentB range expectedAction expectedContentB = do
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      docB  <- createDoc "ModuleB.hs" "haskell" contentB
      _diags <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(InR CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB range
      liftIO $ expectedAction @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ expectedContentB @=? contentAfterAction

importRenameActionTests :: TestTree
importRenameActionTests = testGroup "import rename actions"
  [ testSession "Data.Mape -> Data.Map"   $ check "Map"
  , testSession "Data.Mape -> Data.Maybe" $ check "Maybe" ] where
  check modname = do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Mape"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 1 8) (Position 1 16))
      let [changeToMap] = [action | InR action@CodeAction{ _title = actionTitle } <- actionsOrCommands, ("Data." <> modname) `T.isInfixOf` actionTitle ]
      executeCodeAction changeToMap
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data." <> modname
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction

fillTypedHoleTests :: TestTree
fillTypedHoleTests = let

  sourceCode :: T.Text -> T.Text -> T.Text -> T.Text
  sourceCode a b c = T.unlines
    [ "module Testing where"
      , ""
      , "globalConvert :: Int -> String"
      , "globalConvert = undefined"
      , ""
      , "globalInt :: Int"
      , "globalInt = 3"
      , ""
      , "bar :: Int -> Int -> String"
      , "bar n parameterInt = " <> a <> " (n + " <> b <> " + " <> c <> ")  where"
      , "  localConvert = (flip replicate) 'x'"
      , ""
      , "foo :: () -> Int -> String"
      , "foo = undefined"

    ]

  check :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle
        oldA oldB oldC
        newA newB newC = testSession (T.unpack actionTitle) $ do
    let originalCode = sourceCode oldA oldB oldC
    let expectedCode = sourceCode newA newB newC
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 9 0) (Position 9 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "fill typed holes"
  [ check "replace _ with show"
          "_"    "n" "n"
          "show" "n" "n"

  , check "replace _ with globalConvert"
          "_"             "n" "n"
          "globalConvert" "n" "n"

  , check "replace _convertme with localConvert"
          "_convertme"   "n" "n"
          "localConvert" "n" "n"

  , check "replace _b with globalInt"
          "_a" "_b"        "_c"
          "_a" "globalInt" "_c"

  , check "replace _c with globalInt"
          "_a" "_b"        "_c"
          "_a" "_b" "globalInt"

  , check "replace _c with parameterInt"
          "_a" "_b" "_c"
          "_a" "_b"  "parameterInt"
  , check "replace _ with foo _"
          "_" "n" "n"
          "(foo _)" "n" "n"
  , testSession "replace _toException with E.toException" $ do
      let mkDoc x = T.unlines
            [ "module Testing where"
            , "import qualified Control.Exception as E"
            , "ioToSome :: E.IOException -> E.SomeException"
            , "ioToSome = " <> x ]
      doc <- createDoc "Test.hs" "haskell" $ mkDoc "_toException"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc (Range (Position 3 0) (Position 3 maxBound))
      chosen <- liftIO $ pickActionWithTitle "replace _toException with E.toException" actions
      executeCodeAction chosen
      modifiedCode <- documentContents doc
      liftIO $ mkDoc "E.toException" @=? modifiedCode
  , testSession "filling infix type hole uses prefix notation" $ do
      let mkDoc x = T.unlines
              [ "module Testing where"
              , "data A = A"
              , "foo :: A -> A -> A"
              , "foo A A = A"
              , "test :: A -> A -> A"
              , "test a1 a2 = a1 " <> x <> " a2"
              ]
      doc <- createDoc "Test.hs" "haskell" $ mkDoc "`_`"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc (Range (Position 5 16) (Position 5 19))
      chosen <- liftIO $ pickActionWithTitle "replace _ with foo" actions
      executeCodeAction chosen
      modifiedCode <- documentContents doc
      liftIO $ mkDoc "`foo`" @=? modifiedCode
  , testSession "postfix hole uses postfix notation of infix operator" $ do
      let mkDoc x = T.unlines
              [ "module Testing where"
              , "test :: Int -> Int -> Int"
              , "test a1 a2 = " <> x <> " a1 a2"
              ]
      doc <- createDoc "Test.hs" "haskell" $ mkDoc "_"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc (Range (Position 2 13) (Position 2 14))
      chosen <- liftIO $ pickActionWithTitle "replace _ with (+)" actions
      executeCodeAction chosen
      modifiedCode <- documentContents doc
      liftIO $ mkDoc "(+)" @=? modifiedCode
  , testSession "filling infix type hole uses infix operator" $ do
      let mkDoc x = T.unlines
              [ "module Testing where"
              , "test :: Int -> Int -> Int"
              , "test a1 a2 = a1 " <> x <> " a2"
              ]
      doc <- createDoc "Test.hs" "haskell" $ mkDoc "`_`"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc (Range (Position 2 16) (Position 2 19))
      chosen <- liftIO $ pickActionWithTitle "replace _ with (+)" actions
      executeCodeAction chosen
      modifiedCode <- documentContents doc
      liftIO $ mkDoc "+" @=? modifiedCode
  ]

addInstanceConstraintTests :: TestTree
addInstanceConstraintTests = let
  missingConstraintSourceCode :: Maybe T.Text -> T.Text
  missingConstraintSourceCode mConstraint =
    let constraint = maybe "" (<> " => ") mConstraint
     in T.unlines
    [ "module Testing where"
    , ""
    , "data Wrap a = Wrap a"
    , ""
    , "instance " <> constraint <> "Eq (Wrap a) where"
    , "  (Wrap x) == (Wrap y) = x == y"
    ]

  incompleteConstraintSourceCode :: Maybe T.Text -> T.Text
  incompleteConstraintSourceCode mConstraint =
    let constraint = maybe "Eq a" (\c -> "(Eq a, " <> c <> ")")  mConstraint
     in T.unlines
    [ "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "instance " <> constraint <> " => Eq (Pair a b) where"
    , "  (Pair x y) == (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCode2 :: Maybe T.Text -> T.Text
  incompleteConstraintSourceCode2 mConstraint =
    let constraint = maybe "(Eq a, Eq b)" (\c -> "(Eq a, Eq b, " <> c <> ")")  mConstraint
     in T.unlines
    [ "module Testing where"
    , ""
    , "data Three a b c = Three a b c"
    , ""
    , "instance " <> constraint <> " => Eq (Three a b c) where"
    , "  (Three x y z) == (Three x' y' z') = x == x' && y == y' && z == z'"
    ]

  check :: T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle originalCode expectedCode = testSession (T.unpack actionTitle) $ do
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getAllCodeActions doc
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode

  in testGroup "add instance constraint"
  [ check
    "Add `Eq a` to the context of the instance declaration"
    (missingConstraintSourceCode Nothing)
    (missingConstraintSourceCode $ Just "Eq a")
  , check
    "Add `Eq b` to the context of the instance declaration"
    (incompleteConstraintSourceCode Nothing)
    (incompleteConstraintSourceCode $ Just "Eq b")
  , check
    "Add `Eq c` to the context of the instance declaration"
    (incompleteConstraintSourceCode2 Nothing)
    (incompleteConstraintSourceCode2 $ Just "Eq c")
  ]

addFunctionConstraintTests :: TestTree
addFunctionConstraintTests = let
  missingConstraintSourceCode :: T.Text -> T.Text
  missingConstraintSourceCode constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "eq :: " <> constraint <> "a -> a -> Bool"
    , "eq x y = x == y"
    ]

  missingConstraintWithForAllSourceCode :: T.Text -> T.Text
  missingConstraintWithForAllSourceCode constraint =
    T.unlines
    [ "{-# LANGUAGE ExplicitForAll #-}"
    , "module Testing where"
    , ""
    , "eq :: forall a. " <> constraint <> "a -> a -> Bool"
    , "eq x y = x == y"
    ]

  incompleteConstraintWithForAllSourceCode :: T.Text -> T.Text
  incompleteConstraintWithForAllSourceCode constraint =
    T.unlines
    [ "{-# LANGUAGE ExplicitForAll #-}"
    , "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "eq :: " <> constraint <> " => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCode :: T.Text -> T.Text
  incompleteConstraintSourceCode constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "eq :: " <> constraint <> " => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCode2 :: T.Text -> T.Text
  incompleteConstraintSourceCode2 constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "data Three a b c = Three a b c"
    , ""
    , "eq :: " <> constraint <> " => Three a b c -> Three a b c -> Bool"
    , "eq (Three x y z) (Three x' y' z') = x == x' && y == y' && z == z'"
    ]

  incompleteConstraintSourceCodeWithExtraCharsInContext :: T.Text -> T.Text
  incompleteConstraintSourceCodeWithExtraCharsInContext constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "eq :: ( " <> constraint <> " ) => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCodeWithNewlinesInTypeSignature :: T.Text -> T.Text
  incompleteConstraintSourceCodeWithNewlinesInTypeSignature constraint =
    T.unlines
    [ "module Testing where"
    , "data Pair a b = Pair a b"
    , "eq "
    , "    :: (" <> constraint <> ")"
    , "    => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  missingMonadConstraint constraint = T.unlines
    [ "module Testing where"
    , "f :: " <> constraint <> "m ()"
    , "f = do "
    , "  return ()"
    ]

  in testGroup "add function constraint"
  [ checkCodeAction
    "no preexisting constraint"
    "Add `Eq a` to the context of the type signature for `eq`"
    (missingConstraintSourceCode "")
    (missingConstraintSourceCode "Eq a => ")
  , checkCodeAction
    "no preexisting constraint, with forall"
    "Add `Eq a` to the context of the type signature for `eq`"
    (missingConstraintWithForAllSourceCode "")
    (missingConstraintWithForAllSourceCode "Eq a => ")
  , checkCodeAction
    "preexisting constraint, no parenthesis"
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCode "Eq a")
    (incompleteConstraintSourceCode "(Eq a, Eq b)")
  , checkCodeAction
    "preexisting constraints in parenthesis"
    "Add `Eq c` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCode2 "(Eq a, Eq b)")
    (incompleteConstraintSourceCode2 "(Eq a, Eq b, Eq c)")
  , checkCodeAction
    "preexisting constraints with forall"
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintWithForAllSourceCode "Eq a")
    (incompleteConstraintWithForAllSourceCode "(Eq a, Eq b)")
  , checkCodeAction
    "preexisting constraint, with extra spaces in context"
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCodeWithExtraCharsInContext "Eq a")
    (incompleteConstraintSourceCodeWithExtraCharsInContext "Eq a, Eq b")
  , checkCodeAction
    "preexisting constraint, with newlines in type signature"
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCodeWithNewlinesInTypeSignature "Eq a")
    (incompleteConstraintSourceCodeWithNewlinesInTypeSignature "Eq a, Eq b")
  , checkCodeAction
    "missing Monad constraint"
    "Add `Monad m` to the context of the type signature for `f`"
    (missingMonadConstraint "")
    (missingMonadConstraint "Monad m => ")
  ]

checkCodeAction :: String -> T.Text -> T.Text -> T.Text -> TestTree
checkCodeAction testName actionTitle originalCode expectedCode = testSession testName $ do
  doc <- createDoc "Testing.hs" "haskell" originalCode
  _ <- waitForDiagnostics
  actionsOrCommands <- getAllCodeActions doc
  chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
  executeCodeAction chosenAction
  modifiedCode <- documentContents doc
  liftIO $ expectedCode @=? modifiedCode

addImplicitParamsConstraintTests :: TestTree
addImplicitParamsConstraintTests =
  testGroup
    "add missing implicit params constraints"
    [ testGroup
        "introduced"
        [ let ex ctxtA = exampleCode "?a" ctxtA ""
           in checkCodeAction "at top level" "Add ?a::() to the context of fBase" (ex "") (ex "?a::()"),
          let ex ctxA = exampleCode "x where x = ?a" ctxA ""
           in checkCodeAction "in nested def" "Add ?a::() to the context of fBase" (ex "") (ex "?a::()")
        ],
      testGroup
        "inherited"
        [ let ex = exampleCode "()" "?a::()"
           in checkCodeAction
                "with preexisting context"
                "Add `?a::()` to the context of the type signature for `fCaller`"
                (ex "Eq ()")
                (ex "Eq (), ?a::()"),
          let ex = exampleCode "()" "?a::()"
           in checkCodeAction "without preexisting context" "Add ?a::() to the context of fCaller" (ex "") (ex "?a::()")
        ]
    ]
  where
    mkContext ""       = ""
    mkContext contents = "(" <> contents <> ") => "

    exampleCode bodyBase contextBase contextCaller =
      T.unlines
        [ "{-# LANGUAGE FlexibleContexts, ImplicitParams #-}",
          "module Testing where",
          "fBase :: " <> mkContext contextBase <> "()",
          "fBase = " <> bodyBase,
          "fCaller :: " <> mkContext contextCaller <> "()",
          "fCaller = fBase"
        ]

removeRedundantConstraintsTests :: TestTree
removeRedundantConstraintsTests = let
  header =
    [ "{-# OPTIONS_GHC -Wredundant-constraints #-}"
    , "module Testing where"
    , ""
    ]

  headerExt :: [T.Text] -> [T.Text]
  headerExt exts =
    redunt : extTxt ++ ["module Testing where"]
    where
      redunt = "{-# OPTIONS_GHC -Wredundant-constraints #-}"
      extTxt = map (\ext -> "{-# LANGUAGE " <> ext <> " #-}") exts

  redundantConstraintsCode :: Maybe T.Text -> T.Text
  redundantConstraintsCode mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "foo :: " <> constraint <> "a -> a"
        , "foo = id"
        ]

  redundantMixedConstraintsCode :: Maybe T.Text -> T.Text
  redundantMixedConstraintsCode mConstraint =
    let constraint = maybe "(Num a, Eq a)" (\c -> "(Num a, Eq a, " <> c <> ")") mConstraint
      in T.unlines $ header <>
        [ "foo :: " <> constraint <> " => a -> Bool"
        , "foo x = x == 1"
        ]

  typeSignatureSpaces :: Maybe T.Text -> T.Text
  typeSignatureSpaces mConstraint =
    let constraint = maybe "(Num a, Eq a)" (\c -> "(Num a, Eq a, " <> c <> ")") mConstraint
      in T.unlines $ header <>
        [ "foo ::  " <> constraint <> " => a -> Bool"
        , "foo x = x == 1"
        ]

  redundantConstraintsForall :: Maybe T.Text -> T.Text
  redundantConstraintsForall mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ headerExt ["RankNTypes"] <>
        [ "foo :: forall a. " <> constraint <> "a -> a"
        , "foo = id"
        ]

  typeSignatureDo :: Maybe T.Text -> T.Text
  typeSignatureDo mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "f :: Int -> IO ()"
        , "f n = do"
        , "  let foo :: " <> constraint <> "a -> IO ()"
        , "      foo _ = return ()"
        , "  r n"
        ]

  typeSignatureNested :: Maybe T.Text -> T.Text
  typeSignatureNested mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "f :: Int -> ()"
        , "f = g"
        , "  where"
        , "    g :: " <> constraint <> "a -> ()"
        , "    g _ = ()"
        ]

  typeSignatureNested' :: Maybe T.Text -> T.Text
  typeSignatureNested' mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "f :: Int -> ()"
        , "f ="
        , "  let"
        , "    g :: Int -> ()"
        , "    g = h"
        , "      where"
        , "        h :: " <> constraint <> "a -> ()"
        , "        h _ = ()"
        , "  in g"
        ]

  typeSignatureNested'' :: Maybe T.Text -> T.Text
  typeSignatureNested'' mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "f :: Int -> ()"
        , "f = g"
        , "  where"
        , "    g :: Int -> ()"
        , "    g = "
        , "      let"
        , "        h :: " <> constraint <> "a -> ()"
        , "        h _ = ()"
        , "      in h"
        ]

  typeSignatureLined1 = T.unlines $ header <>
    [ "foo :: Eq a =>"
    , "  a -> Bool"
    , "foo _ = True"
    ]

  typeSignatureLined2 = T.unlines $ header <>
    [ "foo :: (Eq a, Show a)"
    , "  => a -> Bool"
    , "foo _ = True"
    ]

  typeSignatureOneLine = T.unlines $ header <>
    [ "foo :: a -> Bool"
    , "foo _ = True"
    ]

  typeSignatureLined3 = T.unlines $ header <>
    [ "foo :: ( Eq a"
    , "       , Show a"
    , "       )"
    , "    => a -> Bool"
    , "foo x = x == x"
    ]

  typeSignatureLined3' = T.unlines $ header <>
    [ "foo :: ( Eq a"
    , "       )"
    , "    => a -> Bool"
    , "foo x = x == x"
    ]


  check :: T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle originalCode expectedCode = testSession (T.unpack actionTitle) $ do
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getAllCodeActions doc
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode

  in testGroup "remove redundant function constraints"
  [ check
    "Remove redundant constraint `Eq a` from the context of the type signature for `foo`"
    (redundantConstraintsCode $ Just "Eq a")
    (redundantConstraintsCode Nothing)
  , check
    "Remove redundant constraints `(Eq a, Monoid a)` from the context of the type signature for `foo`"
    (redundantConstraintsCode $ Just "(Eq a, Monoid a)")
    (redundantConstraintsCode Nothing)
  , check
    "Remove redundant constraints `(Monoid a, Show a)` from the context of the type signature for `foo`"
    (redundantMixedConstraintsCode $ Just "Monoid a, Show a")
    (redundantMixedConstraintsCode Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `g`"
    (typeSignatureNested $ Just "Eq a")
    (typeSignatureNested Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `h`"
    (typeSignatureNested' $ Just "Eq a")
    (typeSignatureNested' Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `h`"
    (typeSignatureNested'' $ Just "Eq a")
    (typeSignatureNested'' Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `foo`"
    (redundantConstraintsForall $ Just "Eq a")
    (redundantConstraintsForall Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `foo`"
    (typeSignatureDo $ Just "Eq a")
    (typeSignatureDo Nothing)
  , check
    "Remove redundant constraints `(Monoid a, Show a)` from the context of the type signature for `foo`"
    (typeSignatureSpaces $ Just "Monoid a, Show a")
    (typeSignatureSpaces Nothing)
    , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `foo`"
    typeSignatureLined1
    typeSignatureOneLine
    , check
    "Remove redundant constraints `(Eq a, Show a)` from the context of the type signature for `foo`"
    typeSignatureLined2
    typeSignatureOneLine
    , check
    "Remove redundant constraint `Show a` from the context of the type signature for `foo`"
    typeSignatureLined3
    typeSignatureLined3'
  ]

addSigActionTests :: TestTree
addSigActionTests = let
  header = [ "{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-pattern-synonym-signatures #-}"
           , "{-# LANGUAGE PatternSynonyms,BangPatterns,GADTs #-}"
           , "module Sigs where"
           , "data T1 a where"
           , "  MkT1 :: (Show b) => a -> b -> T1 a"
           ]
  before def     = T.unlines $ header ++ [def]
  after' def sig = T.unlines $ header ++ [sig, def]

  def >:: sig = testSession (T.unpack $ T.replace "\n" "\\n" def) $ do
    let originalCode = before def
    let expectedCode = after' def sig
    doc <- createDoc "Sigs.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 5 1) (Position 5 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle ("add signature: " <> sig) actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "add signature"
    [ "abc = True"              >:: "abc :: Bool"
    , "foo a b = a + b"         >:: "foo :: Num a => a -> a -> a"
    , "bar a b = show $ a + b"  >:: "bar :: (Show a, Num a) => a -> a -> String"
    , "(!!!) a b = a > b"       >:: "(!!!) :: Ord a => a -> a -> Bool"
    , "a >>>> b = a + b"        >:: "(>>>>) :: Num a => a -> a -> a"
    , "a `haha` b = a b"        >:: "haha :: (t1 -> t2) -> t1 -> t2"
    , "pattern Some a = Just a" >:: "pattern Some :: a -> Maybe a"
    , "pattern Some a <- Just a" >:: "pattern Some :: a -> Maybe a"
    , "pattern Some a <- Just a\n  where Some a = Just a" >:: "pattern Some :: a -> Maybe a"
    , "pattern Some a <- Just !a\n  where Some !a = Just a" >:: "pattern Some :: a -> Maybe a"
    , "pattern Point{x, y} = (x, y)" >:: "pattern Point :: a -> b -> (a, b)"
    , "pattern Point{x, y} <- (x, y)" >:: "pattern Point :: a -> b -> (a, b)"
    , "pattern Point{x, y} <- (x, y)\n  where Point x y = (x, y)" >:: "pattern Point :: a -> b -> (a, b)"
    , "pattern MkT1' b = MkT1 42 b" >:: "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a"
    , "pattern MkT1' b <- MkT1 42 b" >:: "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a"
    , "pattern MkT1' b <- MkT1 42 b\n  where MkT1' b = T1 42 b" >:: "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a"
    ]

exportUnusedTests :: TestTree
exportUnusedTests = testGroup "export unused actions"
  [ testGroup "don't want suggestion"
    [ testSession "implicit exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# OPTIONS_GHC -Wmissing-signatures #-}"
              , "module A where"
              , "foo = id"])
        (R 3 0 3 3)
        "Export ‘foo’"
        Nothing -- codeaction should not be available
    , testSession "not top-level" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# OPTIONS_GHC -Wunused-binds #-}"
              , "module A (foo,bar) where"
              , "foo = ()"
              , "  where bar = ()"
              , "bar = ()"])
        (R 2 0 2 11)
        "Export ‘bar’"
        Nothing
    , testSession "type is exported but not the constructor of same name" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo) where"
              , "data Foo = Foo"])
        (R 2 0 2 8)
        "Export ‘Foo’"
        Nothing -- codeaction should not be available
    , testSession "unused data field" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(Foo)) where"
              , "data Foo = Foo {foo :: ()}"])
        (R 2 0 2 20)
        "Export ‘foo’"
        Nothing -- codeaction should not be available
    ]
  , testGroup "want suggestion"
    [ testSession "empty exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A ("
              , ") where"
              , "foo = id"])
        (R 3 0 3 3)
        "Export ‘foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A ("
              , "foo) where"
              , "foo = id"])
    , testSession "single line explicit exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (foo) where"
              , "foo = id"
              , "bar = foo"])
        (R 3 0 3 3)
        "Export ‘bar’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (foo,bar) where"
              , "foo = id"
              , "bar = foo"])
    , testSession "multi line explicit exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ("
              , "    foo) where"
              , "foo = id"
              , "bar = foo"])
        (R 5 0 5 3)
        "Export ‘bar’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ("
              , "    foo,bar) where"
              , "foo = id"
              , "bar = foo"])
    , testSession "export list ends in comma" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  (foo,"
              , "  ) where"
              , "foo = id"
              , "bar = foo"])
        (R 5 0 5 3)
        "Export ‘bar’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  (foo,"
              , "  bar) where"
              , "foo = id"
              , "bar = foo"])
    , testSession "unused pattern synonym" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE PatternSynonyms #-}"
              , "module A () where"
              , "pattern Foo a <- (a, _)"])
        (R 3 0 3 10)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE PatternSynonyms #-}"
              , "module A (pattern Foo) where"
              , "pattern Foo a <- (a, _)"])
    , testSession "unused data type" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "data Foo = Foo"])
        (R 2 0 2 7)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(..)) where"
              , "data Foo = Foo"])
    , testSession "unused newtype" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "newtype Foo = Foo ()"])
        (R 2 0 2 10)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(..)) where"
              , "newtype Foo = Foo ()"])
    , testSession "unused type synonym" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "type Foo = ()"])
        (R 2 0 2 7)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo) where"
              , "type Foo = ()"])
    , testSession "unused type family" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "module A () where"
              , "type family Foo p"])
        (R 3 0 3 15)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "module A (Foo(..)) where"
              , "type family Foo p"])
    , testSession "unused typeclass" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "class Foo a"])
        (R 2 0 2 8)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(..)) where"
              , "class Foo a"])
    , testSession "infix" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "a `f` b = ()"])
        (R 2 0 2 11)
        "Export ‘f’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (f) where"
              , "a `f` b = ()"])
    , testSession "function operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "(<|) = ($)"])
        (R 2 0 2 9)
        "Export ‘<|’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A ((<|)) where"
              , "(<|) = ($)"])
    , testSession "type synonym operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "type (:<) = ()"])
        (R 3 0 3 13)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A ((:<)) where"
              , "type (:<) = ()"])
    , testSession "type family operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "type family (:<)"])
        (R 4 0 4 15)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "type family (:<)"])
    , testSession "typeclass operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "class (:<) a"])
        (R 3 0 3 11)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "class (:<) a"])
    , testSession "newtype operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "newtype (:<) = Foo ()"])
        (R 3 0 3 20)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "newtype (:<) = Foo ()"])
    , testSession "data type operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "data (:<) = Foo ()"])
        (R 3 0 3 17)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "data (:<) = Foo ()"])
    ]
  ]
  where
    template doc range = exportTemplate (Just range) doc

exportTemplate :: Maybe Range -> T.Text -> T.Text -> Maybe T.Text -> Session ()
exportTemplate mRange initialContent expectedAction expectedContents = do
  doc <- createDoc "A.hs" "haskell" initialContent
  _ <- waitForDiagnostics
  actions <- case mRange of
    Nothing    -> getAllCodeActions doc
    Just range -> getCodeActions doc range
  case expectedContents of
    Just content -> do
      action <- liftIO $ pickActionWithTitle expectedAction actions
      executeCodeAction action
      contentAfterAction <- documentContents doc
      liftIO $ content @=? contentAfterAction
    Nothing ->
      liftIO $ [_title | InR CodeAction{_title} <- actions, _title == expectedAction ] @?= []

removeExportTests :: TestTree
removeExportTests = testGroup "remove export actions"
    [ testSession "single export" $ template
        (T.unlines
              [ "module A (  a   ) where"
              , "b :: ()"
              , "b = ()"])
        "Remove ‘a’ from export"
        (Just $ T.unlines
              [ "module A (     ) where"
              , "b :: ()"
              , "b = ()"])
    , testSession "ending comma" $ template
        (T.unlines
              [ "module A (  a,   ) where"
              , "b :: ()"
              , "b = ()"])
        "Remove ‘a’ from export"
        (Just $ T.unlines
              [ "module A (  ) where"
              , "b :: ()"
              , "b = ()"])
    , testSession "multiple exports" $ template
        (T.unlines
              [ "module A (a  ,   c,    b ) where"
              , "a, c :: ()"
              , "a = ()"
              , "c = ()"])
        "Remove ‘b’ from export"
        (Just $ T.unlines
              [ "module A (a  ,   c ) where"
              , "a, c :: ()"
              , "a = ()"
              , "c = ()"])
    , testSession "not in scope constructor" $ template
        (T.unlines
              [ "module A (A (X,Y,Z,(:<)), ab) where"
              , "data A = X Int | Y | (:<) Int"
              , "ab :: ()"
              , "ab = ()"
              ])
        "Remove ‘Z’ from export"
        (Just $ T.unlines
              [ "module A (A (X,Y,(:<)), ab) where"
              , "data A = X Int | Y | (:<) Int"
              , "ab :: ()"
              , "ab = ()"])
    , testSession "multiline export" $ template
        (T.unlines
              [ "module A (a"
              , " ,  b"
              , " , (:*:)"
              , " , ) where"
              , "a,b :: ()"
              , "a = ()"
              , "b = ()"])
        "Remove ‘:*:’ from export"
        (Just $ T.unlines
              [ "module A (a"
              , " ,  b"
              , " "
              , " , ) where"
              , "a,b :: ()"
              , "a = ()"
              , "b = ()"])
    , testSession "qualified re-export" $ template
        (T.unlines
              [ "module A (M.x,a) where"
              , "import qualified Data.List as M"
              , "a :: ()"
              , "a = ()"])
        "Remove ‘M.x’ from export"
        (Just $ T.unlines
              [ "module A (a) where"
              , "import qualified Data.List as M"
              , "a :: ()"
              , "a = ()"])
    , testSession "export module" $ template
        (T.unlines
              [ "module A (module B) where"
              , "a :: ()"
              , "a = ()"])
        "Remove ‘module B’ from export"
        (Just $ T.unlines
              [ "module A () where"
              , "a :: ()"
              , "a = ()"])
    , testSession "dodgy export" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A (A (..)) where"
              , "data X = X"
              , "type A = X"])
        "Remove ‘A(..)’ from export"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A () where"
              , "data X = X"
              , "type A = X"])
    , testSession "dodgy export" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A (A (..)) where"
              , "data X = X"
              , "type A = X"])
        "Remove ‘A(..)’ from export"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A () where"
              , "data X = X"
              , "type A = X"])
    , testSession "duplicate module export" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A (module L,module L) where"
              , "import Data.List as L"
              , "a :: ()"
              , "a = ()"])
        "Remove ‘Module L’ from export"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A (module L) where"
              , "import Data.List as L"
              , "a :: ()"
              , "a = ()"])
    , testSession "remove all exports single" $ template
        (T.unlines
              [ "module A (x) where"
              , "a :: ()"
              , "a = ()"])
        "Remove all redundant exports"
        (Just $ T.unlines
              [ "module A () where"
              , "a :: ()"
              , "a = ()"])
    , testSession "remove all exports two" $ template
        (T.unlines
              [ "module A (x,y) where"
              , "a :: ()"
              , "a = ()"])
        "Remove all redundant exports"
        (Just $ T.unlines
              [ "module A () where"
              , "a :: ()"
              , "a = ()"])
    , testSession "remove all exports three" $ template
        (T.unlines
              [ "module A (a,x,y) where"
              , "a :: ()"
              , "a = ()"])
        "Remove all redundant exports"
        (Just $ T.unlines
              [ "module A (a) where"
              , "a :: ()"
              , "a = ()"])
    , testSession "remove all exports composite" $ template
        (T.unlines
              [ "module A (x,y,b, module Ls, a, A(X,getW, Y, Z,(:-),getV), (-+), B(B)) where"
              , "data A = X {getV :: Int} | Y {getV :: Int}"
              , "data B = B"
              , "a,b :: ()"
              , "a = ()"
              , "b = ()"])
        "Remove all redundant exports"
        (Just $ T.unlines
              [ "module A (b, a, A(X, Y,getV), B(B)) where"
              , "data A = X {getV :: Int} | Y {getV :: Int}"
              , "data B = B"
              , "a,b :: ()"
              , "a = ()"
              , "b = ()"])
    ]
  where
    template = exportTemplate Nothing

addSigLensesTests :: TestTree
addSigLensesTests =
  let pragmas = "{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-pattern-synonym-signatures #-}"
      moduleH exported =
        T.unlines
          [ "{-# LANGUAGE PatternSynonyms,TypeApplications,DataKinds,RankNTypes,ScopedTypeVariables,TypeOperators,GADTs,BangPatterns #-}"
          , "module Sigs(" <> exported <> ") where"
          , "import qualified Data.Complex as C"
          , "import Data.Data (Proxy (..), type (:~:) (..), mkCharType)"
          , "data T1 a where"
          , "  MkT1 :: (Show b) => a -> b -> T1 a"
          ]
      before enableGHCWarnings exported (def, _) others =
        T.unlines $ [pragmas | enableGHCWarnings] <> [moduleH exported, def] <> others
      after' enableGHCWarnings exported (def, sig) others =
        T.unlines $ [pragmas | enableGHCWarnings] <> [moduleH exported] <> maybe [] pure sig <> [def] <> others
      createConfig mode = A.object ["haskell" A..= A.object ["plugin" A..= A.object ["ghcide-type-lenses" A..= A.object ["config" A..= A.object ["mode" A..= A.String mode]]]]]
      sigSession testName enableGHCWarnings mode exported def others = testSession testName $ do
        let originalCode = before enableGHCWarnings exported def others
        let expectedCode = after' enableGHCWarnings exported def others
        sendNotification SWorkspaceDidChangeConfiguration $ DidChangeConfigurationParams $ createConfig mode
        doc <- createDoc "Sigs.hs" "haskell" originalCode
        waitForProgressDone
        codeLenses <- getCodeLenses doc
        if not $ null $ snd def
          then do
            liftIO $ length codeLenses == 1 @? "Expected 1 code lens, but got: " <> show codeLenses
            executeCommand $ fromJust $ head codeLenses ^. L.command
            modifiedCode <- skipManyTill anyMessage (getDocumentEdit doc)
            liftIO $ expectedCode @=? modifiedCode
          else liftIO $ null codeLenses @? "Expected no code lens, but got: " <> show codeLenses
      cases =
        [ ("abc = True", "abc :: Bool")
        , ("foo a b = a + b", "foo :: Num a => a -> a -> a")
        , ("bar a b = show $ a + b", "bar :: (Show a, Num a) => a -> a -> String")
        , ("(!!!) a b = a > b", "(!!!) :: Ord a => a -> a -> Bool")
        , ("a >>>> b = a + b", "(>>>>) :: Num a => a -> a -> a")
        , ("a `haha` b = a b", "haha :: (t1 -> t2) -> t1 -> t2")
        , ("pattern Some a = Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Some a <- Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Some a <- Just a\n  where Some a = Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Some a <- Just !a\n  where Some !a = Just a", "pattern Some :: a -> Maybe a")
        , ("pattern Point{x, y} = (x, y)", "pattern Point :: a -> b -> (a, b)")
        , ("pattern Point{x, y} <- (x, y)", "pattern Point :: a -> b -> (a, b)")
        , ("pattern Point{x, y} <- (x, y)\n  where Point x y = (x, y)", "pattern Point :: a -> b -> (a, b)")
        , ("pattern MkT1' b = MkT1 42 b", "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a")
        , ("pattern MkT1' b <- MkT1 42 b", "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a")
        , ("pattern MkT1' b <- MkT1 42 b\n  where MkT1' b = T1 42 b", "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a")
        , ("qualifiedSigTest= C.realPart", "qualifiedSigTest :: C.Complex a -> a")
        , ("head = 233", "head :: Integer")
        , ("rank2Test (k :: forall a . a -> a) = (k 233 :: Int, k \"QAQ\")", "rank2Test :: (forall a. a -> a) -> (Int, " <> listOfChar <> ")")
        , ("symbolKindTest = Proxy @\"qwq\"", "symbolKindTest :: Proxy \"qwq\"")
        , ("promotedKindTest = Proxy @Nothing", "promotedKindTest :: Proxy 'Nothing")
        , ("typeOperatorTest = Refl", "typeOperatorTest :: a :~: a")
        , ("notInScopeTest = mkCharType", "notInScopeTest :: String -> Data.Data.DataType")
        ]
   in testGroup
        "add signature"
        [ testGroup "signatures are correct" [sigSession (T.unpack $ T.replace "\n" "\\n" def) False "always" "" (def, Just sig) [] | (def, sig) <- cases]
        , sigSession "exported mode works" False "exported" "xyz" ("xyz = True", Just "xyz :: Bool") (fst <$> take 3 cases)
        , testGroup
            "diagnostics mode works"
            [ sigSession "with GHC warnings" True "diagnostics" "" (second Just $ head cases) []
            , sigSession "without GHC warnings" False "diagnostics" "" (second (const Nothing) $ head cases) []
            ]
        ]

linkToLocation :: [LocationLink] -> [Location]
linkToLocation = map (\LocationLink{_targetUri,_targetRange} -> Location _targetUri _targetRange)

checkDefs :: [Location] |? [LocationLink] -> Session [Expect] -> Session ()
checkDefs (either id linkToLocation . toEither -> defs) mkExpectations = traverse_ check =<< mkExpectations where
  check (ExpectRange expectedRange) = do
    assertNDefinitionsFound 1 defs
    assertRangeCorrect (head defs) expectedRange
  check (ExpectLocation expectedLocation) = do
    assertNDefinitionsFound 1 defs
    liftIO $ do
      canonActualLoc <- canonicalizeLocation (head defs)
      canonExpectedLoc <- canonicalizeLocation expectedLocation
      canonActualLoc @?= canonExpectedLoc
  check ExpectNoDefinitions = do
    assertNDefinitionsFound 0 defs
  check ExpectExternFail = liftIO $ assertFailure "Expecting to fail to find in external file"
  check _ = pure () -- all other expectations not relevant to getDefinition

  assertNDefinitionsFound :: Int -> [a] -> Session ()
  assertNDefinitionsFound n defs = liftIO $ assertEqual "number of definitions" n (length defs)

  assertRangeCorrect Location{_range = foundRange} expectedRange =
    liftIO $ expectedRange @=? foundRange

canonicalizeLocation :: Location -> IO Location
canonicalizeLocation (Location uri range) = Location <$> canonicalizeUri uri <*> pure range

findDefinitionAndHoverTests :: TestTree
findDefinitionAndHoverTests = let

  tst :: (TextDocumentIdentifier -> Position -> Session a, a -> Session [Expect] -> Session ()) -> Position -> Session [Expect] -> String -> TestTree
  tst (get, check) pos targetRange title = testSessionWithExtraFiles "hover" title $ \dir -> do

    -- Dirty the cache to check that definitions work even in the presence of iface files
    liftIO $ runInDir dir $ do
      let fooPath = dir </> "Foo.hs"
      fooSource <- liftIO $ readFileUtf8 fooPath
      fooDoc <- createDoc fooPath "haskell" fooSource
      _ <- getHover fooDoc $ Position 4 3
      closeDoc fooDoc

    doc <- openTestDataDoc (dir </> sourceFilePath)
    waitForProgressDone
    found <- get doc pos
    check found targetRange



  checkHover :: Maybe Hover -> Session [Expect] -> Session ()
  checkHover hover expectations = traverse_ check =<< expectations where

    check expected =
      case hover of
        Nothing -> unless (expected == ExpectNoHover) $ liftIO $ assertFailure "no hover found"
        Just Hover{_contents = (HoverContents MarkupContent{_value = standardizeQuotes -> msg})
                  ,_range    = rangeInHover } ->
          case expected of
            ExpectRange  expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverRange expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverText snippets -> liftIO $ traverse_ (`assertFoundIn` msg) snippets
            ExpectNoHover -> liftIO $ assertFailure $ "Expected no hover but got " <> show hover
            _ -> pure () -- all other expectations not relevant to hover
        _ -> liftIO $ assertFailure $ "test not expecting this kind of hover info" <> show hover

  extractLineColFromHoverMsg :: T.Text -> [T.Text]
  extractLineColFromHoverMsg = T.splitOn ":" . head . T.splitOn "*" . last . T.splitOn (sourceFileName <> ":")

  checkHoverRange :: Range -> Maybe Range -> T.Text -> Session ()
  checkHoverRange expectedRange rangeInHover msg =
    let
      lineCol = extractLineColFromHoverMsg msg
      -- looks like hovers use 1-based numbering while definitions use 0-based
      -- turns out that they are stored 1-based in RealSrcLoc by GHC itself.
      adjust Position{_line = l, _character = c} =
        Position{_line = l + 1, _character = c + 1}
    in
    case map (read . T.unpack) lineCol of
      [l,c] -> liftIO $ adjust (_start expectedRange) @=? Position l c
      _     -> liftIO $ assertFailure $
        "expected: " <> show ("[...]" <> sourceFileName <> ":<LINE>:<COL>**[...]", Just expectedRange) <>
        "\n but got: " <> show (msg, rangeInHover)

  assertFoundIn :: T.Text -> T.Text -> Assertion
  assertFoundIn part whole = assertBool
    (T.unpack $ "failed to find: `" <> part <> "` in hover message:\n" <> whole)
    (part `T.isInfixOf` whole)

  sourceFilePath = T.unpack sourceFileName
  sourceFileName = "GotoHover.hs"

  mkFindTests tests = testGroup "get"
    [ testGroup "definition" $ mapMaybe fst tests
    , testGroup "hover"      $ mapMaybe snd tests
    , checkFileCompiles sourceFilePath $
        expectDiagnostics
          [ ( "GotoHover.hs", [(DsError, (62, 7), "Found hole: _")]) ]
    , testGroup "type-definition" typeDefinitionTests ]

  typeDefinitionTests = [ tst (getTypeDefinitions, checkDefs) aaaL14 (pure tcData) "Saturated data con"
                        , tst (getTypeDefinitions, checkDefs) aL20 (pure [ExpectNoDefinitions]) "Polymorphic variable"]

  test runDef runHover look expect = testM runDef runHover look (return expect)

  testM runDef runHover look expect title =
    ( runDef   $ tst def   look expect title
    , runHover $ tst hover look expect title ) where
      def   = (getDefinitions, checkDefs)
      hover = (getHover      , checkHover)

  -- search locations            expectations on results
  fffL4  = _start fffR     ;  fffR = mkRange 8  4    8  7 ; fff  = [ExpectRange fffR]
  fffL8  = Position 12  4  ;
  fffL14 = Position 18  7  ;
  aL20   = Position 19 15
  aaaL14 = Position 18 20  ;  aaa    = [mkR  11  0   11  3]
  dcL7   = Position 11 11  ;  tcDC   = [mkR   7 23    9 16]
  dcL12  = Position 16 11  ;
  xtcL5  = Position  9 11  ;  xtc    = [ExpectExternFail,   ExpectHoverText ["Int", "Defined in ", "GHC.Types", "ghc-prim"]]
  tcL6   = Position 10 11  ;  tcData = [mkR   7  0    9 16, ExpectHoverText ["TypeConstructor", "GotoHover.hs:8:1"]]
  vvL16  = Position 20 12  ;  vv     = [mkR  20  4   20  6]
  opL16  = Position 20 15  ;  op     = [mkR  21  2   21  4]
  opL18  = Position 22 22  ;  opp    = [mkR  22 13   22 17]
  aL18   = Position 22 20  ;  apmp   = [mkR  22 10   22 11]
  b'L19  = Position 23 13  ;  bp     = [mkR  23  6   23  7]
  xvL20  = Position 24  8  ;  xvMsg  = [ExpectExternFail,   ExpectHoverText ["pack", ":: String -> Text", "Data.Text", "text"]]
  clL23  = Position 27 11  ;  cls    = [mkR  25  0   26 20, ExpectHoverText ["MyClass", "GotoHover.hs:26:1"]]
  clL25  = Position 29  9
  eclL15 = Position 19  8  ;  ecls   = [ExpectExternFail, ExpectHoverText ["Num", "Defined in ", "GHC.Num", "base"]]
  dnbL29 = Position 33 18  ;  dnb    = [ExpectHoverText [":: ()"],   mkR  33 12   33 21]
  dnbL30 = Position 34 23
  lcbL33 = Position 37 26  ;  lcb    = [ExpectHoverText [":: Char"], mkR  37 26   37 27]
  lclL33 = Position 37 22
  mclL36 = Position 40  1  ;  mcl    = [mkR  40  0   40 14]
  mclL37 = Position 41  1
  spaceL37 = Position 41  24 ; space = [ExpectNoDefinitions, ExpectHoverText [":: Char"]]
  docL41 = Position 45  1  ;  doc    = [ExpectHoverText ["Recognizable docs: kpqz"]]
                           ;  constr = [ExpectHoverText ["Monad m"]]
  eitL40 = Position 44 28  ;  kindE  = [ExpectHoverText [":: * -> * -> *\n"]]
  intL40 = Position 44 34  ;  kindI  = [ExpectHoverText [":: *\n"]]
  tvrL40 = Position 44 37  ;  kindV  = [ExpectHoverText [":: * -> *\n"]]
  intL41 = Position 45 20  ;  litI   = [ExpectHoverText ["7518"]]
  chrL36 = Position 41 24  ;  litC   = [ExpectHoverText ["'f'"]]
  txtL8  = Position 12 14  ;  litT   = [ExpectHoverText ["\"dfgy\""]]
  lstL43 = Position 47 12  ;  litL   = [ExpectHoverText ["[8391 :: Int, 6268]"]]
  outL45 = Position 49  3  ;  outSig = [ExpectHoverText ["outer", "Bool"], mkR 46 0 46 5]
  innL48 = Position 52  5  ;  innSig = [ExpectHoverText ["inner", "Char"], mkR 49 2 49 7]
  holeL60 = Position 62 7  ;  hleInfo = [ExpectHoverText ["_ ::"]]
  cccL17 = Position 17 16  ;  docLink = [ExpectHoverText ["[Documentation](file:///"]]
  imported = Position 56 13 ; importedSig = getDocUri "Foo.hs" >>= \foo -> return [ExpectHoverText ["foo", "Foo", "Haddock"], mkL foo 5 0 5 3]
  reexported = Position 55 14 ; reexportedSig = getDocUri "Bar.hs" >>= \bar -> return [ExpectHoverText ["Bar", "Bar", "Haddock"], mkL bar 3 0 3 14]
  thLocL57 = Position 59 10 ; thLoc = [ExpectHoverText ["Identity"]]
  in
  mkFindTests
  --      def    hover  look       expect
  [
    if ghcVersion >= GHC90 then
        -- It suggests either going to the constructor or to the field
        test  broken yes    fffL4      fff           "field in record definition"
    else
        test  yes    yes    fffL4      fff           "field in record definition"
  , test  yes    yes    fffL8      fff           "field in record construction    #1102"
  , test  yes    yes    fffL14     fff           "field name used as accessor"           -- https://github.com/haskell/ghcide/pull/120 in Calculate.hs
  , test  yes    yes    aaaL14     aaa           "top-level name"                        -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    dcL7       tcDC          "data constructor record         #1029"
  , test  yes    yes    dcL12      tcDC          "data constructor plain"                -- https://github.com/haskell/ghcide/pull/121
  , test  yes    yes    tcL6       tcData        "type constructor                #1028" -- https://github.com/haskell/ghcide/pull/147
  , test  broken yes    xtcL5      xtc           "type constructor external   #717,1028"
  , test  broken yes    xvL20      xvMsg         "value external package           #717" -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    vvL16      vv            "plain parameter"                       -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    aL18       apmp          "pattern match name"                    -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    opL16      op            "top-level operator               #713" -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    opL18      opp           "parameter operator"                    -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    b'L19      bp            "name in backticks"                     -- https://github.com/haskell/ghcide/pull/120
  , test  yes    yes    clL23      cls           "class in instance declaration   #1027"
  , test  yes    yes    clL25      cls           "class in signature              #1027" -- https://github.com/haskell/ghcide/pull/147
  , test  broken yes    eclL15     ecls          "external class in signature #717,1027"
  , test  yes    yes    dnbL29     dnb           "do-notation   bind              #1073"
  , test  yes    yes    dnbL30     dnb           "do-notation lookup"
  , test  yes    yes    lcbL33     lcb           "listcomp   bind                 #1073"
  , test  yes    yes    lclL33     lcb           "listcomp lookup"
  , test  yes    yes    mclL36     mcl           "top-level fn 1st clause"
  , test  yes    yes    mclL37     mcl           "top-level fn 2nd clause         #1030"
  , if ghcVersion >= GHC810 then
        test  yes    yes    spaceL37   space         "top-level fn on space           #1002"
    else
        test  yes    broken spaceL37   space         "top-level fn on space           #1002"
  , test  no     yes    docL41     doc           "documentation                   #1129"
  , test  no     yes    eitL40     kindE         "kind of Either                  #1017"
  , test  no     yes    intL40     kindI         "kind of Int                     #1017"
  , test  no     broken tvrL40     kindV         "kind of (* -> *) type variable  #1017"
  , test  no     broken intL41     litI          "literal Int  in hover info      #1016"
  , test  no     broken chrL36     litC          "literal Char in hover info      #1016"
  , test  no     broken txtL8      litT          "literal Text in hover info      #1016"
  , test  no     broken lstL43     litL          "literal List in hover info      #1016"
  , if ghcVersion >= GHC90 then
        test  no     yes    docL41     constr        "type constraint in hover info   #1012"
    else
        test  no     broken docL41     constr        "type constraint in hover info   #1012"
  , test  broken broken outL45     outSig        "top-level signature              #767"
  , test  broken broken innL48     innSig        "inner     signature              #767"
  , test  no     yes    holeL60    hleInfo       "hole without internal name       #831"
  , test  no     skip   cccL17     docLink       "Haddock html links"
  , testM yes    yes    imported   importedSig   "Imported symbol"
  , testM yes    yes    reexported reexportedSig "Imported symbol (reexported)"
  , if ghcVersion == GHC90 && isWindows then
        test  no     broken    thLocL57   thLoc         "TH Splice Hover"
    else
        test  no     yes       thLocL57   thLoc         "TH Splice Hover"
  ]
  where yes, broken :: (TestTree -> Maybe TestTree)
        yes    = Just -- test should run and pass
        broken = Just . (`xfail` "known broken")
        no = const Nothing -- don't run this test at all
        skip = const Nothing -- unreliable, don't run

checkFileCompiles :: FilePath -> Session () -> TestTree
checkFileCompiles fp diag =
  testSessionWithExtraFiles "hover" ("Does " ++ fp ++ " compile") $ \dir -> do
    void (openTestDataDoc (dir </> fp))
    diag

pluginSimpleTests :: TestTree
pluginSimpleTests =
  ignoreInWindowsForGHC88And810 $
#if __GLASGOW_HASKELL__ == 810 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 5
  expectFailBecause "known broken for ghc 8.10.5 (see GHC #19763)" $
#endif
  testSessionWithExtraFiles "plugin-knownnat" "simple plugin" $ \dir -> do
    _ <- openDoc (dir </> "KnownNat.hs") "haskell"
    liftIO $ writeFile (dir</>"hie.yaml")
      "cradle: {cabal: [{path: '.', component: 'lib:plugin'}]}"

    expectDiagnostics
      [ ( "KnownNat.hs",
          [(DsError, (9, 15), "Variable not in scope: c")]
          )
      ]

pluginParsedResultTests :: TestTree
pluginParsedResultTests =
  ignoreInWindowsForGHC88And810 $
  testSessionWithExtraFiles "plugin-recorddot" "parsedResultAction plugin" $ \dir -> do
    _ <- openDoc (dir</> "RecordDot.hs") "haskell"
    expectNoMoreDiagnostics 2

cppTests :: TestTree
cppTests =
  testGroup "cpp"
    [ ignoreInWindowsBecause "Throw a lsp session time out in windows for ghc-8.8 and is broken for other versions" $ testCase "cpp-error" $ do
        let content =
              T.unlines
                [ "{-# LANGUAGE CPP #-}",
                  "module Testing where",
                  "#ifdef FOO",
                  "foo = 42"
                ]
        -- The error locations differ depending on which C-preprocessor is used.
        -- Some give the column number and others don't (hence -1). Assert either
        -- of them.
        (run $ expectError content (2, -1))
          `catch` ( \e -> do
                      let _ = e :: HUnitFailure
                      run $ expectError content (2, 1)
                  )
    , testSessionWait "cpp-ghcide" $ do
        _ <- createDoc "A.hs" "haskell" $ T.unlines
          ["{-# LANGUAGE CPP #-}"
          ,"main ="
          ,"#ifdef __GHCIDE__"
          ,"  worked"
          ,"#else"
          ,"  failed"
          ,"#endif"
          ]
        expectDiagnostics [("A.hs", [(DsError, (3, 2), "Variable not in scope: worked")])]
    ]
  where
    expectError :: T.Text -> Cursor -> Session ()
    expectError content cursor = do
      _ <- createDoc "Testing.hs" "haskell" content
      expectDiagnostics
        [ ( "Testing.hs",
            [(DsError, cursor, "error: unterminated")]
          )
        ]
      expectNoMoreDiagnostics 0.5

preprocessorTests :: TestTree
preprocessorTests = testSessionWait "preprocessor" $ do
  let content =
        T.unlines
          [ "{-# OPTIONS_GHC -F -pgmF=ghcide-test-preprocessor #-}"
          , "module Testing where"
          , "y = x + z" -- plugin replaces x with y, making this have only one diagnostic
          ]
  _ <- createDoc "Testing.hs" "haskell" content
  expectDiagnostics
    [ ( "Testing.hs",
        [(DsError, (2, 8), "Variable not in scope: z")]
      )
    ]


safeTests :: TestTree
safeTests =
  testGroup
    "SafeHaskell"
    [ -- Test for https://github.com/haskell/ghcide/issues/424
      testSessionWait "load" $ do
        let sourceA =
              T.unlines
                ["{-# LANGUAGE Trustworthy #-}"
                ,"module A where"
                ,"import System.IO.Unsafe"
                ,"import System.IO ()"
                ,"trustWorthyId :: a -> a"
                ,"trustWorthyId i = unsafePerformIO $ do"
                ,"  putStrLn \"I'm safe\""
                ,"  return i"]
            sourceB =
              T.unlines
                ["{-# LANGUAGE Safe #-}"
                ,"module B where"
                ,"import A"
                ,"safeId :: a -> a"
                ,"safeId = trustWorthyId"
                ]

        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectNoMoreDiagnostics 1 ]

thTests :: TestTree
thTests =
  testGroup
    "TemplateHaskell"
    [ -- Test for https://github.com/haskell/ghcide/pull/212
      testSessionWait "load" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE PackageImports #-}",
                  "{-# LANGUAGE TemplateHaskell #-}",
                  "module A where",
                  "import \"template-haskell\" Language.Haskell.TH",
                  "a :: Integer",
                  "a = $(litE $ IntegerL 3)"
                ]
            sourceB =
              T.unlines
                [ "{-# LANGUAGE PackageImports #-}",
                  "{-# LANGUAGE TemplateHaskell #-}",
                  "module B where",
                  "import A",
                  "import \"template-haskell\" Language.Haskell.TH",
                  "b :: Integer",
                  "b = $(litE $ IntegerL $ a) + n"
                ]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectDiagnostics [ ( "B.hs", [(DsError, (6, 29), "Variable not in scope: n")] ) ]
    , testSessionWait "newtype-closure" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE DeriveDataTypeable #-}"
                  ,"{-# LANGUAGE TemplateHaskell #-}"
                  ,"module A (a) where"
                  ,"import Data.Data"
                  ,"import Language.Haskell.TH"
                  ,"newtype A = A () deriving (Data)"
                  ,"a :: ExpQ"
                  ,"a = [| 0 |]"]
        let sourceB =
              T.unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                ,"module B where"
                ,"import A"
                ,"b :: Int"
                ,"b = $( a )" ]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        return ()
    , thReloadingTest False
    , ignoreInWindowsBecause "Broken in windows" $ thReloadingTest True
    -- Regression test for https://github.com/haskell/haskell-language-server/issues/891
    , thLinkingTest False
    , ignoreInWindowsBecause "Broken in windows" $ thLinkingTest True
    , testSessionWait "findsTHIdentifiers" $ do
        let sourceA =
              T.unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                , "module A (a) where"
                , "import Language.Haskell.TH (ExpQ)"
                , "a :: ExpQ" -- TH 2.17 requires an explicit type signature since splices are polymorphic
                , "a = [| glorifiedID |]"
                , "glorifiedID :: a -> a"
                , "glorifiedID = id" ]
        let sourceB =
              T.unlines
                [ "{-# OPTIONS_GHC -Wall #-}"
                , "{-# LANGUAGE TemplateHaskell #-}"
                , "module B where"
                , "import A"
                , "main = $a (putStrLn \"success!\")"]
        _ <- createDoc "A.hs" "haskell" sourceA
        _ <- createDoc "B.hs" "haskell" sourceB
        expectDiagnostics [ ( "B.hs", [(DsWarning, (4, 0), "Top-level binding with no type signature: main :: IO ()")] ) ]
    , ignoreInWindowsForGHC88 $ testCase "findsTHnewNameConstructor" $ runWithExtraFiles "THNewName" $ \dir -> do

    -- This test defines a TH value with the meaning "data A = A" in A.hs
    -- Loads and export the template in B.hs
    -- And checks wether the constructor A can be loaded in C.hs
    -- This test does not fail when either A and B get manually loaded before C.hs
    -- or when we remove the seemingly unnecessary TH pragma from C.hs

    let cPath = dir </> "C.hs"
    _ <- openDoc cPath "haskell"
    expectDiagnostics [ ( cPath, [(DsWarning, (3, 0), "Top-level binding with no type signature: a :: A")] ) ]
    ]

-- | test that TH is reevaluated on typecheck
thReloadingTest :: Bool -> TestTree
thReloadingTest unboxed = testCase name $ runWithExtraFiles dir $ \dir -> do

    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"
        cPath = dir </> "THC.hs"

    aSource <- liftIO $ readFileUtf8 aPath --  th = [d|a = ()|]
    bSource <- liftIO $ readFileUtf8 bPath --  $th
    cSource <- liftIO $ readFileUtf8 cPath --  c = a :: ()

    adoc <- createDoc aPath "haskell" aSource
    bdoc <- createDoc bPath "haskell" bSource
    cdoc <- createDoc cPath "haskell" cSource

    expectDiagnostics [("THB.hs", [(DsWarning, (4,thDollarIdx), "Top-level binding")])]

    -- Change th from () to Bool
    let aSource' = T.unlines $ init (T.lines aSource) ++ ["th_a = [d| a = False|]"]
    changeDoc adoc [TextDocumentContentChangeEvent Nothing Nothing aSource']
    -- generate an artificial warning to avoid timing out if the TH change does not propagate
    changeDoc cdoc [TextDocumentContentChangeEvent Nothing Nothing $ cSource <> "\nfoo=()"]

    -- Check that the change propagates to C
    expectDiagnostics
        [("THC.hs", [(DsError, (4, 4), "Couldn't match expected type '()' with actual type 'Bool'")])
        ,("THC.hs", [(DsWarning, (6,0), "Top-level binding")])
        ,("THB.hs", [(DsWarning, (4,thDollarIdx), "Top-level bindin")])
        ]

    closeDoc adoc
    closeDoc bdoc
    closeDoc cdoc
  where
    name = "reloading-th-test" <> if unboxed then "-unboxed" else ""
    dir | unboxed = "THUnboxed"
        | otherwise = "TH"

thLinkingTest :: Bool -> TestTree
thLinkingTest unboxed = testCase name $ runWithExtraFiles dir $ \dir -> do

    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"

    aSource <- liftIO $ readFileUtf8 aPath --  th_a = [d|a :: ()|]
    bSource <- liftIO $ readFileUtf8 bPath --  $th_a

    adoc <- createDoc aPath "haskell" aSource
    bdoc <- createDoc bPath "haskell" bSource

    expectDiagnostics [("THB.hs", [(DsWarning, (4,thDollarIdx), "Top-level binding")])]

    let aSource' = T.unlines $ init (init (T.lines aSource)) ++ ["th :: DecsQ", "th = [d| a = False|]"]
    changeDoc adoc [TextDocumentContentChangeEvent Nothing Nothing aSource']

    -- modify b too
    let bSource' = T.unlines $ init (T.lines bSource) ++ ["$th"]
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing bSource']

    expectDiagnostics [("THB.hs", [(DsWarning, (4,thDollarIdx), "Top-level binding")])]

    closeDoc adoc
    closeDoc bdoc
  where
    name = "th-linking-test" <> if unboxed then "-unboxed" else ""
    dir | unboxed = "THUnboxed"
        | otherwise = "TH"

completionTests :: TestTree
completionTests
  = testGroup "completion"
    [ testGroup "non local" nonLocalCompletionTests
    , testGroup "topLevel" topLevelCompletionTests
    , testGroup "local" localCompletionTests
    , testGroup "other" otherCompletionTests
    ]

completionTest :: String -> [T.Text] -> Position -> [(T.Text, CompletionItemKind, T.Text, Bool, Bool, Maybe (List TextEdit))] -> TestTree
completionTest name src pos expected = testSessionWait name $ do
    docId <- createDoc "A.hs" "haskell" (T.unlines src)
    _ <- waitForDiagnostics
    compls <- getCompletions docId pos
    let compls' = [ (_label, _kind, _insertText, _additionalTextEdits) | CompletionItem{..} <- compls]
    liftIO $ do
        let emptyToMaybe x = if T.null x then Nothing else Just x
        sortOn (Lens.view Lens._1) compls' @?=
            sortOn (Lens.view Lens._1) [ (l, Just k, emptyToMaybe t, at) | (l,k,t,_,_,at) <- expected]
        forM_ (zip compls expected) $ \(CompletionItem{..}, (_,_,_,expectedSig, expectedDocs, _)) -> do
            when expectedSig $
                assertBool ("Missing type signature: " <> T.unpack _label) (isJust _detail)
            when expectedDocs $
                assertBool ("Missing docs: " <> T.unpack _label) (isJust _documentation)

completionCommandTest ::
  String ->
  [T.Text] ->
  Position ->
  T.Text ->
  [T.Text] ->
  TestTree
completionCommandTest name src pos wanted expected = testSession name $ do
  docId <- createDoc "A.hs" "haskell" (T.unlines src)
  _ <- waitForDiagnostics
  compls <- skipManyTill anyMessage (getCompletions docId pos)
  let wantedC = find ( \case
            CompletionItem {_insertText = Just x} -> wanted `T.isPrefixOf` x
            _                                     -> False
            ) compls
  case wantedC of
    Nothing ->
      liftIO $ assertFailure $ "Cannot find expected completion in: " <> show [_label | CompletionItem {_label} <- compls]
    Just CompletionItem {..} -> do
      c <- assertJust "Expected a command" _command
      executeCommand c
      if src /= expected
          then do
            void $ skipManyTill anyMessage loggingNotification
            modifiedCode <- skipManyTill anyMessage (getDocumentEdit docId)
            liftIO $ modifiedCode @?= T.unlines expected
          else do
            expectMessages SWorkspaceApplyEdit 1 $ \edit ->
              liftIO $ assertFailure $ "Expected no edit but got: " <> show edit

completionNoCommandTest ::
  String ->
  [T.Text] ->
  Position ->
  T.Text ->
  TestTree
completionNoCommandTest name src pos wanted = testSession name $ do
  docId <- createDoc "A.hs" "haskell" (T.unlines src)
  _ <- waitForDiagnostics
  compls <- getCompletions docId pos
  let wantedC = find ( \case
            CompletionItem {_insertText = Just x} -> wanted `T.isPrefixOf` x
            _                                     -> False
            ) compls
  case wantedC of
    Nothing ->
      liftIO $ assertFailure $ "Cannot find expected completion in: " <> show [_label | CompletionItem {_label} <- compls]
    Just CompletionItem{..} -> liftIO . assertBool ("Expected no command but got: " <> show _command) $ null _command


topLevelCompletionTests :: [TestTree]
topLevelCompletionTests = [
    completionTest
        "variable"
        ["bar = xx", "-- | haddock", "xxx :: ()", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 8)
        [("xxx", CiFunction, "xxx", True, True, Nothing),
         ("XxxCon", CiConstructor, "XxxCon", False, True, Nothing)
        ],
    completionTest
        "constructor"
        ["bar = xx", "-- | haddock", "xxx :: ()", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 8)
        [("xxx", CiFunction, "xxx", True, True, Nothing),
         ("XxxCon", CiConstructor, "XxxCon", False, True, Nothing)
        ],
    completionTest
        "class method"
        ["bar = xx", "class Xxx a where", "-- | haddock", "xxx :: ()", "xxx = ()"]
        (Position 0 8)
        [("xxx", CiFunction, "xxx", True, True, Nothing)],
    completionTest
        "type"
        ["bar :: Xx", "xxx = ()", "-- | haddock", "data Xxx = XxxCon"]
        (Position 0 9)
        [("Xxx", CiStruct, "Xxx", False, True, Nothing)],
    completionTest
        "class"
        ["bar :: Xx", "xxx = ()", "-- | haddock", "class Xxx a"]
        (Position 0 9)
        [("Xxx", CiInterface, "Xxx", False, True, Nothing)],
    completionTest
        "records"
        ["data Person = Person { _personName:: String, _personAge:: Int}", "bar = Person { _pers }" ]
        (Position 1 19)
        [("_personName", CiFunction, "_personName", False, True, Nothing),
         ("_personAge", CiFunction, "_personAge", False, True, Nothing)],
    completionTest
        "recordsConstructor"
        ["data XxRecord = XyRecord { x:: String, y:: Int}", "bar = Xy" ]
        (Position 1 19)
        [("XyRecord", CiConstructor, "XyRecord", False, True, Nothing),
         ("XyRecord", CiSnippet, "XyRecord {x=${1:_x}, y=${2:_y}}", False, True, Nothing)]
    ]

localCompletionTests :: [TestTree]
localCompletionTests = [
    completionTest
        "argument"
        ["bar (Just abcdef) abcdefg = abcd"]
        (Position 0 32)
        [("abcdef", CiFunction, "abcdef", True, False, Nothing),
         ("abcdefg", CiFunction , "abcdefg", True, False, Nothing)
        ],
    completionTest
        "let"
        ["bar = let (Just abcdef) = undefined"
        ,"          abcdefg = let abcd = undefined in undefined"
        ,"        in abcd"
        ]
        (Position 2 15)
        [("abcdef", CiFunction, "abcdef", True, False, Nothing),
         ("abcdefg", CiFunction , "abcdefg", True, False, Nothing)
        ],
    completionTest
        "where"
        ["bar = abcd"
        ,"  where (Just abcdef) = undefined"
        ,"        abcdefg = let abcd = undefined in undefined"
        ]
        (Position 0 10)
        [("abcdef", CiFunction, "abcdef", True, False, Nothing),
         ("abcdefg", CiFunction , "abcdefg", True, False, Nothing)
        ],
    completionTest
        "do/1"
        ["bar = do"
        ,"  Just abcdef <- undefined"
        ,"  abcd"
        ,"  abcdefg <- undefined"
        ,"  pure ()"
        ]
        (Position 2 6)
        [("abcdef", CiFunction, "abcdef", True, False, Nothing)
        ],
    completionTest
        "do/2"
        ["bar abcde = do"
        ,"    Just [(abcdef,_)] <- undefined"
        ,"    abcdefg <- undefined"
        ,"    let abcdefgh = undefined"
        ,"        (Just [abcdefghi]) = undefined"
        ,"    abcd"
        ,"  where"
        ,"    abcdefghij = undefined"
        ]
        (Position 5 8)
        [("abcde", CiFunction, "abcde", True, False, Nothing)
        ,("abcdefghij", CiFunction, "abcdefghij", True, False, Nothing)
        ,("abcdef", CiFunction, "abcdef", True, False, Nothing)
        ,("abcdefg", CiFunction, "abcdefg", True, False, Nothing)
        ,("abcdefgh", CiFunction, "abcdefgh", True, False, Nothing)
        ,("abcdefghi", CiFunction, "abcdefghi", True, False, Nothing)
        ],
    completionTest
        "class method"
        [
          "class Test a where"
        , "    abcd :: a -> ()"
        , "    abcde :: a -> Int"
        , "instance Test Int where"
        , "    abcd = abc"
        ]
        (Position 4 14)
        [("abcd", CiFunction, "abcd", True, False, Nothing)
        ,("abcde", CiFunction, "abcde", True, False, Nothing)
        ]
    ]

nonLocalCompletionTests :: [TestTree]
nonLocalCompletionTests =
  [ completionTest
      "variable"
      ["module A where", "f = hea"]
      (Position 1 7)
      [("head", CiFunction, "head ${1:([a])}", True, True, Nothing)],
    completionTest
      "constructor"
      ["module A where", "f = Tru"]
      (Position 1 7)
      [ ("True", CiConstructor, "True ", True, True, Nothing),
        ("truncate", CiFunction, "truncate ${1:a}", True, True, Nothing)
      ],
    completionTest
      "type"
      ["{-# OPTIONS_GHC -Wall #-}", "module A () where", "f :: Bo", "f = True"]
      (Position 2 7)
      [ ("Bounded", CiInterface, "Bounded ${1:(*)}", True, True, Nothing),
        ("Bool", CiStruct, "Bool ", True, True, Nothing)
      ],
    completionTest
      "qualified"
      ["{-# OPTIONS_GHC -Wunused-binds #-}", "module A () where", "f = Prelude.hea"]
      (Position 2 15)
      [ ("head", CiFunction, "head ${1:([a])}", True, True, Nothing)
      ],
    completionTest
      "duplicate import"
      ["module A where", "import Data.List", "import Data.List", "f = perm"]
      (Position 3 8)
      [ ("permutations", CiFunction, "permutations ${1:([a])}", False, False, Nothing)
      ],
    completionTest
       "dont show hidden items"
       [ "{-# LANGUAGE NoImplicitPrelude #-}",
         "module A where",
         "import Control.Monad hiding (join)",
         "f = joi"
       ]
       (Position 3 6)
       [],
    testGroup "auto import snippets"
      [ completionCommandTest
        "show imports not in list - simple"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (msum)", "f = joi"]
        (Position 3 6)
        "join"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (msum, join)", "f = joi"]
      , completionCommandTest
        "show imports not in list - multi-line"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (\n    msum)", "f = joi"]
        (Position 4 6)
        "join"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (\n    msum, join)", "f = joi"]
      , completionCommandTest
        "show imports not in list - names with _"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad as M (msum)", "f = M.mapM_"]
        (Position 3 11)
        "mapM_"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad as M (msum, mapM_)", "f = M.mapM_"]
      , completionCommandTest
        "show imports not in list - initial empty list"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad as M ()", "f = M.joi"]
        (Position 3 10)
        "join"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad as M (join)", "f = M.joi"]
      , testGroup "qualified imports"
        [ completionCommandTest
            "single"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad ()", "f = Control.Monad.joi"]
            (Position 3 22)
            "join"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad (join)", "f = Control.Monad.joi"]
        , completionCommandTest
            "as"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad as M ()", "f = M.joi"]
            (Position 3 10)
            "join"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad as M (join)", "f = M.joi"]
        , completionCommandTest
            "multiple"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad as M ()", "import Control.Monad as N ()", "f = N.joi"]
            (Position 4 10)
            "join"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad as M ()", "import Control.Monad as N (join)", "f = N.joi"]
        ]
      , testGroup "Data constructor"
        [ completionCommandTest
            "not imported"
            ["module A where", "import Text.Printf ()", "ZeroPad"]
            (Position 2 4)
            "ZeroPad"
            ["module A where", "import Text.Printf (FormatAdjustment (ZeroPad))", "ZeroPad"]
        , completionCommandTest
            "parent imported abs"
            ["module A where", "import Text.Printf (FormatAdjustment)", "ZeroPad"]
            (Position 2 4)
            "ZeroPad"
            ["module A where", "import Text.Printf (FormatAdjustment (ZeroPad))", "ZeroPad"]
        , completionNoCommandTest
            "parent imported all"
            ["module A where", "import Text.Printf (FormatAdjustment (..))", "ZeroPad"]
            (Position 2 4)
            "ZeroPad"
        , completionNoCommandTest
            "already imported"
            ["module A where", "import Text.Printf (FormatAdjustment (ZeroPad))", "ZeroPad"]
            (Position 2 4)
            "ZeroPad"
        , completionNoCommandTest
            "function from Prelude"
            ["module A where", "import Data.Maybe ()", "Nothing"]
            (Position 2 4)
            "Nothing"
        , completionCommandTest
            "type operator parent"
            ["module A where", "import Data.Type.Equality ()", "f = Ref"]
            (Position 2 8)
            "Refl"
            ["module A where", "import Data.Type.Equality (type (:~:) (Refl))", "f = Ref"]
        ]
      , testGroup "Record completion"
        [ completionCommandTest
            "not imported"
            ["module A where", "import Text.Printf ()", "FormatParse"]
            (Position 2 10)
            "FormatParse {"
            ["module A where", "import Text.Printf (FormatParse (FormatParse))", "FormatParse"]
        , completionCommandTest
            "parent imported"
            ["module A where", "import Text.Printf (FormatParse)", "FormatParse"]
            (Position 2 10)
            "FormatParse {"
            ["module A where", "import Text.Printf (FormatParse (FormatParse))", "FormatParse"]
        , completionNoCommandTest
            "already imported"
            ["module A where", "import Text.Printf (FormatParse (FormatParse))", "FormatParse"]
            (Position 2 10)
            "FormatParse {"
        ]
      ],
      -- we need this test to make sure the ghcide completions module does not return completions for language pragmas. this functionality is turned on in hls
     completionTest
      "do not show pragma completions"
      [ "{-# LANGUAGE  ",
        "{module A where}",
        "main = return ()"
      ]
      (Position 0 13)
      []
  ]

otherCompletionTests :: [TestTree]
otherCompletionTests = [
    completionTest
      "keyword"
      ["module A where", "f = newty"]
      (Position 1 9)
      [("newtype", CiKeyword, "", False, False, Nothing)],
    completionTest
      "type context"
      [ "{-# OPTIONS_GHC -Wunused-binds #-}",
        "module A () where",
        "f = f",
        "g :: Intege"
      ]
      -- At this point the module parses but does not typecheck.
      -- This should be sufficient to detect that we are in a
      -- type context and only show the completion to the type.
      (Position 3 11)
      [("Integer", CiStruct, "Integer ", True, True, Nothing)],

    testSession "duplicate record fields" $ do
      void $
        createDoc "B.hs" "haskell" $
          T.unlines
            [ "{-# LANGUAGE DuplicateRecordFields #-}",
              "module B where",
              "newtype Foo = Foo { member :: () }",
              "newtype Bar = Bar { member :: () }"
            ]
      docA <-
        createDoc "A.hs" "haskell" $
          T.unlines
            [ "module A where",
              "import B",
              "memb"
            ]
      _ <- waitForDiagnostics
      compls <- getCompletions docA $ Position 2 4
      let compls' = [txt | CompletionItem {_insertText = Just txt, ..} <- compls, _label == "member"]
      liftIO $ compls' @?= ["member ${1:Foo}", "member ${1:Bar}"],

    testSessionWait "maxCompletions" $ do
        doc <- createDoc "A.hs" "haskell" $ T.unlines
            [ "{-# OPTIONS_GHC -Wunused-binds #-}",
                "module A () where",
                "a = Prelude."
            ]
        _ <- waitForDiagnostics
        compls <- getCompletions  doc (Position 3 13)
        liftIO $ length compls @?= maxCompletions def
  ]

highlightTests :: TestTree
highlightTests = testGroup "highlight"
  [ testSessionWait "value" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 3 2)
    liftIO $ highlights @?= List
            [ DocumentHighlight (R 2 0 2 3) (Just HkRead)
            , DocumentHighlight (R 3 0 3 3) (Just HkWrite)
            , DocumentHighlight (R 4 6 4 9) (Just HkRead)
            , DocumentHighlight (R 5 22 5 25) (Just HkRead)
            ]
  , testSessionWait "type" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 2 8)
    liftIO $ highlights @?= List
            [ DocumentHighlight (R 2 7 2 10) (Just HkRead)
            , DocumentHighlight (R 3 11 3 14) (Just HkRead)
            ]
  , testSessionWait "local" $ do
    doc <- createDoc "A.hs" "haskell" source
    _ <- waitForDiagnostics
    highlights <- getHighlights doc (Position 6 5)
    liftIO $ highlights @?= List
            [ DocumentHighlight (R 6 4 6 7) (Just HkWrite)
            , DocumentHighlight (R 6 10 6 13) (Just HkRead)
            , DocumentHighlight (R 7 12 7 15) (Just HkRead)
            ]
  , knownBrokenForGhcVersions [GHC90] "Ghc9 highlights the constructor and not just this field" $
        testSessionWait "record" $ do
        doc <- createDoc "A.hs" "haskell" recsource
        _ <- waitForDiagnostics
        highlights <- getHighlights doc (Position 4 15)
        liftIO $ highlights @?= List
          -- Span is just the .. on 8.10, but Rec{..} before
          [ if ghcVersion >= GHC810
              then DocumentHighlight (R 4 8 4 10) (Just HkWrite)
              else DocumentHighlight (R 4 4 4 11) (Just HkWrite)
          , DocumentHighlight (R 4 14 4 20) (Just HkRead)
          ]
        highlights <- getHighlights doc (Position 3 17)
        liftIO $ highlights @?= List
          [ DocumentHighlight (R 3 17 3 23) (Just HkWrite)
          -- Span is just the .. on 8.10, but Rec{..} before
          , if ghcVersion >= GHC810
              then DocumentHighlight (R 4 8 4 10) (Just HkRead)
              else DocumentHighlight (R 4 4 4 11) (Just HkRead)
          ]
  ]
  where
    source = T.unlines
      ["{-# OPTIONS_GHC -Wunused-binds #-}"
      ,"module Highlight () where"
      ,"foo :: Int"
      ,"foo = 3 :: Int"
      ,"bar = foo"
      ,"  where baz = let x = foo in x"
      ,"baz arg = arg + x"
      ,"  where x = arg"
      ]
    recsource = T.unlines
      ["{-# LANGUAGE RecordWildCards #-}"
      ,"{-# OPTIONS_GHC -Wunused-binds #-}"
      ,"module Highlight () where"
      ,"data Rec = Rec { field1 :: Int, field2 :: Char }"
      ,"foo Rec{..} = field2 + field1"
      ]

outlineTests :: TestTree
outlineTests = testGroup
  "outline"
  [ testSessionWait "type class" $ do
    let source = T.unlines ["module A where", "class A a where a :: a -> Bool"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ moduleSymbol
          "A"
          (R 0 7 0 8)
          [ classSymbol "A a"
                        (R 1 0 1 30)
                        [docSymbol' "a" SkMethod (R 1 16 1 30) (R 1 16 1 17)]
          ]
      ]
  , testSessionWait "type class instance " $ do
    let source = T.unlines ["class A a where", "instance A () where"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ classSymbol "A a" (R 0 0 0 15) []
      , docSymbol "A ()" SkInterface (R 1 0 1 19)
      ]
  , testSessionWait "type family" $ do
    let source = T.unlines ["{-# language TypeFamilies #-}", "type family A"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "A" "type family" SkFunction (R 1 0 1 13)]
  , testSessionWait "type family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "type family A a"
          , "type instance A () = ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolD "A a"   "type family" SkFunction     (R 1 0 1 15)
      , docSymbol "A ()" SkInterface (R 2 0 2 23)
      ]
  , testSessionWait "data family" $ do
    let source = T.unlines ["{-# language TypeFamilies #-}", "data family A"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "A" "data family" SkFunction (R 1 0 1 11)]
  , testSessionWait "data family instance " $ do
    let source = T.unlines
          [ "{-# language TypeFamilies #-}"
          , "data family A a"
          , "data instance A () = A ()"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolD "A a"   "data family" SkFunction     (R 1 0 1 11)
      , docSymbol "A ()" SkInterface (R 2 0 2 25)
      ]
  , testSessionWait "constant" $ do
    let source = T.unlines ["a = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol "a" SkFunction (R 0 0 0 6)]
  , testSessionWait "pattern" $ do
    let source = T.unlines ["Just foo = Just 21"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol "Just foo" SkFunction (R 0 0 0 18)]
  , testSessionWait "pattern with type signature" $ do
    let source = T.unlines ["{-# language ScopedTypeVariables #-}", "a :: () = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol "a :: ()" SkFunction (R 1 0 1 12)]
  , testSessionWait "function" $ do
    let source = T.unlines ["a _x = ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbol "a" SkFunction (R 0 0 0 9)]
  , testSessionWait "type synonym" $ do
    let source = T.unlines ["type A = Bool"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbol' "A" SkTypeParameter (R 0 0 0 13) (R 0 5 0 6)]
  , testSessionWait "datatype" $ do
    let source = T.unlines ["data A = C"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [ docSymbolWithChildren "A"
                              SkStruct
                              (R 0 0 0 10)
                              [docSymbol "C" SkConstructor (R 0 9 0 10)]
      ]
  , testSessionWait "record fields" $ do
    let source = T.unlines ["data A = B {", "  x :: Int", "  , y :: Int}"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @=? Left
      [ docSymbolWithChildren "A" SkStruct (R 0 0 2 13)
          [ docSymbolWithChildren' "B" SkConstructor (R 0 9 2 13) (R 0 9 0 10)
            [ docSymbol "x" SkField (R 1 2 1 3)
            , docSymbol "y" SkField (R 2 4 2 5)
            ]
          ]
      ]
  , testSessionWait "import" $ do
    let source = T.unlines ["import Data.Maybe ()"]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbolWithChildren "imports"
                             SkModule
                             (R 0 0 0 20)
                             [ docSymbol "import Data.Maybe" SkModule (R 0 0 0 20)
                             ]
      ]
  , testSessionWait "multiple import" $ do
    let source = T.unlines ["", "import Data.Maybe ()", "", "import Control.Exception ()", ""]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left
      [docSymbolWithChildren "imports"
                             SkModule
                             (R 1 0 3 27)
                             [ docSymbol "import Data.Maybe" SkModule (R 1 0 1 20)
                             , docSymbol "import Control.Exception" SkModule (R 3 0 3 27)
                             ]
      ]
  , testSessionWait "foreign import" $ do
    let source = T.unlines
          [ "{-# language ForeignFunctionInterface #-}"
          , "foreign import ccall \"a\" a :: Int"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "a" "import" SkObject (R 1 0 1 33)]
  , testSessionWait "foreign export" $ do
    let source = T.unlines
          [ "{-# language ForeignFunctionInterface #-}"
          , "foreign export ccall odd :: Int -> Bool"
          ]
    docId   <- createDoc "A.hs" "haskell" source
    symbols <- getDocumentSymbols docId
    liftIO $ symbols @?= Left [docSymbolD "odd" "export" SkObject (R 1 0 1 39)]
  ]
 where
  docSymbol name kind loc =
    DocumentSymbol name Nothing kind Nothing Nothing loc loc Nothing
  docSymbol' name kind loc selectionLoc =
    DocumentSymbol name Nothing kind Nothing Nothing loc selectionLoc Nothing
  docSymbolD name detail kind loc =
    DocumentSymbol name (Just detail) kind Nothing Nothing loc loc Nothing
  docSymbolWithChildren name kind loc cc =
    DocumentSymbol name Nothing kind Nothing Nothing loc loc (Just $ List cc)
  docSymbolWithChildren' name kind loc selectionLoc cc =
    DocumentSymbol name Nothing kind Nothing Nothing loc selectionLoc (Just $ List cc)
  moduleSymbol name loc cc = DocumentSymbol name
                                            Nothing
                                            SkFile
                                            Nothing
                                            Nothing
                                            (R 0 0 maxBound 0)
                                            loc
                                            (Just $ List cc)
  classSymbol name loc cc = DocumentSymbol name
                                           (Just "class")
                                           SkInterface
                                           Nothing
                                           Nothing
                                           loc
                                           loc
                                           (Just $ List cc)

pattern R :: Int -> Int -> Int -> Int -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

xfail :: TestTree -> String -> TestTree
xfail = flip expectFailBecause

ignoreInWindowsBecause :: String -> TestTree -> TestTree
ignoreInWindowsBecause
    | isWindows = ignoreTestBecause
    | otherwise = \_ x -> x

ignoreInWindowsForGHC88And810 :: TestTree -> TestTree
ignoreInWindowsForGHC88And810
    | ghcVersion `elem` [GHC88, GHC810] =
        ignoreInWindowsBecause "tests are unreliable in windows for ghc 8.8 and 8.10"
    | otherwise = id

ignoreInWindowsForGHC88 :: TestTree -> TestTree
ignoreInWindowsForGHC88
    | ghcVersion == GHC88 =
        ignoreInWindowsBecause "tests are unreliable in windows for ghc 8.8"
    | otherwise = id

knownBrokenForGhcVersions :: [GhcVersion] -> String -> TestTree -> TestTree
knownBrokenForGhcVersions ghcVers
    | ghcVersion `elem` ghcVers = expectFailBecause
    | otherwise = \_ x -> x


data Expect
  = ExpectRange Range -- Both gotoDef and hover should report this range
  | ExpectLocation Location
--  | ExpectDefRange Range -- Only gotoDef should report this range
  | ExpectHoverRange Range -- Only hover should report this range
  | ExpectHoverText [T.Text] -- the hover message must contain these snippets
  | ExpectExternFail -- definition lookup in other file expected to fail
  | ExpectNoDefinitions
  | ExpectNoHover
--  | ExpectExtern -- TODO: as above, but expected to succeed: need some more info in here, once we have some working examples
  deriving Eq

mkR :: Int -> Int -> Int -> Int -> Expect
mkR startLine startColumn endLine endColumn = ExpectRange $ mkRange startLine startColumn endLine endColumn

mkL :: Uri -> Int -> Int -> Int -> Int -> Expect
mkL uri startLine startColumn endLine endColumn = ExpectLocation $ Location uri $ mkRange startLine startColumn endLine endColumn

haddockTests :: TestTree
haddockTests
  = testGroup "haddock"
      [ testCase "Num" $ checkHaddock
          (unlines
             [ "However, '(+)' and '(*)' are"
             , "customarily expected to define a ring and have the following properties:"
             , ""
             , "[__Associativity of (+)__]: @(x + y) + z@ = @x + (y + z)@"
             , "[__Commutativity of (+)__]: @x + y@ = @y + x@"
             , "[__@fromInteger 0@ is the additive identity__]: @x + fromInteger 0@ = @x@"
             ]
          )
          (unlines
             [ ""
             , ""
             , "However,  `(+)`  and  `(*)`  are"
             , "customarily expected to define a ring and have the following properties: "
             , "+ ****Associativity of (+)****: `(x + y) + z`  =  `x + (y + z)`"
             , "+ ****Commutativity of (+)****: `x + y`  =  `y + x`"
             , "+ ****`fromInteger 0`  is the additive identity****: `x + fromInteger 0`  =  `x`"
             ]
          )
      , testCase "unsafePerformIO" $ checkHaddock
          (unlines
             [ "may require"
             , "different precautions:"
             , ""
             , "  * Use @{\\-\\# NOINLINE foo \\#-\\}@ as a pragma on any function @foo@"
             , "        that calls 'unsafePerformIO'.  If the call is inlined,"
             , "        the I\\/O may be performed more than once."
             , ""
             , "  * Use the compiler flag @-fno-cse@ to prevent common sub-expression"
             , "        elimination being performed on the module."
             , ""
             ]
          )
          (unlines
             [ ""
             , ""
             , "may require"
             , "different precautions: "
             , "+ Use  `{-# NOINLINE foo #-}`  as a pragma on any function  `foo` "
             , "  that calls  `unsafePerformIO` .  If the call is inlined,"
             , "  the I/O may be performed more than once."
             , ""
             , "+ Use the compiler flag  `-fno-cse`  to prevent common sub-expression"
             , "  elimination being performed on the module."
             , ""
             ]
          )
      ]
  where
    checkHaddock s txt = spanDocToMarkdownForTest s @?= txt

cradleTests :: TestTree
cradleTests = testGroup "cradle"
    [testGroup "dependencies" [sessionDepsArePickedUp]
    ,testGroup "ignore-fatal" [ignoreFatalWarning]
    ,testGroup "loading" [loadCradleOnlyonce, retryFailedCradle]
    ,testGroup "multi"   [simpleMultiTest, simpleMultiTest2, simpleMultiDefTest]
    ,testGroup "sub-directory"   [simpleSubDirectoryTest]
    ]

loadCradleOnlyonce :: TestTree
loadCradleOnlyonce = testGroup "load cradle only once"
    [ testSession' "implicit" implicit
    , testSession' "direct"   direct
    ]
    where
        direct dir = do
            liftIO $ writeFileUTF8 (dir </> "hie.yaml")
                "cradle: {direct: {arguments: []}}"
            test dir
        implicit dir = test dir
        test _dir = do
            doc <- createDoc "B.hs" "haskell" "module B where\nimport Data.Foo"
            msgs <- someTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message STextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 1
            changeDoc doc [TextDocumentContentChangeEvent Nothing Nothing "module B where\nimport Data.Maybe"]
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message STextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0
            _ <- createDoc "A.hs" "haskell" "module A where\nimport LoadCradleBar"
            msgs <- manyTill (skipManyTill anyMessage cradleLoadedMessage) (skipManyTill anyMessage (message STextDocumentPublishDiagnostics))
            liftIO $ length msgs @?= 0

retryFailedCradle :: TestTree
retryFailedCradle = testSession' "retry failed" $ \dir -> do
  -- The false cradle always fails
  let hieContents = "cradle: {bios: {shell: \"false\"}}"
      hiePath = dir </> "hie.yaml"
  liftIO $ writeFile hiePath hieContents
  let aPath = dir </> "A.hs"
  doc <- createDoc aPath "haskell" "main = return ()"
  Right WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
  liftIO $ "Test assumption failed: cradle should error out" `assertBool` not ideResultSuccess

  -- Fix the cradle and typecheck again
  let validCradle = "cradle: {bios: {shell: \"echo A.hs\"}}"
  liftIO $ writeFileUTF8 hiePath $ T.unpack validCradle
  sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
          List [FileEvent (filePathToUri $ dir </> "hie.yaml") FcChanged ]
  -- Force a session restart by making an edit, just to dirty the typecheck node
  changeDoc
    doc
    [ TextDocumentContentChangeEvent
        { _range = Just Range {_start = Position 0 0, _end = Position 0 0},
          _rangeLength = Nothing,
          _text = "\n"
        }
    ]

  Right WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" doc
  liftIO $ "No joy after fixing the cradle" `assertBool` ideResultSuccess


dependentFileTest :: TestTree
dependentFileTest = testGroup "addDependentFile"
    [testGroup "file-changed" [ignoreInWindowsForGHC88 $ testSession' "test" test]
    ]
    where
      test dir = do
        -- If the file contains B then no type error
        -- otherwise type error
        let depFilePath = dir </> "dep-file.txt"
        liftIO $ writeFile depFilePath "A"
        let fooContent = T.unlines
              [ "{-# LANGUAGE TemplateHaskell #-}"
              , "module Foo where"
              , "import Language.Haskell.TH.Syntax"
              , "foo :: Int"
              , "foo = 1 + $(do"
              , "               qAddDependentFile \"dep-file.txt\""
              , "               f <- qRunIO (readFile \"dep-file.txt\")"
              , "               if f == \"B\" then [| 1 |] else lift f)"
              ]
        let bazContent = T.unlines ["module Baz where", "import Foo ()"]
        _ <- createDoc "Foo.hs" "haskell" fooContent
        doc <- createDoc "Baz.hs" "haskell" bazContent
        expectDiagnostics $
            if ghcVersion >= GHC90
                -- String vs [Char] causes this change in error message
                then [("Foo.hs", [(DsError, (4, 6), "Couldn't match type")])]
                else [("Foo.hs", [(DsError, (4, 6), "Couldn't match expected type")])]
        -- Now modify the dependent file
        liftIO $ writeFile depFilePath "B"
        sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
          List [FileEvent (filePathToUri "dep-file.txt") FcChanged ]

        -- Modifying Baz will now trigger Foo to be rebuilt as well
        let change = TextDocumentContentChangeEvent
              { _range = Just (Range (Position 2 0) (Position 2 6))
              , _rangeLength = Nothing
              , _text = "f = ()"
              }
        changeDoc doc [change]
        expectDiagnostics [("Foo.hs", [])]


cradleLoadedMessage :: Session FromServerMessage
cradleLoadedMessage = satisfy $ \case
        FromServerMess (SCustomMethod m) (NotMess _) -> m == cradleLoadedMethod
        _                                            -> False

cradleLoadedMethod :: T.Text
cradleLoadedMethod = "ghcide/cradle/loaded"

ignoreFatalWarning :: TestTree
ignoreFatalWarning = testCase "ignore-fatal-warning" $ runWithExtraFiles "ignore-fatal" $ \dir -> do
    let srcPath = dir </> "IgnoreFatal.hs"
    src <- liftIO $ readFileUtf8 srcPath
    _ <- createDoc srcPath "haskell" src
    expectNoMoreDiagnostics 5

simpleSubDirectoryTest :: TestTree
simpleSubDirectoryTest =
  testCase "simple-subdirectory" $ runWithExtraFiles "cabal-exe" $ \dir -> do
    let mainPath = dir </> "a/src/Main.hs"
    mainSource <- liftIO $ readFileUtf8 mainPath
    _mdoc <- createDoc mainPath "haskell" mainSource
    expectDiagnosticsWithTags
      [("a/src/Main.hs", [(DsWarning,(2,0), "Top-level binding", Nothing)]) -- So that we know P has been loaded
      ]
    expectNoMoreDiagnostics 0.5

simpleMultiTest :: TestTree
simpleMultiTest = testCase "simple-multi-test" $ withLongTimeout $ runWithExtraFiles "multi" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    aSource <- liftIO $ readFileUtf8 aPath
    adoc <- createDoc aPath "haskell" aSource
    Right WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" adoc
    liftIO $ assertBool "A should typecheck" ideResultSuccess
    bSource <- liftIO $ readFileUtf8 bPath
    bdoc <- createDoc bPath "haskell" bSource
    Right WaitForIdeRuleResult {..} <- waitForAction "TypeCheck" bdoc
    liftIO $ assertBool "B should typecheck" ideResultSuccess
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- Like simpleMultiTest but open the files in the other order
simpleMultiTest2 :: TestTree
simpleMultiTest2 = testCase "simple-multi-test2" $ runWithExtraFiles "multi" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    bSource <- liftIO $ readFileUtf8 bPath
    bdoc <- createDoc bPath "haskell" bSource
    expectNoMoreDiagnostics 10
    aSource <- liftIO $ readFileUtf8 aPath
    (TextDocumentIdentifier adoc) <- createDoc aPath "haskell" aSource
    -- Need to have some delay here or the test fails
    expectNoMoreDiagnostics 10
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL adoc 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

-- Like simpleMultiTest but open the files in component 'a' in a seperate session
simpleMultiDefTest :: TestTree
simpleMultiDefTest = testCase "simple-multi-def-test" $ runWithExtraFiles "multi" $ \dir -> do
    let aPath = dir </> "a/A.hs"
        bPath = dir </> "b/B.hs"
    adoc <- liftIO $ runInDir dir $ do
      aSource <- liftIO $ readFileUtf8 aPath
      adoc <- createDoc aPath "haskell" aSource
      ~() <- skipManyTill anyMessage $ satisfyMaybe $ \case
        FromServerMess (SCustomMethod "ghcide/reference/ready") (NotMess NotificationMessage{_params = fp}) -> do
          A.Success fp' <- pure $ fromJSON fp
          if equalFilePath fp' aPath then pure () else Nothing
        _ -> Nothing
      closeDoc adoc
      pure adoc
    bSource <- liftIO $ readFileUtf8 bPath
    bdoc <- createDoc bPath "haskell" bSource
    locs <- getDefinitions bdoc (Position 2 7)
    let fooL = mkL (adoc ^. L.uri) 2 0 2 3
    checkDefs locs (pure [fooL])
    expectNoMoreDiagnostics 0.5

ifaceTests :: TestTree
ifaceTests = testGroup "Interface loading tests"
    [ -- https://github.com/haskell/ghcide/pull/645/
      ifaceErrorTest
    , ifaceErrorTest2
    , ifaceErrorTest3
    , ifaceTHTest
    ]

bootTests :: TestTree
bootTests = testCase "boot-def-test" $ runWithExtraFiles "boot" $ \dir -> do
  let cPath = dir </> "C.hs"
  cSource <- liftIO $ readFileUtf8 cPath

  -- Dirty the cache
  liftIO $ runInDir dir $ do
    cDoc <- createDoc cPath "haskell" cSource
    _ <- getHover cDoc $ Position 4 3
    ~() <- skipManyTill anyMessage $ satisfyMaybe $ \case
      FromServerMess (SCustomMethod "ghcide/reference/ready") (NotMess NotificationMessage{_params = fp}) -> do
        A.Success fp' <- pure $ fromJSON fp
        if equalFilePath fp' cPath then pure () else Nothing
      _ -> Nothing
    closeDoc cDoc

  cdoc <- createDoc cPath "haskell" cSource
  locs <- getDefinitions cdoc (Position 7 4)
  let floc = mkR 9 0 9 1
  checkDefs locs (pure [floc])

-- | test that TH reevaluates across interfaces
ifaceTHTest :: TestTree
ifaceTHTest = testCase "iface-th-test" $ runWithExtraFiles "TH" $ \dir -> do
    let aPath = dir </> "THA.hs"
        bPath = dir </> "THB.hs"
        cPath = dir </> "THC.hs"

    aSource <- liftIO $ readFileUtf8 aPath -- [TH] a :: ()
    _bSource <- liftIO $ readFileUtf8 bPath -- a :: ()
    cSource <- liftIO $ readFileUtf8 cPath -- c = a :: ()

    cdoc <- createDoc cPath "haskell" cSource

    -- Change [TH]a from () to Bool
    liftIO $ writeFileUTF8 aPath (unlines $ init (lines $ T.unpack aSource) ++ ["th_a = [d| a = False|]"])

    -- Check that the change propogates to C
    changeDoc cdoc [TextDocumentContentChangeEvent Nothing Nothing cSource]
    expectDiagnostics
      [("THC.hs", [(DsError, (4, 4), "Couldn't match expected type '()' with actual type 'Bool'")])
      ,("THB.hs", [(DsWarning, (4,thDollarIdx), "Top-level binding")])]
    closeDoc cdoc

ifaceErrorTest :: TestTree
ifaceErrorTest = testCase "iface-error-test-1" $ runWithExtraFiles "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])] -- So what we know P has been loaded

    -- Change y from Int to B
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines ["module B where", "y :: Bool", "y = undefined"]]
    -- save so that we can that the error propogates to A
    sendNotification STextDocumentDidSave (DidSaveTextDocumentParams bdoc Nothing)

    -- Check that the error propogates to A
    expectDiagnostics
      [("A.hs", [(DsError, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])]


    -- Check that we wrote the interfaces for B when we saved
    let m = SCustomMethod "test"
    lid <- sendRequest m $ toJSON $ GetInterfaceFilesDir bPath
    res <- skipManyTill anyMessage $ responseForId m lid
    liftIO $ case res of
      ResponseMessage{_result=Right (A.fromJSON -> A.Success hidir)} -> do
        hi_exists <- doesFileExist $ hidir </> "B.hi"
        assertBool ("Couldn't find B.hi in " ++ hidir) hi_exists
      _ -> assertFailure $ "Got malformed response for CustomMessage hidir: " ++ show res

    pdoc <- createDoc pPath "haskell" pSource
    changeDoc pdoc [TextDocumentContentChangeEvent Nothing Nothing $ pSource <> "\nfoo = y :: Bool" ]
    -- Now in P we have
    -- bar = x :: Int
    -- foo = y :: Bool
    -- HOWEVER, in A...
    -- x = y  :: Int
    -- This is clearly inconsistent, and the expected outcome a bit surprising:
    --   - The diagnostic for A has already been received. Ghcide does not repeat diagnostics
    --   - P is being typechecked with the last successful artifacts for A.
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])
      ,("P.hs", [(DsWarning,(6,0), "Top-level binding")])
      ]
    expectNoMoreDiagnostics 2

ifaceErrorTest2 :: TestTree
ifaceErrorTest2 = testCase "iface-error-test-2" $ runWithExtraFiles "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource
    pdoc <- createDoc pPath "haskell" pSource
    expectDiagnostics
      [("P.hs", [(DsWarning,(4,0), "Top-level binding")])] -- So that we know P has been loaded

    -- Change y from Int to B
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines ["module B where", "y :: Bool", "y = undefined"]]

    -- Add a new definition to P
    changeDoc pdoc [TextDocumentContentChangeEvent Nothing Nothing $ pSource <> "\nfoo = y :: Bool" ]
    -- Now in P we have
    -- bar = x :: Int
    -- foo = y :: Bool
    -- HOWEVER, in A...
    -- x = y  :: Int
    expectDiagnostics
    -- As in the other test, P is being typechecked with the last successful artifacts for A
    -- (ot thanks to -fdeferred-type-errors)
      [("A.hs", [(DsError, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])
      ,("P.hs", [(DsWarning, (4, 0), "Top-level binding")])
      ,("P.hs", [(DsWarning, (6, 0), "Top-level binding")])
      ]

    expectNoMoreDiagnostics 2

ifaceErrorTest3 :: TestTree
ifaceErrorTest3 = testCase "iface-error-test-3" $ runWithExtraFiles "recomp" $ \dir -> do
    let bPath = dir </> "B.hs"
        pPath = dir </> "P.hs"

    bSource <- liftIO $ readFileUtf8 bPath -- y :: Int
    pSource <- liftIO $ readFileUtf8 pPath -- bar = x :: Int

    bdoc <- createDoc bPath "haskell" bSource

    -- Change y from Int to B
    changeDoc bdoc [TextDocumentContentChangeEvent Nothing Nothing $ T.unlines ["module B where", "y :: Bool", "y = undefined"]]

    -- P should not typecheck, as there are no last valid artifacts for A
    _pdoc <- createDoc pPath "haskell" pSource

    -- In this example the interface file for A should not exist (modulo the cache folder)
    -- Despite that P still type checks, as we can generate an interface file for A thanks to -fdeferred-type-errors
    expectDiagnostics
      [("A.hs", [(DsError, (5, 4), "Couldn't match expected type 'Int' with actual type 'Bool'")])
      ,("P.hs", [(DsWarning,(4,0), "Top-level binding")])
      ]
    expectNoMoreDiagnostics 2

sessionDepsArePickedUp :: TestTree
sessionDepsArePickedUp = testSession'
  "session-deps-are-picked-up"
  $ \dir -> do
    liftIO $
      writeFileUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: []}}"
    -- Open without OverloadedStrings and expect an error.
    doc <- createDoc "Foo.hs" "haskell" fooContent
    expectDiagnostics $
        if ghcVersion >= GHC90
            -- String vs [Char] causes this change in error message
            then [("Foo.hs", [(DsError, (3, 6), "Couldn't match type")])]
            else [("Foo.hs", [(DsError, (3, 6), "Couldn't match expected type")])]
    -- Update hie.yaml to enable OverloadedStrings.
    liftIO $
      writeFileUTF8
        (dir </> "hie.yaml")
        "cradle: {direct: {arguments: [-XOverloadedStrings]}}"
    sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $
          List [FileEvent (filePathToUri $ dir </> "hie.yaml") FcChanged ]
    -- Send change event.
    let change =
          TextDocumentContentChangeEvent
            { _range = Just (Range (Position 4 0) (Position 4 0)),
              _rangeLength = Nothing,
              _text = "\n"
            }
    changeDoc doc [change]
    -- Now no errors.
    expectDiagnostics [("Foo.hs", [])]
  where
    fooContent =
      T.unlines
        [ "module Foo where",
          "import Data.Text",
          "foo :: Text",
          "foo = \"hello\""
        ]

-- A test to ensure that the command line ghcide workflow stays working
nonLspCommandLine :: TestTree
nonLspCommandLine = testGroup "ghcide command line"
  [ testCase "works" $ withTempDir $ \dir -> do
        ghcide <- locateGhcideExecutable
        copyTestDataFiles dir "multi"
        let cmd = (proc ghcide ["a/A.hs"]){cwd = Just dir}

        setEnv "HOME" "/homeless-shelter" False

        (ec, _, _) <- readCreateProcessWithExitCode cmd ""

        ec @?= ExitSuccess
  ]

benchmarkTests :: TestTree
benchmarkTests =
    let ?config = Bench.defConfig
            { Bench.verbosity = Bench.Quiet
            , Bench.repetitions = Just 3
            , Bench.buildTool = Bench.Cabal
            } in
    withResource Bench.setup Bench.cleanUp $ \getResource -> testGroup "benchmark experiments"
    [ testCase (Bench.name e) $ do
        Bench.SetupResult{Bench.benchDir} <- getResource
        res <- Bench.runBench (runInDir benchDir) e
        assertBool "did not successfully complete 5 repetitions" $ Bench.success res
        | e <- Bench.experiments
        , Bench.name e /= "edit" -- the edit experiment does not ever fail
        -- the cradle experiments are way too slow
        , not ("cradle" `isInfixOf` Bench.name e)
    ]

-- | checks if we use InitializeParams.rootUri for loading session
rootUriTests :: TestTree
rootUriTests = testCase "use rootUri" . runTest "dirA" "dirB" $ \dir -> do
  let bPath = dir </> "dirB/Foo.hs"
  liftIO $ copyTestDataFiles dir "rootUri"
  bSource <- liftIO $ readFileUtf8 bPath
  _ <- createDoc "Foo.hs" "haskell" bSource
  expectNoMoreDiagnostics 0.5
  where
    -- similar to run' except we can configure where to start ghcide and session
    runTest :: FilePath -> FilePath -> (FilePath -> Session ()) -> IO ()
    runTest dir1 dir2 s = withTempDir $ \dir -> runInDir' dir dir1 dir2 [] (s dir)

-- | Test if ghcide asynchronously handles Commands and user Requests
asyncTests :: TestTree
asyncTests = testGroup "async"
    [
      testSession "command" $ do
            -- Execute a command that will block forever
            let req = ExecuteCommandParams Nothing blockCommandId Nothing
            void $ sendRequest SWorkspaceExecuteCommand req
            -- Load a file and check for code actions. Will only work if the command is run asynchronously
            doc <- createDoc "A.hs" "haskell" $ T.unlines
              [ "{-# OPTIONS -Wmissing-signatures #-}"
              , "foo = id"
              ]
            void waitForDiagnostics
            actions <- getCodeActions doc (Range (Position 1 0) (Position 1 0))
            liftIO $ [ _title | InR CodeAction{_title} <- actions] @=?
              [ "add signature: foo :: a -> a" ]
    , testSession "request" $ do
            -- Execute a custom request that will block for 1000 seconds
            void $ sendRequest (SCustomMethod "test") $ toJSON $ BlockSeconds 1000
            -- Load a file and check for code actions. Will only work if the request is run asynchronously
            doc <- createDoc "A.hs" "haskell" $ T.unlines
              [ "{-# OPTIONS -Wmissing-signatures #-}"
              , "foo = id"
              ]
            void waitForDiagnostics
            actions <- getCodeActions doc (Range (Position 1 0) (Position 1 0))
            liftIO $ [ _title | InR CodeAction{_title} <- actions] @=?
              [ "add signature: foo :: a -> a" ]
    ]


clientSettingsTest :: TestTree
clientSettingsTest = testGroup "client settings handling"
    [ testSession "ghcide restarts shake session on config changes" $ do
            void $ skipManyTill anyMessage $ message SClientRegisterCapability
            void $ createDoc "A.hs" "haskell" "module A where"
            waitForProgressDone
            sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON ("" :: String)))
            skipManyTill anyMessage restartingBuildSession

    ]
  where
    restartingBuildSession :: Session ()
    restartingBuildSession = do
        FromServerMess SWindowLogMessage NotificationMessage{_params = LogMessageParams{..}} <- loggingNotification
        guard $ "Restarting build session" `T.isInfixOf` _message

referenceTests :: TestTree
referenceTests = testGroup "references"
    [ testGroup "can get references to FOIs"
          [ referenceTest "can get references to symbols"
                          ("References.hs", 4, 7)
                          YesIncludeDeclaration
                          [ ("References.hs", 4, 6)
                          , ("References.hs", 6, 0)
                          , ("References.hs", 6, 14)
                          , ("References.hs", 9, 7)
                          , ("References.hs", 10, 11)
                          ]

          , referenceTest "can get references to data constructor"
                          ("References.hs", 13, 2)
                          YesIncludeDeclaration
                          [ ("References.hs", 13, 2)
                          , ("References.hs", 16, 14)
                          , ("References.hs", 19, 21)
                          ]

          , referenceTest "getting references works in the other module"
                          ("OtherModule.hs", 6, 0)
                          YesIncludeDeclaration
                          [ ("OtherModule.hs", 6, 0)
                          , ("OtherModule.hs", 8, 16)
                          ]

          , referenceTest "getting references works in the Main module"
                          ("Main.hs", 9, 0)
                          YesIncludeDeclaration
                          [ ("Main.hs", 9, 0)
                          , ("Main.hs", 10, 4)
                          ]

          , referenceTest "getting references to main works"
                          ("Main.hs", 5, 0)
                          YesIncludeDeclaration
                          [ ("Main.hs", 4, 0)
                          , ("Main.hs", 5, 0)
                          ]

          , referenceTest "can get type references"
                          ("Main.hs", 9, 9)
                          YesIncludeDeclaration
                          [ ("Main.hs", 9, 0)
                          , ("Main.hs", 9, 9)
                          , ("Main.hs", 10, 0)
                          ]

          , expectFailBecause "references provider does not respect includeDeclaration parameter" $
 referenceTest "works when we ask to exclude declarations"
                          ("References.hs", 4, 7)
                          NoExcludeDeclaration
                          [ ("References.hs", 6, 0)
                          , ("References.hs", 6, 14)
                          , ("References.hs", 9, 7)
                          , ("References.hs", 10, 11)
                          ]

          , referenceTest "INCORRECTLY returns declarations when we ask to exclude them"
                          ("References.hs", 4, 7)
                          NoExcludeDeclaration
                          [ ("References.hs", 4, 6)
                          , ("References.hs", 6, 0)
                          , ("References.hs", 6, 14)
                          , ("References.hs", 9, 7)
                          , ("References.hs", 10, 11)
                          ]
          ]

    , testGroup "can get references to non FOIs"
          [ referenceTest "can get references to symbol defined in a module we import"
                          ("References.hs", 22, 4)
                          YesIncludeDeclaration
                          [ ("References.hs", 22, 4)
                          , ("OtherModule.hs", 0, 20)
                          , ("OtherModule.hs", 4, 0)
                          ]

          , referenceTest "can get references in modules that import us to symbols we define"
                          ("OtherModule.hs", 4, 0)
                          YesIncludeDeclaration
                          [ ("References.hs", 22, 4)
                          , ("OtherModule.hs", 0, 20)
                          , ("OtherModule.hs", 4, 0)
                          ]

          , referenceTest "can get references to symbol defined in a module we import transitively"
                          ("References.hs", 24, 4)
                          YesIncludeDeclaration
                          [ ("References.hs", 24, 4)
                          , ("OtherModule.hs", 0, 48)
                          , ("OtherOtherModule.hs", 2, 0)
                          ]

          , referenceTest "can get references in modules that import us transitively to symbols we define"
                          ("OtherOtherModule.hs", 2, 0)
                          YesIncludeDeclaration
                          [ ("References.hs", 24, 4)
                          , ("OtherModule.hs", 0, 48)
                          , ("OtherOtherModule.hs", 2, 0)
                          ]

          , referenceTest "can get type references to other modules"
                          ("Main.hs", 12, 10)
                          YesIncludeDeclaration
                          [ ("Main.hs", 12, 7)
                          , ("Main.hs", 13, 0)
                          , ("References.hs", 12, 5)
                          , ("References.hs", 16, 0)
                          ]
          ]
    ]

-- | When we ask for all references to symbol "foo", should the declaration "foo
-- = 2" be among the references returned?
data IncludeDeclaration =
    YesIncludeDeclaration
    | NoExcludeDeclaration

getReferences' :: SymbolLocation -> IncludeDeclaration -> Session (List Location)
getReferences' (file, l, c) includeDeclaration = do
    doc <- openDoc file "haskell"
    getReferences doc (Position l c) $ toBool includeDeclaration
    where toBool YesIncludeDeclaration = True
          toBool NoExcludeDeclaration  = False

referenceTestSession :: String -> FilePath -> [FilePath] -> (FilePath -> Session ()) -> TestTree
referenceTestSession name thisDoc docs' f = testSessionWithExtraFiles "references" name $ \dir -> do
  let docs = map (dir </>) $ delete thisDoc $ nubOrd docs'
  -- Initial Index
  docid <- openDoc thisDoc "haskell"
  let
    loop :: [FilePath] -> Session ()
    loop [] = pure ()
    loop docs = do
      doc <- skipManyTill anyMessage $ satisfyMaybe $ \case
          FromServerMess (SCustomMethod "ghcide/reference/ready") (NotMess NotificationMessage{_params = fp}) -> do
            A.Success fp' <- pure $ fromJSON fp
            find (fp' ==) docs
          _ -> Nothing
      loop (delete doc docs)
  loop docs
  f dir
  closeDoc docid

-- | Given a location, lookup the symbol and all references to it. Make sure
-- they are the ones we expect.
referenceTest :: String -> SymbolLocation -> IncludeDeclaration -> [SymbolLocation] -> TestTree
referenceTest name loc includeDeclaration expected =
    referenceTestSession name (fst3 loc) docs $ \dir -> do
        List actual <- getReferences' loc includeDeclaration
        liftIO $ actual `expectSameLocations` map (first3 (dir </>)) expected
  where
    docs = map fst3 expected

type SymbolLocation = (FilePath, Int, Int)

expectSameLocations :: [Location] -> [SymbolLocation] -> Assertion
expectSameLocations actual expected = do
    let actual' =
            Set.map (\location -> (location ^. L.uri
                                   , location ^. L.range . L.start . L.line
                                   , location ^. L.range . L.start . L.character))
            $ Set.fromList actual
    expected' <- Set.fromList <$>
        (forM expected $ \(file, l, c) -> do
                              fp <- canonicalizePath file
                              return (filePathToUri fp, l, c))
    actual' @?= expected'

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

testSession :: String -> Session () -> TestTree
testSession name = testCase name . run

testSessionWithExtraFiles :: FilePath -> String -> (FilePath -> Session ()) -> TestTree
testSessionWithExtraFiles prefix name = testCase name . runWithExtraFiles prefix

testSession' :: String -> (FilePath -> Session ()) -> TestTree
testSession' name = testCase name . run'

testSessionWait :: HasCallStack => String -> Session () -> TestTree
testSessionWait name = testSession name .
      -- Check that any diagnostics produced were already consumed by the test case.
      --
      -- If in future we add test cases where we don't care about checking the diagnostics,
      -- this could move elsewhere.
      --
      -- Experimentally, 0.5s seems to be long enough to wait for any final diagnostics to appear.
      ( >> expectNoMoreDiagnostics 0.5)

pickActionWithTitle :: T.Text -> [Command |? CodeAction] -> IO CodeAction
pickActionWithTitle title actions = do
  assertBool ("Found no matching actions for " <> show title <> " in " <> show titles) (not $ null matches)
  return $ head matches
  where
    titles =
        [ actionTitle
        | InR CodeAction { _title = actionTitle } <- actions
        ]
    matches =
        [ action
        | InR action@CodeAction { _title = actionTitle } <- actions
        , title == actionTitle
        ]

mkRange :: Int -> Int -> Int -> Int -> Range
mkRange a b c d = Range (Position a b) (Position c d)

run :: Session a -> IO a
run s = run' (const s)

runWithExtraFiles :: FilePath -> (FilePath -> Session a) -> IO a
runWithExtraFiles prefix s = withTempDir $ \dir -> do
  copyTestDataFiles dir prefix
  runInDir dir (s dir)

copyTestDataFiles :: FilePath -> FilePath -> IO ()
copyTestDataFiles dir prefix = do
  -- Copy all the test data files to the temporary workspace
  testDataFiles <- getDirectoryFilesIO ("test/data" </> prefix) ["//*"]
  for_ testDataFiles $ \f -> do
    createDirectoryIfMissing True $ dir </> takeDirectory f
    copyFile ("test/data" </> prefix </> f) (dir </> f)

run' :: (FilePath -> Session a) -> IO a
run' s = withTempDir $ \dir -> runInDir dir (s dir)

runInDir :: FilePath -> Session a -> IO a
runInDir dir = runInDir' dir "." "." []

withLongTimeout :: IO a -> IO a
withLongTimeout = bracket_ (setEnv "LSP_TIMEOUT" "120" True) (unsetEnv "LSP_TIMEOUT")

-- | Takes a directory as well as relative paths to where we should launch the executable as well as the session root.
runInDir' :: FilePath -> FilePath -> FilePath -> [String] -> Session a -> IO a
runInDir' dir startExeIn startSessionIn extraOptions s = do
  ghcideExe <- locateGhcideExecutable
  let startDir = dir </> startExeIn
  let projDir = dir </> startSessionIn

  createDirectoryIfMissing True startDir
  createDirectoryIfMissing True projDir
  -- Temporarily hack around https://github.com/mpickering/hie-bios/pull/56
  -- since the package import test creates "Data/List.hs", which otherwise has no physical home
  createDirectoryIfMissing True $ projDir ++ "/Data"

  shakeProfiling <- getEnv "SHAKE_PROFILING"
  let cmd = unwords $
       [ghcideExe, "--lsp", "--test", "--verbose", "-j2", "--cwd", startDir
       ] ++ ["--shake-profiling=" <> dir | Just dir <- [shakeProfiling]
       ] ++ extraOptions
  -- HIE calls getXgdDirectory which assumes that HOME is set.
  -- Only sets HOME if it wasn't already set.
  setEnv "HOME" "/homeless-shelter" False
  conf <- getConfigFromEnv
  runSessionWithConfig conf cmd lspTestCaps projDir s

getConfigFromEnv :: IO SessionConfig
getConfigFromEnv = do
  logColor <- fromMaybe True <$> checkEnv "LSP_TEST_LOG_COLOR"
  timeoutOverride <- fmap read <$> getEnv "LSP_TIMEOUT"
  return defaultConfig
    { messageTimeout = fromMaybe (messageTimeout defaultConfig) timeoutOverride
    , logColor
    }
  where
    checkEnv :: String -> IO (Maybe Bool)
    checkEnv s = fmap convertVal <$> getEnv s
    convertVal "0" = False
    convertVal _   = True

lspTestCaps :: ClientCapabilities
lspTestCaps = fullCaps { _window = Just $ WindowClientCapabilities $ Just True }

openTestDataDoc :: FilePath -> Session TextDocumentIdentifier
openTestDataDoc path = do
  source <- liftIO $ readFileUtf8 $ "test/data" </> path
  createDoc path "haskell" source

findCodeActions :: TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActions = findCodeActions' (==) "is not a superset of"

findCodeActionsByPrefix :: TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActionsByPrefix = findCodeActions' T.isPrefixOf "is not prefix of"

findCodeActions' :: (T.Text -> T.Text -> Bool) -> String -> TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActions' op errMsg doc range expectedTitles = do
  actions <- getCodeActions doc range
  let matches = sequence
        [ listToMaybe
          [ action
          | InR action@CodeAction { _title = actionTitle } <- actions
          , expectedTitle `op` actionTitle]
        | expectedTitle <- expectedTitles]
  let msg = show
            [ actionTitle
            | InR CodeAction { _title = actionTitle } <- actions
            ]
            ++ " " <> errMsg <> " "
            ++ show expectedTitles
  liftIO $ case matches of
    Nothing -> assertFailure msg
    Just _  -> pure ()
  return (fromJust matches)

findCodeAction :: TextDocumentIdentifier -> Range -> T.Text -> Session CodeAction
findCodeAction doc range t = head <$> findCodeActions doc range [t]

unitTests :: TestTree
unitTests = do
  testGroup "Unit"
     [ testCase "empty file path does NOT work with the empty String literal" $
         uriToFilePath' (fromNormalizedUri $ filePathToUri' "") @?= Just "."
     , testCase "empty file path works using toNormalizedFilePath'" $
         uriToFilePath' (fromNormalizedUri $ filePathToUri' (toNormalizedFilePath' "")) @?= Just ""
     , testCase "empty path URI" $ do
         Just URI{..} <- pure $ parseURI (T.unpack $ getUri $ fromNormalizedUri emptyPathUri)
         uriScheme @?= "file:"
         uriPath @?= ""
     , testCase "from empty path URI" $ do
         let uri = Uri "file://"
         uriToFilePath' uri @?= Just ""
     , testCase "Key with empty file path roundtrips via Binary"  $
         Binary.decode (Binary.encode (Q ((), emptyFilePath))) @?= Q ((), emptyFilePath)
     , testCase "showDiagnostics prints ranges 1-based (like vscode)" $ do
         let diag = ("", Diagnostics.ShowDiag, Diagnostic
               { _range = Range
                   { _start = Position{_line = 0, _character = 1}
                   , _end = Position{_line = 2, _character = 3}
                   }
               , _severity = Nothing
               , _code = Nothing
               , _source = Nothing
               , _message = ""
               , _relatedInformation = Nothing
               , _tags = Nothing
               })
         let shown = T.unpack (Diagnostics.showDiagnostics [diag])
         let expected = "1:2-3:4"
         assertBool (unwords ["expected to find range", expected, "in diagnostic", shown]) $
             expected `isInfixOf` shown
     , testCase "notification handlers run sequentially" $ do
        orderRef <- newIORef []
        let plugins = pluginDescToIdePlugins $
                [ (defaultPluginDescriptor $ fromString $ show i)
                    { pluginNotificationHandlers = mconcat
                        [ mkPluginNotificationHandler LSP.STextDocumentDidOpen $ \_ _ _ ->
                            liftIO $ atomicModifyIORef_ orderRef (i:)
                        ]
                    }
                    | i <- [(1::Int)..20]
                ] ++ Ghcide.descriptors

        testIde def{IDE.argsHlsPlugins = plugins} $ do
            _ <- createDoc "haskell" "A.hs" "module A where"
            waitForProgressDone
            actualOrder <- liftIO $ readIORef orderRef

            liftIO $ actualOrder @?= reverse [(1::Int)..20]
     , testCase "timestamps have millisecond resolution" $ do
         resolution_us <- findResolution_us 1
         let msg = printf "Timestamps do not have millisecond resolution: %dus" resolution_us
         assertBool msg (resolution_us <= 1000)
     , Progress.tests
     ]

findResolution_us :: Int -> IO Int
findResolution_us delay_us | delay_us >= 1000000 = error "Unable to compute timestamp resolution"
findResolution_us delay_us = withTempFile $ \f -> withTempFile $ \f' -> do
    writeFile f ""
    threadDelay delay_us
    writeFile f' ""
    t <- getModTime f
    t' <- getModTime f'
    if t /= t' then return delay_us else findResolution_us (delay_us * 10)


testIde :: IDE.Arguments -> Session () -> IO ()
testIde arguments session = do
    config <- getConfigFromEnv
    (hInRead, hInWrite) <- createPipe
    (hOutRead, hOutWrite) <- createPipe
    let server = IDE.defaultMain arguments
            { IDE.argsHandleIn = pure hInRead
            , IDE.argsHandleOut = pure hOutWrite
            }

    withAsync server $ \_ ->
        runSessionWithHandles hInWrite hOutRead config lspTestCaps "." session

positionMappingTests :: TestTree
positionMappingTests =
    testGroup "position mapping"
        [ testGroup "toCurrent"
              [ testCase "before" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 0) @?= PositionExact (Position 0 0)
              , testCase "after, same line, same length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 3) @?= PositionExact (Position 0 3)
              , testCase "after, same line, increased length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 0 3) @?= PositionExact (Position 0 4)
              , testCase "after, same line, decreased length" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "a"
                    (Position 0 3) @?= PositionExact (Position 0 2)
              , testCase "after, next line, no newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 1 3) @?= PositionExact (Position 1 3)
              , testCase "after, next line, newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\ndef"
                    (Position 1 0) @?= PositionExact (Position 2 0)
              , testCase "after, same line, newline" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd"
                    (Position 0 4) @?= PositionExact (Position 1 2)
              , testCase "after, same line, newline + newline at end" $
                toCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd\n"
                    (Position 0 4) @?= PositionExact (Position 2 1)
              , testCase "after, same line, newline + newline at end" $
                toCurrent
                    (Range (Position 0 1) (Position 0 1))
                    "abc"
                    (Position 0 1) @?= PositionExact (Position 0 4)
              ]
        , testGroup "fromCurrent"
              [ testCase "before" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 0) @?= PositionExact (Position 0 0)
              , testCase "after, same line, same length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "ab"
                    (Position 0 3) @?= PositionExact (Position 0 3)
              , testCase "after, same line, increased length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 0 4) @?= PositionExact (Position 0 3)
              , testCase "after, same line, decreased length" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "a"
                    (Position 0 2) @?= PositionExact (Position 0 3)
              , testCase "after, next line, no newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc"
                    (Position 1 3) @?= PositionExact (Position 1 3)
              , testCase "after, next line, newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\ndef"
                    (Position 2 0) @?= PositionExact (Position 1 0)
              , testCase "after, same line, newline" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd"
                    (Position 1 2) @?= PositionExact (Position 0 4)
              , testCase "after, same line, newline + newline at end" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 3))
                    "abc\nd\n"
                    (Position 2 1) @?= PositionExact (Position 0 4)
              , testCase "after, same line, newline + newline at end" $
                fromCurrent
                    (Range (Position 0 1) (Position 0 1))
                    "abc"
                    (Position 0 4) @?= PositionExact (Position 0 1)
              ]
        , adjustOption (\(QuickCheckTests i) -> QuickCheckTests (max 1000 i)) $ testGroup "properties"
              [ testProperty "fromCurrent r t <=< toCurrent r t" $ do
                -- Note that it is important to use suchThatMap on all values at once
                -- instead of only using it on the position. Otherwise you can get
                -- into situations where there is no position that can be mapped back
                -- for the edit which will result in QuickCheck looping forever.
                let gen = do
                        rope <- genRope
                        range <- genRange rope
                        PrintableText replacement <- arbitrary
                        oldPos <- genPosition rope
                        pure (range, replacement, oldPos)
                forAll
                    (suchThatMap gen
                        (\(range, replacement, oldPos) -> positionResultToMaybe $ (range, replacement, oldPos,) <$> toCurrent range replacement oldPos)) $
                    \(range, replacement, oldPos, newPos) ->
                    fromCurrent range replacement newPos === PositionExact oldPos
              , testProperty "toCurrent r t <=< fromCurrent r t" $ do
                let gen = do
                        rope <- genRope
                        range <- genRange rope
                        PrintableText replacement <- arbitrary
                        let newRope = applyChange rope (TextDocumentContentChangeEvent (Just range) Nothing replacement)
                        newPos <- genPosition newRope
                        pure (range, replacement, newPos)
                forAll
                    (suchThatMap gen
                        (\(range, replacement, newPos) -> positionResultToMaybe $ (range, replacement, newPos,) <$> fromCurrent range replacement newPos)) $
                    \(range, replacement, newPos, oldPos) ->
                    toCurrent range replacement oldPos === PositionExact newPos
              ]
        ]

newtype PrintableText = PrintableText { getPrintableText :: T.Text }
    deriving Show

instance Arbitrary PrintableText where
    arbitrary = PrintableText . T.pack . getPrintableString <$> arbitrary


genRope :: Gen Rope
genRope = Rope.fromText . getPrintableText <$> arbitrary

genPosition :: Rope -> Gen Position
genPosition r = do
    row <- choose (0, max 0 $ rows - 1)
    let columns = Rope.columns (nthLine row r)
    column <- choose (0, max 0 $ columns - 1)
    pure $ Position row column
    where rows = Rope.rows r

genRange :: Rope -> Gen Range
genRange r = do
    startPos@(Position startLine startColumn) <- genPosition r
    let maxLineDiff = max 0 $ rows - 1 - startLine
    endLine <- choose (startLine, startLine + maxLineDiff)
    let columns = Rope.columns (nthLine endLine r)
    endColumn <-
        if startLine == endLine
            then choose (startColumn, columns)
            else choose (0, max 0 $ columns - 1)
    pure $ Range startPos (Position endLine endColumn)
    where rows = Rope.rows r

-- | Get the ith line of a rope, starting from 0. Trailing newline not included.
nthLine :: Int -> Rope -> Rope
nthLine i r
    | i < 0 = error $ "Negative line number: " <> show i
    | i == 0 && Rope.rows r == 0 = r
    | i >= Rope.rows r = error $ "Row number out of bounds: " <> show i <> "/" <> show (Rope.rows r)
    | otherwise = Rope.takeWhile (/= '\n') $ fst $ Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (i - 1) r

getWatchedFilesSubscriptionsUntil :: forall m. SServerMethod m -> Session [DidChangeWatchedFilesRegistrationOptions]
getWatchedFilesSubscriptionsUntil m = do
      msgs <- manyTill (Just <$> message SClientRegisterCapability <|> Nothing <$ anyMessage) (message m)
      return
            [ args
            | Just RequestMessage{_params = RegistrationParams (List regs)} <- msgs
            , SomeRegistration (Registration _id SWorkspaceDidChangeWatchedFiles args) <- regs
            ]

-- | Version of 'System.IO.Extra.withTempDir' that canonicalizes the path
-- Which we need to do on macOS since the $TMPDIR can be in @/private/var@ or
-- @/var@
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir f = System.IO.Extra.withTempDir $ \dir -> do
  dir' <- canonicalizePath dir
  f dir'

-- | Assert that a value is not 'Nothing', and extract the value.
assertJust :: MonadIO m => String -> Maybe a -> m a
assertJust s = \case
  Nothing -> liftIO $ assertFailure s
  Just x  -> pure x

-- | Before ghc9, lists of Char is displayed as [Char], but with ghc9 and up, it's displayed as String
listOfChar :: T.Text
listOfChar | ghcVersion >= GHC90 = "String"
           | otherwise = "[Char]"

-- | Ghc 9 doesn't include the $-sign in TH warnings like earlier versions did
thDollarIdx :: Int
thDollarIdx | ghcVersion >= GHC90 = 1
            | otherwise = 0
