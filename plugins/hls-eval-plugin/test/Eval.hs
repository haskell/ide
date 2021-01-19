{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval (
    tests,
) where

import Control.Applicative.Combinators (
    skipManyTill,
 )
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Test (
    Session,
    anyMessage,
    documentContents,
    executeCommand,
    fullCaps,
    getCodeLenses,
    message,
    openDoc,
    runSession,
 )
import Language.Haskell.LSP.Types (
    ApplyWorkspaceEditRequest,
    CodeLens (CodeLens, _command, _range),
    Command (Command, _title),
    Position (..),
    Range (..),
    TextDocumentIdentifier,
 )
import System.Directory (doesFileExist)
import System.FilePath (
    (<.>),
    (</>),
 )
import Test.Hls.Util (hlsCommand)
import Test.Tasty (
    TestTree,
    testGroup,
 )
import Test.Tasty.ExpectedFailure (
    expectFailBecause,
    ignoreTestBecause,
 )
import Test.Tasty.HUnit (
    testCase,
    (@?=),
 )

tests :: TestTree
tests =
    testGroup
        "eval"
        [ testCase "Produces Evaluate code lenses" $
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T1.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map (fmap _title . _command) lenses @?= [Just "Evaluate..."]
        , testCase "Produces Refresh code lenses" $
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T2.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map (fmap _title . _command) lenses @?= [Just "Refresh..."]
        , testCase "Code lenses have ranges" $
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T1.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map _range lenses @?= [Range (Position 4 0) (Position 5 0)]
        , testCase "Multi-line expressions have a multi-line range" $ do
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T3.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map _range lenses @?= [Range (Position 3 0) (Position 5 0)]
        , testCase "Executed expressions range covers only the expression" $ do
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T2.hs" "haskell"
                lenses <- getEvalCodeLenses doc
                liftIO $ map _range lenses @?= [Range (Position 4 0) (Position 5 0)]
        , gtest "Evaluation of expressions" "T1"
        , gtest "Reevaluation of expressions" "T2"
        , gtest "Evaluation of expressions w/ imports" "T3"
        , gtest "Evaluation of expressions w/ lets" "T4"
        , gtest "Refresh an evaluation" "T5"
        , gtest "Refresh an evaluation w/ lets" "T6"
        , gtest "Refresh a multiline evaluation" "T7"
        , gtest "Semantic and Lexical errors are reported" "T8"
        , gtest "Applies file LANGUAGE extensions" "T9"
        , gtest "Evaluate a type with :kind!" "T10"
        , gtest
            "Reports an error for an incorrect type with :kind!"
            "T11"
        , gtest "Shows a kind with :kind" "T12"
        , gtest
            "Reports an error for an incorrect type with :kind"
            "T13"
        , gtest
            "Returns a fully-instantiated type for :type"
            "T14"
        , gtest
            "Returns an uninstantiated type for :type +v, admitting multiple whitespaces around arguments"
            "T15"
        , gtest
            "Returns defaulted type for :type +d, admitting multiple whitespaces around arguments"
            "T16"
        , gtest
            ":type reports an error when given with unknown +x option"
            "T17"
        , gtest
            "Reports an error when given with unknown command"
            "T18"
        , gtest
            "Returns defaulted type for :type +d reflecting the default declaration specified in the >>> prompt"
            "T19"
        , expectFailBecause
            "known issue - see a note in P.R. #361"
            $ gtest
                ":type +d reflects the `default' declaration of the module"
                "T20"
        , gtest
            ":type handles a multilined result properly"
            "T21"
        , gtest
            ":t behaves exactly the same as :type"
            "T22"
        , gtest ":type does \"dovetails\" for short identifiers" "T23"
        , gtest ":kind! treats a multilined result properly" "T24"
        , gtest ":kind treats a multilined result properly" "T25"
        , gtest "local imports" "T26"
        , gtest
            "Preserves one empty comment line after prompt"
            "T27"
        , gtest
            "Multi line comments"
            "TMulti"
        , gtest
            "Evaluate expressions in Plain comments in both single line and multi line format"
            "TPlainComment"
        , gtest
            "Evaluate expressions in Haddock comments in both single line and multi line format"
            "THaddock"
        , gtest "Compare results (for Haddock tests only)" "TCompare"
        , gtest "Local Modules imports are accessible in a test" "TLocalImport"
        , gtest "Local Modules can be imported in a test" "TLocalImportInTest"
        , gtest
            "Setting language options"
            "TLanguageOptions"
        , ignoreTestBecause
            "Unexplained but minor issue"
            $ gtest
                "Setting language option TupleSections"
                "TLanguageOptionsTupleSections"
        , gtest "IO expressions are supported, stdout/stderr output is ignored" "TIO"
        , gtest "Property checking" "TProperty"
        , gtest
            "Prelude has no special treatment, it is imported as stated in the module"
            "TPrelude"
        , gtest "Lenses" "TLens"
#if __GLASGOW_HASKELL__ >= 808
            , gtest "CPP support" "TCPP"
            , gtest_ ".lhs" "Literate Haskell Bird Style" "TLHS"
#endif
        -- , gtest "Literate Haskell LaTeX Style"  "TLHSLateX.lhs"
        ]
  where
    gtest = gtest_ ".hs"
    gtest_ ext name file = testCase name $ goldenTest (file ++ ext)

goldenTest :: FilePath -> IO ()
goldenTest = goldenTestBy isEvalTest

{- |Execute all CodeLens accepted by 'filter'
 Compare results with the contents of corresponding '.expected' file (and creates it, if missing)
-}
goldenTestBy :: (CodeLens -> Bool) -> FilePath -> IO ()
goldenTestBy f input = runSession hlsCommand fullCaps evalPath $ do
    doc <- openDoc input "haskell"

    -- Execute lenses backwards, to avoid affecting their position in the source file
    codeLenses <- reverse <$> getCodeLensesBy f doc
    -- liftIO $ print codeLenses

    -- Execute sequentially
    mapM_ executeCmd $ [c | CodeLens{_command = Just c} <- codeLenses]

    edited <- replaceUnicodeQuotes <$> documentContents doc
    -- liftIO $ T.putStrLn edited

    let expectedFile = evalPath </> input <.> "expected"

    liftIO $ do
        -- Write expected file if missing
        missingExpected <- not <$> doesFileExist expectedFile
        when missingExpected $ T.writeFile expectedFile edited

    expected <- liftIO $ T.readFile expectedFile
    liftIO $ edited @?= expected

getEvalCodeLenses :: TextDocumentIdentifier -> Session [CodeLens]
getEvalCodeLenses = getCodeLensesBy isEvalTest

getCodeLensesBy :: (CodeLens -> Bool) -> TextDocumentIdentifier -> Session [CodeLens]
getCodeLensesBy f doc = filter f <$> getCodeLenses doc

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
    executeCommand cmd
    _resp :: ApplyWorkspaceEditRequest <- skipManyTill anyMessage message
    -- liftIO $ print _resp
    return ()

-- Execute only Eval tests to avoid interference from other plugins (e.g ghcide:typesignature.add)
isEvalTest :: CodeLens -> Bool
isEvalTest (CodeLens _ (Just (Command _ cmd _)) _)
    | ":eval:" `T.isInfixOf` cmd = True
isEvalTest _ = False

replaceUnicodeQuotes :: T.Text -> T.Text
replaceUnicodeQuotes = T.replace "‘" "'" . T.replace "’" "'"

evalPath :: FilePath
evalPath = "plugins/hls-eval-plugin/test/testdata"
