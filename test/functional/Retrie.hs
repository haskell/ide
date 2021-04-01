{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Retrie where

import           Control.Applicative.Combinators (skipManyTill)
import           Control.Monad.IO.Class
import           Data.List                       (find)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Language.LSP.Test
import           Language.LSP.Types
import           System.FilePath
import           Test.Hls

tests :: TestTree
tests = testGroup "retrie"
    [ goldenTest "FoldLocalTypeSynonym.hs" (containsWords "Fold in file") 2 0
    , goldenTest "FoldLocalTypeSynonymAtUse.hs" (containsWords "Use type synonym") 4 8
    ]

goldenTest :: FilePath -> (Text -> Bool) -> Int -> Int -> TestTree
goldenTest input actionPred line col =
    testCase input $ do
        runSession testCommand fullCaps spliceTestPath $ do
            doc <- openDoc input "haskell"
            _ <- waitForDiagnostics
            actions <- getCodeActions doc $ pointRange line col
            case find (maybe False actionPred . codeActionTitle) actions of
                Just (InR action) -> do
                    executeCodeAction action
                    _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
                    edited <- documentContents doc
                    let expected_name = spliceTestPath </> input <.> "expected"
                    expected <- liftIO $ T.readFile expected_name
                    liftIO $ edited @?= expected
                _ -> liftIO $ assertFailure "No CodeAction detected"


spliceTestPath :: FilePath
spliceTestPath = "test/testdata/retrie"

pointRange :: Int -> Int -> Range
pointRange line col = Range (Position line col) (Position line $ col + 1)

-- | Get the title of a code action.
codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle InL{}                               = Nothing
codeActionTitle (InR(CodeAction title _ _ _ _ _ _)) = Just title

containsWords :: Text -> Text -> Bool
containsWords (T.words -> targets) input = all (`T.isInfixOf` input) targets
