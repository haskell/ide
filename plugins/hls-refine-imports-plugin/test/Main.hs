{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}

module Main
  ( main
  ) where

import           Data.Foldable            (find, forM_)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Ide.Plugin.RefineImports as RefineImports
import           System.FilePath          ((<.>), (</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner $
  testGroup
    "Refine Imports"
    [ codeActionGoldenTest refineAction "WithOverride" 3 1
    , codeActionGoldenTest removeUnusedAction "UnusedImport" 3 1
    , codeLensGoldenTest "UsualCase" 1
    , codeLensGoldenTest "UnusedImportLens" 1
    ]

  where
    refineAction = "Refine all imports"
    removeUnusedAction = "Remove all unused imports"

refineImportsPlugin :: PluginDescriptor IdeState
refineImportsPlugin = RefineImports.descriptor "refineImports"

-- code action tests

codeActionGoldenTest :: Text -> FilePath -> Int -> Int -> TestTree
codeActionGoldenTest actName fp l c = goldenWithRefineImports fp $ \doc -> do
  actions <- getCodeActions doc (pointRange l c)
  case find ((== Just actName) . caTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

caTitle :: (Command |? CodeAction) -> Maybe Text
caTitle (InR CodeAction {_title}) = Just _title
caTitle _                         = Nothing


-- code lens tests

codeLensGoldenTest :: FilePath -> Int -> TestTree
codeLensGoldenTest fp codeLensIdx = goldenWithRefineImports fp $ \doc -> do
  codeLens <- (!! codeLensIdx) <$> getCodeLensesBy isRefineImports doc
  mapM_ executeCmd
    [c | CodeLens{_command = Just c} <- [codeLens]]

getCodeLensesBy :: (CodeLens -> Bool) -> TextDocumentIdentifier -> Session [CodeLens]
getCodeLensesBy f doc = filter f <$> getCodeLenses doc

isRefineImports :: CodeLens -> Bool
isRefineImports (CodeLens _ (Just (Command _ cmd _)) _)
  | ":refineImports:" `T.isInfixOf` cmd = True
isRefineImports _ = False

-- Execute command and wait for result
executeCmd :: Command -> Session ()
executeCmd cmd = do
    executeCommand cmd
    _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
    -- liftIO $ print _resp
    return ()

-- helpers

goldenWithRefineImports :: FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRefineImports fp = goldenWithHaskellDoc refineImportsPlugin (fp <> " (golden)") testDataDir fp "expected" "hs"

testDataDir :: String
testDataDir = "test" </> "testdata"

pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> line)
  (subtract 1 -> col) =
    Range (Position line col) (Position line $ col + 1)
