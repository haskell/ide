{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.RefineImports
  ( descriptor
  , RefineImports(..)
  , RefineImportsResult(..)
  , RemoveUnusedImports(..)
  , RemoveUnusedImportsResult(..)
  , mkExplicitEdit
  , mkExplicitEdit'
  ) where

import           Avail                                             (AvailInfo (Avail),
                                                                    availName,
                                                                    availNames,
                                                                    availNamesWithSelectors)
import           Control.DeepSeq                                   (rwhnf)
import           Control.Monad.IO.Class                            (liftIO)
import           Data.Aeson.Types
import qualified Data.HashMap.Strict                               as HashMap
import           Data.List.Extra                                   (notNull)
import qualified Data.Map.Strict                                   as Map
import           Data.Maybe                                        (catMaybes,
                                                                    fromMaybe)
import qualified Data.Set                                          as S
import qualified Data.Text                                         as T
import           Data.Traversable                                  (forM)
import           Development.IDE                                   hiding
                                                                   (getFileContents)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat                        (AvailInfo,
                                                                    GenLocated (L),
                                                                    GhcRn,
                                                                    HsModule (hsmodImports),
                                                                    ImportDecl (ImportDecl, ideclHiding, ideclName),
                                                                    LIE,
                                                                    LImportDecl,
                                                                    Module (moduleName),
                                                                    ModuleName,
                                                                    ParsedModule (ParsedModule, pm_parsed_source),
                                                                    SrcSpan (RealSrcSpan),
                                                                    getLoc,
                                                                    ieName,
                                                                    noLoc,
                                                                    tcg_exports,
                                                                    unLoc)
import           Development.IDE.Graph.Classes
import           Development.IDE.Plugin.CodeAction.PositionIndexed (extendToIncludePreviousNewlineIfPossible,
                                                                    indexedByPosition)
import           GHC.Generics                                      (Generic)
import           Ide.Plugin.ExplicitImports                        (within)
import qualified Ide.Plugin.ExplicitImports                        as EI
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import           PrelNames                                         (pRELUDE)

-- | plugin declaration
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginCommands = [refineImportCommand]
  , pluginRules = refineImportsRule <> removeUnusedImportsRule
  , pluginHandlers = mconcat
      [ -- This plugin provides code lenses
        mkPluginHandler STextDocumentCodeLens lensProvider
        -- This plugin provides code actions
      , mkPluginHandler STextDocumentCodeAction codeActionProvider
      ]
  }

refineImportCommandId :: CommandId
refineImportCommandId = "RefineImportLensCommand"

newtype RefineImportCommandParams = RefineImportCommandParams WorkspaceEdit
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

-- | The command descriptor
refineImportCommand :: PluginCommand IdeState
refineImportCommand =
  PluginCommand
    { commandId = refineImportCommandId
    , commandDesc = "Directly use the imports as oppose to using aggregation module"
    , commandFunc = runRefineImportCommand
    }

-- | The actual command handler
runRefineImportCommand :: CommandFunction IdeState RefineImportCommandParams
runRefineImportCommand _state (RefineImportCommandParams edit) = do
  -- This command simply triggers a workspace edit!
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  return (Right Null)

lensProvider :: PluginMethodHandler IdeState TextDocumentCodeLens
lensProvider
  state -- ghcide state
  pId
  CodeLensParams {_textDocument = TextDocumentIdentifier {_uri}}
    -- VSCode uses URIs instead of file paths
    -- haskell-lsp provides conversion functions
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri = liftIO $
      do
        mbRefinedImports <-
          runIde state $ useWithStale RefineImports nfp
        mbRemoveUnusedImports <-
          runIde state $ useWithStale RemoveUnusedImports nfp
        retRefinedImports <- case mbRefinedImports of
          -- Implement the provider logic:
          -- for every refined import, generate a code lens
          Just (RefineImportsResult result, posMapping) -> do
            commands <-
              sequence
                [ generateLens' pId _uri edit
                | (imp, Just refinedImports) <- result
                , Just edit <- [mkExplicitEdit' posMapping imp refinedImports]
                ]
            return $ List $ catMaybes commands
          _ -> return $ List []
        retRemoveUnusedImports <- case mbRemoveUnusedImports of
          -- Implement the provider logic:
          -- for every unused import, generate a code lens
          Just (RemoveUnusedImportsResult result, posMapping) -> do
            mbContent <- getFileContents state nfp
            commands <-
              sequence
                [ generateLens pId _uri edit rng
                | imp <- result
                , Just (edit, rng) <- [mkExplicitEdit mbContent posMapping imp ""]
                ]
            return $ List $ catMaybes commands
          _ -> return $ List []

        return $ Right $ retRefinedImports <> retRemoveUnusedImports
    | otherwise =
      return $ Right (List [])

-- | Provide one code action to refine all imports
codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState _pId (CodeActionParams _ _ docId range _context)
  | TextDocumentIdentifier {_uri} <- docId,
    Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri = liftIO $
    do
      pm <- runIde ideState $ use GetParsedModule nfp
      let insideImport = case pm of
            Just ParsedModule {pm_parsed_source}
              | locImports <- hsmodImports (unLoc pm_parsed_source),
                rangesImports <- map getLoc locImports ->
                any (within range) rangesImports
            _ -> False
      if not insideImport
        then return (Right (List []))
        else do
          mbRefinedImports <- runIde ideState $ use RefineImports nfp
          mbRemoveUnusedImports <- runIde ideState $ use RemoveUnusedImports nfp
          mbContent <- getFileContents ideState nfp
          let
              -- common fields
              _documentChanges = Nothing
              _diagnostics = Nothing
              _isPreferred = Nothing
              _disabled = Nothing
              _xdata = Nothing
              _changeAnnotations = Nothing
              refineImportEdits =
                [ e
                | Just (RefineImportsResult result) <- [mbRefinedImports]
                , (imp, Just refinedImports) <- result
                , Just e <- [mkExplicitEdit' zeroMapping imp refinedImports]
                ]
              caRefineImports =
                let
                  _title = "Refine all imports"
                  _kind = Just $ CodeActionUnknown "quickfix.import.refine"
                  _command = Nothing
                  _edit = Just WorkspaceEdit
                    {_changes, _documentChanges, _changeAnnotations}
                  _changes = Just $ HashMap.singleton _uri $ List refineImportEdits
                in InR CodeAction {..}
              removeUnusedImportEdits =
                [ e
                | Just (RemoveUnusedImportsResult result) <- [mbRemoveUnusedImports]
                , imp <- result
                , Just (e, _rng) <- [mkExplicitEdit mbContent zeroMapping imp ""]
                ]
              caRemoveUnusedImports =
                let
                  _title = "Remove all unused imports"
                  _kind = Just $ CodeActionUnknown "quickfix.import.remove_unused"
                  _command = Nothing
                  _edit = Just WorkspaceEdit
                    {_changes, _documentChanges, _changeAnnotations}
                  _changes = Just $ HashMap.singleton _uri $ List removeUnusedImportEdits
                in InR CodeAction {..}
          return $ Right $ List
              $ [caRefineImports | not (null refineImportEdits)]
             <> [caRemoveUnusedImports | not (null removeUnusedImportEdits)]
  | otherwise =
    return $ Right $ List []

--------------------------------------------------------------------------------

data RefineImports = RefineImports
  deriving (Show, Generic, Eq, Ord)

instance Hashable RefineImports
instance NFData RefineImports
instance Binary RefineImports
type instance RuleResult RefineImports = RefineImportsResult

newtype RefineImportsResult = RefineImportsResult
  {getRefineImportsResult :: [(LImportDecl GhcRn, Maybe T.Text)]}

instance Show RefineImportsResult where show _ = "<refineImportsResult>"
instance NFData RefineImportsResult where rnf = rwhnf

refineImportsRule :: Rules ()
refineImportsRule = define $ \RefineImports nfp -> do
  -- 2 layer map ModuleName -> ModuleName -> [Avails] (exports)
  import2Map <- do
    -- first layer is from current(editing) module to its imports
    ImportMap currIm <- use_ GetImportMap nfp
    forM currIm $ \path -> do
      -- second layer is from the imports of first layer to their imports
      ImportMap importIm <- use_ GetImportMap path
      forM importIm $ \imp_path -> do
        imp_tmr <- use_ TypeCheck imp_path
        return $ tcg_exports $ tmrTypechecked imp_tmr

  (minImports, _) <- extractMinimalImports nfp

  let filterByImport
        :: LImportDecl GhcRn
        -> Map.Map ModuleName [AvailInfo]
        -> Maybe (Map.Map ModuleName [AvailInfo])
      filterByImport (L _ ImportDecl{ideclHiding = Just (_, L _ names)}) avails =
        let importedNames = S.fromList $ map (ieName . unLoc) names
            res = flip Map.filter avails $ \a ->
                    any (`S.member` importedNames)
                      $ concatMap availNamesWithSelectors a
            allFilteredAvailsNames = S.fromList
              $ concatMap availNamesWithSelectors
              $ mconcat
              $ Map.elems res
            -- if there is a function defined in the current module and is used
            -- i.e. if a function is not reexported but defined in current
            -- module then this import cannot be refined
        in if importedNames `S.isSubsetOf` allFilteredAvailsNames
              then Just res
              else Nothing
      filterByImport _ _ = Nothing
  let constructImport
        :: LImportDecl GhcRn
        -> (ModuleName, [AvailInfo])
        -> LImportDecl GhcRn
      constructImport
        i@(L lim id@ImportDecl
                  {ideclName = L _ mn, ideclHiding = Just (hiding, L _ names)})
        (newModuleName, avails) = L lim id
          { ideclName = noLoc newModuleName
          , ideclHiding = Just (hiding, noLoc newNames)
          }
          where newNames = filter (\n -> any (n `containsAvail`) avails) names
      constructImport lim _ = lim
  let res =
        [ (i, Just
                . T.intercalate "\n"
                . map (T.pack . prettyPrint . constructImport i)
                . Map.toList
                $ filteredInnerImports)
        -- for every minimal imports
        | i@(L _ ImportDecl{ideclName = L _ mn}) <- minImports
        -- we check for the inner imports
        , Just innerImports <- [Map.lookup mn import2Map]
        -- and only get those symbols used
        , Just filteredInnerImports <- [filterByImport i innerImports]
        -- if no symbols from this modules then don't need to generate new import
        , not $ null filteredInnerImports
        ]
  return ([], Just $ RefineImportsResult res)

  where
    -- Check if a name is exposed by AvailInfo (the available information of a module)
    containsAvail :: LIE GhcRn -> AvailInfo -> Bool
    containsAvail name avail =
      any (\an -> prettyPrint an == (prettyPrint . ieName . unLoc $ name))
        $ availNamesWithSelectors avail

--------------------------------------------------------------------------------

data RemoveUnusedImports = RemoveUnusedImports
  deriving (Show, Generic, Eq, Ord)

instance Hashable RemoveUnusedImports
instance NFData RemoveUnusedImports
instance Binary RemoveUnusedImports
type instance RuleResult RemoveUnusedImports = RemoveUnusedImportsResult

newtype RemoveUnusedImportsResult = RemoveUnusedImportsResult
  {getRemoveUnusedImportsResult :: [LImportDecl GhcRn]}

instance Show RemoveUnusedImportsResult where show _ = "<removeUnusedImportsResult>"
instance NFData RemoveUnusedImportsResult where rnf = rwhnf

removeUnusedImportsRule :: Rules ()
removeUnusedImportsRule = define $ \RemoveUnusedImports nfp -> do
  (minImports, imports) <- extractMinimalImports nfp
  let minImportedModules = S.fromList
        [ mn
        | L _ ImportDecl{ideclName = L _ mn, ideclHiding} <- minImports
        , Just (False, L _ names) <- [ideclHiding]
        , notNull names
        ]

  let importOnlyForInstances Nothing               = False -- implicit -> import all
      -- import SomeModule () -- only for instances
      importOnlyForInstances (Just (_, L _ names)) = null names
  let importsToRemove =
        [ i
        -- for every current imports
        | i@(L _ ImportDecl{ideclName = L _ mn, ideclHiding}) <- imports
        -- if it's import only for instances we shouldn't touch it
        , not $ importOnlyForInstances ideclHiding
        -- check if it's in min imports
        , not $ mn `S.member` minImportedModules
        ]
  return ([], Just $ RemoveUnusedImportsResult importsToRemove)

--------------------------------------------------------------------------------

extractMinimalImports
  :: NormalizedFilePath
  -> Action ([LImportDecl GhcRn], [LImportDecl GhcRn])
extractMinimalImports nfp = do
  -- Get the typechecking artifacts from the module
  tmr <- use TypeCheck nfp
  -- We also need a GHC session with all the dependencies
  hsc <- use GhcSessionDeps nfp

  -- Use the GHC api to extract the "minimal" imports
  -- We shouldn't blindly refine imports
  -- instead we should generate imports statements
  -- for modules/symbols actually got used
  (imports, mbMinImports) <- liftIO $ EI.extractMinimalImports hsc tmr

  return (fromMaybe [] mbMinImports, imports)

mkExplicitEdit' :: PositionMapping -> LImportDecl pass -> T.Text -> Maybe TextEdit
mkExplicitEdit' pm imp txt = fst <$> mkExplicitEdit Nothing pm imp txt

mkExplicitEdit
  :: Maybe T.Text
  -> PositionMapping
  -> LImportDecl pass
  -> T.Text
  -> Maybe (TextEdit, Range) -- Range is the origin range of import
mkExplicitEdit mbContent posMapping (L src imp) explicit
  | RealSrcSpan l <- src,
    L _ mn <- ideclName imp,
    -- (almost) no one wants to see an refine import list for Prelude
    mn /= moduleName pRELUDE,
    Just rng <- toCurrentRange posMapping $ realSrcSpanToRange l =
      let rng' = case mbContent of
            Nothing -> rng
            Just content ->
              extendToIncludePreviousNewlineIfPossible
                (indexedByPosition $ T.unpack content)
                rng
      in Just (TextEdit rng' explicit, rng)
  | otherwise =
    Nothing

generateLens'
  :: PluginId
  -> Uri
  -> TextEdit
  -> IO (Maybe CodeLens)
generateLens' pId uri edits@TextEdit {_range} =
  generateLens pId uri edits _range

-- | Given an import declaration, generate a code lens unless it has an
-- explicit import list or it's qualified
generateLens
  :: PluginId
  -> Uri
  -> TextEdit
  -> Range
  -> IO (Maybe CodeLens)
generateLens
  pId
  uri
  edits@TextEdit {_range = rng, _newText}
  lensRange = do
  -- The title of the command is just the minimal explicit import decl
  let title =
        if T.null _newText
          then "Remove unused import"
          else "Refine imports to " <> T.intercalate ", " (T.lines _newText)
      -- the code lens has no extra data
      _xdata = Nothing
      -- an edit that replaces the whole declaration with the explicit one
      edit = WorkspaceEdit (Just editsMap) Nothing Nothing
      editsMap = HashMap.fromList [(uri, List [edits])]
      -- the command argument is simply the edit
      _arguments = Just [toJSON $ RefineImportCommandParams edit]
      -- create the command
      _command = Just $ mkLspCommand pId refineImportCommandId title _arguments
      _range = lensRange
  -- create and return the code lens
  return $ Just CodeLens {..}

--------------------------------------------------------------------------------

-- | A helper to run ide actions
runIde :: IdeState -> Action a -> IO a
runIde = runAction "RefineImports"

getFileContents
  :: IdeState
  -> NormalizedFilePath
  -> IO (Maybe T.Text)
getFileContents state nfp =
  (snd . fst =<<)
    <$> runIde state (useWithStale GetFileContents nfp)
