{-# LANGUAGE CPP        #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE MultiWayIf #-}


-- Mostly taken from "haskell-ide-engine"
module Development.IDE.Plugin.Completions.Logic (
  CachedCompletions
, cacheDataProducer
, localCompletionsForParsedModule
, getCompletions
) where

import           Control.Applicative
import           Data.Char                                (isUpper)
import           Data.Generics
import           Data.List.Extra                          as List hiding
                                                                  (stripPrefix)
import qualified Data.Map                                 as Map

import           Data.Maybe                               (fromMaybe, isJust,
                                                           listToMaybe,
                                                           mapMaybe)
import qualified Data.Text                                as T
import qualified Text.Fuzzy                               as Fuzzy

import           HscTypes
import           Name
import           RdrName
import           Type
#if MIN_VERSION_ghc(8,10,0)
import           Coercion
import           Pair
import           Predicate                                (isDictTy)
#endif

import           ConLike
import           Control.Monad
import           Data.Aeson                               (ToJSON (toJSON))
import           Data.Either                              (fromRight)
import           Data.Functor
import qualified Data.Set                                 as Set
import           Development.IDE.Core.Compile
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat               as GHC
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.Completions.Types
import           Development.IDE.Spans.Common
import           Development.IDE.Spans.Documentation
import           Development.IDE.Spans.LocalBindings
import           Development.IDE.Types.HscEnvEq
import           Development.IDE.Types.Options
import           GhcPlugins                               (flLabel, unpackFS)
import           Ide.PluginUtils                          (mkLspCommand)
import           Ide.Types                                (CommandId (..),
                                                           PluginId)
import           Language.LSP.Types
import           Language.LSP.Types.Capabilities
import qualified Language.LSP.VFS                         as VFS
import           Outputable                               (Outputable)
import           TyCoRep

-- From haskell-ide-engine/hie-plugin-api/Haskell/Ide/Engine/Context.hs

-- | A context of a declaration in the program
-- e.g. is the declaration a type declaration or a value declaration
-- Used for determining which code completions to show
-- TODO: expand this with more contexts like classes or instances for
-- smarter code completion
data Context = TypeContext
             | ValueContext
             | ModuleContext String -- ^ module context with module name
             | ImportContext String -- ^ import context with module name
             | ImportListContext String -- ^ import list context with module name
             | ImportHidingContext String -- ^ import hiding context with module name
             | ExportContext -- ^ List of exported identifiers from the current module
  deriving (Show, Eq)

-- | Generates a map of where the context is a type and where the context is a value
-- i.e. where are the value decls and the type decls
getCContext :: Position -> ParsedModule -> Maybe Context
getCContext pos pm
  | Just (L r modName) <- moduleHeader
  , pos `isInsideSrcSpan` r
  = Just (ModuleContext (moduleNameString modName))

  | Just (L r _) <- exportList
  , pos `isInsideSrcSpan` r
  = Just ExportContext

  | Just ctx <- something (Nothing `mkQ` go `extQ` goInline) decl
  = Just ctx

  | Just ctx <- something (Nothing `mkQ` importGo) imports
  = Just ctx

  | otherwise
  = Nothing

  where decl = hsmodDecls $ unLoc $ pm_parsed_source pm
        moduleHeader = hsmodName $ unLoc $ pm_parsed_source pm
        exportList = hsmodExports $ unLoc $ pm_parsed_source pm
        imports = hsmodImports $ unLoc $ pm_parsed_source pm

        go :: LHsDecl GhcPs -> Maybe Context
        go (L r SigD {})
          | pos `isInsideSrcSpan` r = Just TypeContext
          | otherwise = Nothing
        go (L r GHC.ValD {})
          | pos `isInsideSrcSpan` r = Just ValueContext
          | otherwise = Nothing
        go _ = Nothing

        goInline :: GHC.LHsType GhcPs -> Maybe Context
        goInline (GHC.L r _)
          | pos `isInsideSrcSpan` r = Just TypeContext
        goInline _ = Nothing

        importGo :: GHC.LImportDecl GhcPs -> Maybe Context
        importGo (L r impDecl)
          | pos `isInsideSrcSpan` r
          = importInline importModuleName (ideclHiding impDecl)
          <|> Just (ImportContext importModuleName)

          | otherwise = Nothing
          where importModuleName = moduleNameString $ unLoc $ ideclName impDecl

        importInline :: String -> Maybe (Bool,  GHC.Located [LIE GhcPs]) -> Maybe Context
        importInline modName (Just (True, L r _))
          | pos `isInsideSrcSpan` r = Just $ ImportHidingContext modName
          | otherwise = Nothing
        importInline modName (Just (False, L r _))
          | pos `isInsideSrcSpan` r = Just $ ImportListContext modName
          | otherwise = Nothing
        importInline _ _ = Nothing

occNameToComKind :: Maybe T.Text -> OccName -> CompletionItemKind
occNameToComKind ty oc
  | isVarOcc  oc = case occNameString oc of
                     i:_ | isUpper i -> CiConstructor
                     _               -> CiFunction
  | isTcOcc   oc = case ty of
                     Just t
                       | "Constraint" `T.isSuffixOf` t
                       -> CiInterface
                     _ -> CiStruct
  | isDataOcc oc = CiConstructor
  | otherwise    = CiVariable


showModName :: ModuleName -> T.Text
showModName = T.pack . moduleNameString

-- mkCompl :: IdeOptions -> CompItem -> CompletionItem
-- mkCompl IdeOptions{..} CI{compKind,insertText, importedFrom,typeText,label,docs} =
--   CompletionItem label kind (List []) ((colon <>) <$> typeText)
--     (Just $ CompletionDocMarkup $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator docs')
--     Nothing Nothing Nothing Nothing (Just insertText) (Just Snippet)
--     Nothing Nothing Nothing Nothing Nothing

mkCompl :: PluginId -> IdeOptions -> CompItem -> IO CompletionItem
mkCompl
  pId
  IdeOptions {..}
  CI
    { compKind,
      isInfix,
      insertText,
      importedFrom,
      typeText,
      label,
      docs,
      additionalTextEdits
    } = do
  mbCommand <- mkAdditionalEditsCommand pId `traverse` additionalTextEdits
  let ci = CompletionItem
                 {_label = label,
                  _kind = kind,
                  _tags = Nothing,
                  _detail = (colon <>) <$> typeText,
                  _documentation = documentation,
                  _deprecated = Nothing,
                  _preselect = Nothing,
                  _sortText = Nothing,
                  _filterText = Nothing,
                  _insertText = Just insertText,
                  _insertTextFormat = Just Snippet,
                  _insertTextMode = Nothing,
                  _textEdit = Nothing,
                  _additionalTextEdits = Nothing,
                  _commitCharacters = Nothing,
                  _command = mbCommand,
                  _xdata = Nothing}
  return $ removeSnippetsWhen (isJust isInfix) ci

  where kind = Just compKind
        docs' = imported : spanDocToMarkdown docs
        imported = case importedFrom of
          Left pos  -> "*Defined at '" <> ppr pos <> "'*\n'"
          Right mod -> "*Defined in '" <> mod <> "'*\n"
        colon = if optNewColonConvention then ": " else ":: "
        documentation = Just $ CompletionDocMarkup $
                        MarkupContent MkMarkdown $
                        T.intercalate sectionSeparator docs'

mkAdditionalEditsCommand :: PluginId -> ExtendImport -> IO Command
mkAdditionalEditsCommand pId edits = pure $
  mkLspCommand pId (CommandId extendImportCommandId) "extend import" (Just [toJSON edits])

mkNameCompItem :: Uri -> Maybe T.Text -> OccName -> ModuleName -> Maybe Type -> Maybe Backtick -> SpanDoc -> Maybe (LImportDecl GhcPs) -> CompItem
mkNameCompItem doc thingParent origName origMod thingType isInfix docs !imp = CI {..}
  where
    compKind = occNameToComKind typeText origName
    importedFrom = Right $ showModName origMod
    isTypeCompl = isTcOcc origName
    label = stripPrefix $ showGhc origName
    insertText = case isInfix of
            Nothing -> case getArgText <$> thingType of
                            Nothing      -> label
                            Just argText -> label <> " " <> argText
            Just LeftSide -> label <> "`"

            Just Surrounded -> label
    typeText
          | Just t <- thingType = Just . stripForall $ showGhc t
          | otherwise = Nothing
    additionalTextEdits =
      imp <&> \x ->
        ExtendImport
          { doc,
            thingParent,
            importName = showModName $ unLoc $ ideclName $ unLoc x,
            importQual = getImportQual x,
            newThing = showNameWithoutUniques origName
          }

    stripForall :: T.Text -> T.Text
    stripForall t
      | T.isPrefixOf "forall" t =
        -- We drop 2 to remove the '.' and the space after it
        T.drop 2 (T.dropWhile (/= '.') t)
      | otherwise               = t

    getArgText :: Type -> T.Text
    getArgText typ = argText
      where
        argTypes = getArgs typ
        argText :: T.Text
        argText = mconcat $ List.intersperse " " $ zipWithFrom snippet 1 argTypes
        snippet :: Int -> Type -> T.Text
        snippet i t = case t of
            (TyVarTy _)     -> noParensSnippet
            (LitTy _)       -> noParensSnippet
            (TyConApp _ []) -> noParensSnippet
            _               -> snippetText i ("(" <> showGhc t <> ")")
            where
                noParensSnippet = snippetText i (showGhc t)
                snippetText i t = "${" <> T.pack (show i) <> ":" <> t <> "}"
        getArgs :: Type -> [Type]
        getArgs t
          | isPredTy t = []
          | isDictTy t = []
          | isForAllTy t = getArgs $ snd (splitForAllTys t)
          | isFunTy t =
            let (args, ret) = splitFunTys t
              in if isForAllTy ret
                  then getArgs ret
                  else Prelude.filter (not . isDictTy) $ map scaledThing args
          | isPiTy t = getArgs $ snd (splitPiTys t)
#if MIN_VERSION_ghc(8,10,0)
          | Just (Pair _ t) <- coercionKind <$> isCoercionTy_maybe t
          = getArgs t
#else
          | isCoercionTy t = maybe [] (getArgs . snd) (splitCoercionType_maybe t)
#endif
          | otherwise = []

mkModCompl :: T.Text -> CompletionItem
mkModCompl label =
  CompletionItem label (Just CiModule) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing

mkImportCompl :: T.Text -> T.Text -> CompletionItem
mkImportCompl enteredQual label =
  CompletionItem m (Just CiModule) Nothing (Just label)
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing
  where
    m = fromMaybe "" (T.stripPrefix enteredQual label)

mkExtCompl :: T.Text -> CompletionItem
mkExtCompl label =
  CompletionItem label (Just CiKeyword) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing

mkPragmaCompl :: T.Text -> T.Text -> CompletionItem
mkPragmaCompl label insertText =
  CompletionItem label (Just CiKeyword) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing (Just insertText) (Just Snippet)
    Nothing Nothing Nothing Nothing Nothing Nothing


cacheDataProducer :: Uri -> HscEnvEq -> Module -> GlobalRdrEnv-> GlobalRdrEnv -> [LImportDecl GhcPs] -> IO CachedCompletions
cacheDataProducer uri env curMod globalEnv inScopeEnv limports = do
  let
      packageState = hscEnv env
      curModName = moduleName curMod

      importMap = Map.fromList [ (l, imp) | imp@(L (OldRealSrcSpan l) _) <- limports ]

      iDeclToModName :: ImportDecl name -> ModuleName
      iDeclToModName = unLoc . ideclName

      asNamespace :: ImportDecl name -> ModuleName
      asNamespace imp = maybe (iDeclToModName imp) GHC.unLoc (ideclAs imp)
      -- Full canonical names of imported modules
      importDeclerations = map unLoc limports


      -- The given namespaces for the imported modules (ie. full name, or alias if used)
      allModNamesAsNS = map (showModName . asNamespace) importDeclerations

      rdrElts = globalRdrEnvElts globalEnv

      foldMapM :: (Foldable f, Monad m, Monoid b) => (a -> m b) -> f a -> m b
      foldMapM f xs = foldr step return xs mempty where
        step x r z = f x >>= \y -> r $! z `mappend` y

      getCompls :: [GlobalRdrElt] -> IO ([CompItem],QualCompls)
      getCompls = foldMapM getComplsForOne

      getComplsForOne :: GlobalRdrElt -> IO ([CompItem],QualCompls)
      getComplsForOne (GRE n par True _) =
          (, mempty) <$> toCompItem par curMod curModName n Nothing
      getComplsForOne (GRE n par False prov) =
        flip foldMapM (map is_decl prov) $ \spec -> do
          let originalImportDecl = do
                -- we don't want to extend import if it's already in scope
                guard . null $ lookupGRE_Name inScopeEnv n
                -- or if it doesn't have a real location
                loc <- realSpan $ is_dloc spec
                Map.lookup loc importMap
          compItem <- toCompItem par curMod (is_mod spec) n originalImportDecl
          let unqual
                | is_qual spec = []
                | otherwise = compItem
              qual
                | is_qual spec = Map.singleton asMod compItem
                | otherwise = Map.fromList [(asMod,compItem),(origMod,compItem)]
              asMod = showModName (is_as spec)
              origMod = showModName (is_mod spec)
          return (unqual,QualCompls qual)

      toCompItem :: Parent -> Module -> ModuleName -> Name -> Maybe (LImportDecl GhcPs) -> IO [CompItem]
      toCompItem par m mn n imp' = do
        docs <- getDocumentationTryGhc packageState curMod n
        let (mbParent, originName) = case par of
                            NoParent -> (Nothing, nameOccName n)
                            ParentIs n' -> (Just . T.pack $ printName n', nameOccName n)
                            FldParent n' lbl -> (Just . T.pack $ printName n', maybe (nameOccName n) mkVarOccFS lbl)
        tys <- catchSrcErrors (hsc_dflags packageState) "completion" $ do
                name' <- lookupName packageState m n
                return ( name' >>= safeTyThingType
                       , guard (isJust mbParent) >> name' >>= safeTyThingForRecord
                       )
        let (ty, record_ty) = fromRight (Nothing, Nothing) tys

        let recordCompls = case record_ty of
                Just (ctxStr, flds) | not (null flds) ->
                    [mkRecordSnippetCompItem uri mbParent ctxStr flds (ppr mn) docs imp']
                _ -> []

        return $ mkNameCompItem uri mbParent originName mn ty Nothing docs imp'
               : recordCompls

  (unquals,quals) <- getCompls rdrElts

  -- The list of all importable Modules from all packages
  moduleNames <- maybe [] (map showModName) <$> envVisibleModuleNames env

  return $ CC
    { allModNamesAsNS = allModNamesAsNS
    , unqualCompls = unquals
    , qualCompls = quals
    , importableModules = moduleNames
    }

-- | Produces completions from the top level declarations of a module.
localCompletionsForParsedModule :: Uri -> ParsedModule -> CachedCompletions
localCompletionsForParsedModule uri pm@ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls, hsmodName}} =
    CC { allModNamesAsNS = mempty
       , unqualCompls = compls
       , qualCompls = mempty
       , importableModules = mempty
        }
  where
    typeSigIds = Set.fromList
        [ id
            | L _ (SigD _ (TypeSig _ ids _)) <- hsmodDecls
            , L _ id <- ids
            ]
    hasTypeSig = (`Set.member` typeSigIds) . unLoc

    compls = concat
        [ case decl of
            SigD _ (TypeSig _ ids typ) ->
                [mkComp id CiFunction (Just $ ppr typ) | id <- ids]
            ValD _ FunBind{fun_id} ->
                [ mkComp fun_id CiFunction Nothing
                | not (hasTypeSig fun_id)
                ]
            ValD _ PatBind{pat_lhs} ->
                [mkComp id CiVariable Nothing
                | VarPat _ id <- listify (\(_ :: Pat GhcPs) -> True) pat_lhs]
            TyClD _ ClassDecl{tcdLName, tcdSigs} ->
                mkComp tcdLName CiInterface Nothing :
                [ mkComp id CiFunction (Just $ ppr typ)
                | L _ (ClassOpSig _ _ ids typ) <- tcdSigs
                , id <- ids]
            TyClD _ x ->
                let generalCompls = [mkComp id cl Nothing
                        | id <- listify (\(_ :: Located(IdP GhcPs)) -> True) x
                        , let cl = occNameToComKind Nothing (rdrNameOcc $ unLoc id)]
                    -- here we only have to look at the outermost type
                    recordCompls = findRecordCompl uri pm thisModName x
                in
                   -- the constructors and snippets will be duplicated here giving the user 2 choices.
                   generalCompls ++ recordCompls
            ForD _ ForeignImport{fd_name,fd_sig_ty} ->
                [mkComp fd_name CiVariable (Just $ ppr fd_sig_ty)]
            ForD _ ForeignExport{fd_name,fd_sig_ty} ->
                [mkComp fd_name CiVariable (Just $ ppr fd_sig_ty)]
            _ -> []
            | L _ decl <- hsmodDecls
        ]

    mkComp n ctyp ty =
        CI ctyp pn (Right thisModName) ty pn Nothing doc (ctyp `elem` [CiStruct, CiInterface]) Nothing
      where
        pn = ppr n
        doc = SpanDocText (getDocumentation [pm] n) (SpanDocUris Nothing Nothing)

    thisModName = ppr hsmodName

findRecordCompl :: Uri -> ParsedModule -> T.Text -> TyClDecl GhcPs -> [CompItem]
findRecordCompl uri pmod mn DataDecl {tcdLName, tcdDataDefn} = result
    where
        result = [mkRecordSnippetCompItem uri (Just $ showNameWithoutUniques $ unLoc tcdLName)
                        (showGhc . unLoc $ con_name) field_labels mn doc Nothing
                 | ConDeclH98{..} <- unLoc <$> dd_cons tcdDataDefn
                 , Just  con_details <- [getFlds con_args]
                 , let field_names = mapMaybe extract con_details
                 , let field_labels = showGhc . unLoc <$> field_names
                 , (not . List.null) field_labels
                 ]
        doc = SpanDocText (getDocumentation [pmod] tcdLName) (SpanDocUris Nothing Nothing)

        getFlds :: HsConDetails arg (Located [LConDeclField GhcPs]) -> Maybe [ConDeclField GhcPs]
        getFlds conArg = case conArg of
                             RecCon rec  -> Just $ unLoc <$> unLoc rec
                             PrefixCon _ -> Just []
                             _           -> Nothing

        extract ConDeclField{..}
             -- TODO: Why is cd_fld_names a list?
            | Just fld_name <- rdrNameFieldOcc . unLoc <$> listToMaybe cd_fld_names = Just fld_name
            | otherwise = Nothing
        -- XConDeclField
        extract _ = Nothing
findRecordCompl _ _ _ _ = []

ppr :: Outputable a => a -> T.Text
ppr = T.pack . prettyPrint

toggleSnippets :: ClientCapabilities -> CompletionsConfig -> CompletionItem -> CompletionItem
toggleSnippets ClientCapabilities {_textDocument} (CompletionsConfig with _) =
  removeSnippetsWhen (not $ with && supported)
  where
    supported =
      Just True == (_textDocument >>= _completion >>= _completionItem >>= _snippetSupport)

toggleAutoExtend :: CompletionsConfig -> CompItem -> CompItem
toggleAutoExtend (CompletionsConfig _ False) x = x {additionalTextEdits = Nothing}
toggleAutoExtend _ x = x

removeSnippetsWhen :: Bool -> CompletionItem -> CompletionItem
removeSnippetsWhen condition x =
  if condition
    then
      x
        { _insertTextFormat = Just PlainText,
          _insertText = Nothing
        }
    else x

-- | Returns the cached completions for the given module and position.
getCompletions
    :: PluginId
    -> IdeOptions
    -> CachedCompletions
    -> Maybe (ParsedModule, PositionMapping)
    -> (Bindings, PositionMapping)
    -> VFS.PosPrefixInfo
    -> ClientCapabilities
    -> CompletionsConfig
    -> IO [CompletionItem]
getCompletions plId ideOpts CC {allModNamesAsNS, unqualCompls, qualCompls, importableModules}
               maybe_parsed (localBindings, bmapping) prefixInfo caps config = do
  let VFS.PosPrefixInfo { fullLine, prefixModule, prefixText } = prefixInfo
      enteredQual = if T.null prefixModule then "" else prefixModule <> "."
      fullPrefix  = enteredQual <> prefixText

      {- correct the position by moving 'foo :: Int -> String ->    '
                                                                    ^
          to                             'foo :: Int -> String ->    '
                                                              ^
      -}
      pos = VFS.cursorPos prefixInfo

      filtModNameCompls =
        map mkModCompl
          $ mapMaybe (T.stripPrefix enteredQual)
          $ Fuzzy.simpleFilter fullPrefix allModNamesAsNS

      filtCompls = map Fuzzy.original $ Fuzzy.filter prefixText ctxCompls "" "" label False
        where

          mcc = case maybe_parsed of
            Nothing -> Nothing
            Just (pm, pmapping) ->
              let PositionMapping pDelta = pmapping
                  position' = fromDelta pDelta pos
                  lpos = lowerRange position'
                  hpos = upperRange position'
              in getCContext lpos pm <|> getCContext hpos pm

          -- completions specific to the current context
          ctxCompls' = case mcc of
                        Nothing           -> compls
                        Just TypeContext  -> filter isTypeCompl compls
                        Just ValueContext -> filter (not . isTypeCompl) compls
                        Just _            -> filter (not . isTypeCompl) compls
          -- Add whether the text to insert has backticks
          ctxCompls = map (\comp -> toggleAutoExtend config $ comp { isInfix = infixCompls }) ctxCompls'

          infixCompls :: Maybe Backtick
          infixCompls = isUsedAsInfix fullLine prefixModule prefixText pos

          PositionMapping bDelta = bmapping
          oldPos = fromDelta bDelta $ VFS.cursorPos prefixInfo
          startLoc = lowerRange oldPos
          endLoc = upperRange oldPos
          localCompls = map (uncurry localBindsToCompItem) $ getFuzzyScope localBindings startLoc endLoc
          localBindsToCompItem :: Name -> Maybe Type -> CompItem
          localBindsToCompItem name typ = CI ctyp pn thisModName ty pn Nothing emptySpanDoc (not $ isValOcc occ) Nothing
            where
              occ = nameOccName name
              ctyp = occNameToComKind Nothing occ
              pn = ppr name
              ty = ppr <$> typ
              thisModName = case nameModule_maybe name of
                Nothing -> Left $ nameSrcSpan name
                Just m  -> Right $ ppr m

          compls = if T.null prefixModule
            then localCompls ++ unqualCompls
            else Map.findWithDefault [] prefixModule $ getQualCompls qualCompls

      filtListWith f list =
        [ f label
        | label <- Fuzzy.simpleFilter fullPrefix list
        , enteredQual `T.isPrefixOf` label
        ]

      filtListWithSnippet f list suffix =
        [ toggleSnippets caps config (f label (snippet <> suffix))
        | (snippet, label) <- list
        , Fuzzy.test (T.toUpper fullPrefix) label
        ]

      filtImportCompls = filtListWith (mkImportCompl enteredQual) importableModules
      filtPragmaCompls = filtListWithSnippet mkPragmaCompl validPragmas
      filtOptsCompls   = filtListWith mkExtCompl
      filtKeywordCompls
          | T.null prefixModule = filtListWith mkExtCompl (optKeywords ideOpts)
          | otherwise = []

      stripLeading :: Char -> String -> String
      stripLeading _ [] = []
      stripLeading c (s:ss)
        | s == c = ss
        | otherwise = s:ss

  if
    | "import " `T.isPrefixOf` fullLine
    -> return filtImportCompls
    -- we leave this condition here to avoid duplications and return empty list
    -- since HLS implements this completion (#haskell-language-server/pull/662)
    | "{-# language" `T.isPrefixOf` T.toLower fullLine
    -> return []
    | "{-# options_ghc" `T.isPrefixOf` T.toLower fullLine
    -> return $ filtOptsCompls (map (T.pack . stripLeading '-') $ flagsForCompletion False)
    | "{-# " `T.isPrefixOf` fullLine
    -> return $ filtPragmaCompls (pragmaSuffix fullLine)
    | otherwise -> do
        let uniqueFiltCompls = nubOrdOn insertText filtCompls
        compls <- mapM (mkCompl plId ideOpts) uniqueFiltCompls
        return $ filtModNameCompls
              ++ filtKeywordCompls
              ++ map (toggleSnippets caps config) compls


-- ---------------------------------------------------------------------
-- helper functions for pragmas
-- ---------------------------------------------------------------------

validPragmas :: [(T.Text, T.Text)]
validPragmas =
  [ ("LANGUAGE ${1:extension}"        , "LANGUAGE")
  , ("OPTIONS_GHC -${1:option}"       , "OPTIONS_GHC")
  , ("INLINE ${1:function}"           , "INLINE")
  , ("NOINLINE ${1:function}"         , "NOINLINE")
  , ("INLINABLE ${1:function}"        , "INLINABLE")
  , ("WARNING ${1:message}"           , "WARNING")
  , ("DEPRECATED ${1:message}"        , "DEPRECATED")
  , ("ANN ${1:annotation}"            , "ANN")
  , ("RULES"                          , "RULES")
  , ("SPECIALIZE ${1:function}"       , "SPECIALIZE")
  , ("SPECIALIZE INLINE ${1:function}", "SPECIALIZE INLINE")
  ]

pragmaSuffix :: T.Text -> T.Text
pragmaSuffix fullLine
  |  "}" `T.isSuffixOf` fullLine = mempty
  | otherwise = " #-}"

-- ---------------------------------------------------------------------
-- helper functions for infix backticks
-- ---------------------------------------------------------------------

hasTrailingBacktick :: T.Text -> Position -> Bool
hasTrailingBacktick line Position { _character }
    | T.length line > _character = (line `T.index` _character) == '`'
    | otherwise = False

isUsedAsInfix :: T.Text -> T.Text -> T.Text -> Position -> Maybe Backtick
isUsedAsInfix line prefixMod prefixText pos
    | hasClosingBacktick && hasOpeningBacktick = Just Surrounded
    | hasOpeningBacktick = Just LeftSide
    | otherwise = Nothing
  where
    hasOpeningBacktick = openingBacktick line prefixMod prefixText pos
    hasClosingBacktick = hasTrailingBacktick line pos

openingBacktick :: T.Text -> T.Text -> T.Text -> Position -> Bool
openingBacktick line prefixModule prefixText Position { _character }
  | backtickIndex < 0 || backtickIndex > T.length line = False
  | otherwise = (line `T.index` backtickIndex) == '`'
    where
    backtickIndex :: Int
    backtickIndex =
      let
          prefixLength = T.length prefixText
          moduleLength = if prefixModule == ""
                    then 0
                    else T.length prefixModule + 1 {- Because of "." -}
      in
        -- Points to the first letter of either the module or prefix text
        _character - (prefixLength + moduleLength) - 1


-- ---------------------------------------------------------------------

-- | Under certain circumstance GHC generates some extra stuff that we
-- don't want in the autocompleted symbols
    {- When e.g. DuplicateRecordFields is enabled, compiler generates
    names like "$sel:accessor:One" and "$sel:accessor:Two" to disambiguate record selectors
    https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/DuplicateRecordFields#Implementation
    -}
-- TODO: Turn this into an alex lexer that discards prefixes as if they were whitespace.
stripPrefix :: T.Text -> T.Text
stripPrefix name = T.takeWhile (/=':') $ go prefixes
  where
    go [] = name
    go (p:ps)
      | T.isPrefixOf p name = T.drop (T.length p) name
      | otherwise = go ps

-- | Prefixes that can occur in a GHC OccName
prefixes :: [T.Text]
prefixes =
  [
    -- long ones
    "$con2tag_"
  , "$tag2con_"
  , "$maxtag_"

  -- four chars
  , "$sel:"
  , "$tc'"

  -- three chars
  , "$dm"
  , "$co"
  , "$tc"
  , "$cp"
  , "$fx"

  -- two chars
  , "$W"
  , "$w"
  , "$m"
  , "$b"
  , "$c"
  , "$d"
  , "$i"
  , "$s"
  , "$f"
  , "$r"
  , "C:"
  , "N:"
  , "D:"
  , "$p"
  , "$L"
  , "$f"
  , "$t"
  , "$c"
  , "$m"
  ]


safeTyThingForRecord :: TyThing -> Maybe (T.Text, [T.Text])
safeTyThingForRecord (AnId _) = Nothing
safeTyThingForRecord (AConLike dc) =
    let ctxStr =   showGhc . occName . conLikeName $ dc
        field_names = T.pack . unpackFS . flLabel <$> conLikeFieldLabels dc
    in
        Just (ctxStr, field_names)
safeTyThingForRecord _ = Nothing

mkRecordSnippetCompItem :: Uri -> Maybe T.Text -> T.Text -> [T.Text] -> T.Text -> SpanDoc -> Maybe (LImportDecl GhcPs) -> CompItem
mkRecordSnippetCompItem uri parent ctxStr compl mn docs imp = r
  where
      r  = CI {
            compKind = CiSnippet
          , insertText = buildSnippet
          , importedFrom = importedFrom
          , typeText = Nothing
          , label = ctxStr
          , isInfix = Nothing
          , docs = docs
          , isTypeCompl = False
          , additionalTextEdits = imp <&> \x ->
            ExtendImport
                { doc = uri,
                  thingParent = parent,
                  importName = showModName $ unLoc $ ideclName $ unLoc x,
                  importQual = getImportQual x,
                  newThing = ctxStr
                }
          }

      placeholder_pairs = zip compl ([1..]::[Int])
      snippet_parts = map (\(x, i) -> x <> "=${" <> T.pack (show i) <> ":_" <> x <> "}") placeholder_pairs
      snippet = T.intercalate (T.pack ", ") snippet_parts
      buildSnippet = ctxStr <> " {" <> snippet <> "}"
      importedFrom = Right mn

getImportQual :: LImportDecl GhcPs -> Maybe T.Text
getImportQual (L _ imp)
    | isQualifiedImport imp = Just $ T.pack $ moduleNameString $ maybe (unLoc $ ideclName imp) unLoc (ideclAs imp)
    | otherwise = Nothing
