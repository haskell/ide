{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}

-- | An HLS plugin to provide code lenses for type signatures
module Development.IDE.Plugin.TypeLenses
  ( descriptor,
    suggestSignature,
    typeLensCommandId,
    GlobalBindingTypeSig (..),
    GetGlobalBindingTypeSigs (..),
    GlobalBindingTypeSigsResult (..),
  )
where

import           Avail                               (availsToNameSet)
import           Control.DeepSeq                     (rwhnf)
import           Control.Monad                       (mzero)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Aeson.Types                    (Value (..), toJSON)
import qualified Data.Aeson.Types                    as A
import qualified Data.HashMap.Strict                 as Map
import           Data.List                           (find)
import           Data.Maybe                          (catMaybes)
import qualified Data.Text                           as T
import           Development.IDE                     (RuleResult, Rules, define,
                                                      srcSpanToRange,
                                                      tmrModSummary)
import           Development.IDE.Core.Compile        (TcModuleResult (..))
import           Development.IDE.Core.RuleTypes      (GetBindings (GetBindings),
                                                      TypeCheck (TypeCheck))
import           Development.IDE.Core.Rules          (IdeState, runAction)
import           Development.IDE.Core.Service        (getDiagnostics)
import           Development.IDE.Core.Shake          (getHiddenDiagnostics, use)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util            (printName)
import           Development.IDE.Spans.Common        (safeTyThingType)
import           Development.IDE.Spans.LocalBindings (Bindings, getFuzzyScope)
import           Development.IDE.Types.Location      (Position (Position, _character, _line),
                                                      Range (Range, _end, _start),
                                                      toNormalizedFilePath',
                                                      uriToFilePath')
import           Development.IDE.Graph.Classes
import           GHC.Generics                        (Generic)
import           GhcPlugins                          (GlobalRdrEnv, SDoc,
                                                      elemNameSet, getSrcSpan,
                                                      idName, lookupTypeEnv)
import           HscTypes                            (mkPrintUnqualified)
import           Ide.Plugin.Config                   (Config)
import           Ide.Plugin.Properties
import           Ide.PluginUtils                     (mkLspCommand,
                                                      usePropertyLsp)
import           Ide.Types                           (CommandFunction,
                                                      CommandId (CommandId),
                                                      PluginCommand (PluginCommand),
                                                      PluginDescriptor (..),
                                                      PluginId,
                                                      defaultPluginDescriptor,
                                                      mkCustomConfig,
                                                      mkPluginHandler)
import qualified Language.LSP.Server                 as LSP
import           Language.LSP.Types                  (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                      CodeLens (CodeLens),
                                                      CodeLensParams (CodeLensParams, _textDocument),
                                                      Diagnostic (..),
                                                      List (..), ResponseError,
                                                      SMethod (..),
                                                      TextDocumentIdentifier (TextDocumentIdentifier),
                                                      TextEdit (TextEdit),
                                                      WorkspaceEdit (WorkspaceEdit))
import           Outputable                          (showSDocForUser)
import           PatSyn                              (patSynName)
import           TcRnTypes                           (TcGblEnv (..))
import           TcType                              (pprSigmaType)
import           Text.Regex.TDFA                     ((=~), (=~~))

typeLensCommandId :: T.Text
typeLensCommandId = "typesignature.add"

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeLens codeLensProvider,
      pluginCommands = [PluginCommand (CommandId typeLensCommandId) "adds a signature" commandHandler],
      pluginRules = rules,
      pluginCustomConfig = mkCustomConfig properties
    }

properties :: Properties '[ 'PropertyKey "mode" ('TEnum Mode)]
properties =
  emptyProperties
    & defineEnumProperty
      #mode
      "Control how type lenses are shown"
      [ (Always, "Always displays type lenses of global bindings"),
        (Exported, "Only display type lenses of exported global bindings"),
        (Diagnostics, "Follows error messages produced by GHC about missing signatures")
      ]
      Always

codeLensProvider ::
  IdeState ->
  PluginId ->
  CodeLensParams ->
  LSP.LspM Config (Either ResponseError (List CodeLens))
codeLensProvider ideState pId CodeLensParams {_textDocument = TextDocumentIdentifier uri} = do
  mode <- usePropertyLsp #mode pId properties
  fmap (Right . List) $ case uriToFilePath' uri of
    Just (toNormalizedFilePath' -> filePath) -> liftIO $ do
      tmr <- runAction "codeLens.TypeCheck" ideState (use TypeCheck filePath)
      bindings <- runAction "codeLens.GetBindings" ideState (use GetBindings filePath)
      gblSigs <- runAction "codeLens.GetGlobalBindingTypeSigs" ideState (use GetGlobalBindingTypeSigs filePath)

      diag <- getDiagnostics ideState
      hDiag <- getHiddenDiagnostics ideState

      let toWorkSpaceEdit tedit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing Nothing
          generateLensForGlobal sig@GlobalBindingTypeSig {..} = do
            range <- srcSpanToRange $ gbSrcSpan sig
            tedit <- gblBindingTypeSigToEdit sig
            let wedit = toWorkSpaceEdit [tedit]
            pure $ generateLens pId range (T.pack gbRendered) wedit
          gblSigs' = maybe [] (\(GlobalBindingTypeSigsResult x) -> x) gblSigs
          generateLensFromDiags f =
            sequence
              [ pure $ generateLens pId _range title edit
                | (dFile, _, dDiag@Diagnostic {_range = _range}) <- diag ++ hDiag,
                  dFile == filePath,
                  (title, tedit) <- f dDiag,
                  let edit = toWorkSpaceEdit tedit
              ]

      case mode of
        Always ->
          pure (catMaybes $ generateLensForGlobal <$> gblSigs')
            <> generateLensFromDiags (suggestLocalSignature False tmr bindings) -- we still need diagnostics for local bindings
        Exported -> pure $ catMaybes $ generateLensForGlobal <$> filter gbExported gblSigs'
        Diagnostics -> generateLensFromDiags $ suggestSignature False gblSigs tmr bindings
    Nothing -> pure []

generateLens :: PluginId -> Range -> T.Text -> WorkspaceEdit -> CodeLens
generateLens pId _range title edit =
  let cId = mkLspCommand pId (CommandId typeLensCommandId) title (Just [toJSON edit])
   in CodeLens _range (Just cId) Nothing

commandHandler :: CommandFunction IdeState WorkspaceEdit
commandHandler _ideState wedit = do
  _ <- LSP.sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right Null

--------------------------------------------------------------------------------

suggestSignature :: Bool -> Maybe GlobalBindingTypeSigsResult -> Maybe TcModuleResult -> Maybe Bindings -> Diagnostic -> [(T.Text, [TextEdit])]
suggestSignature isQuickFix mGblSigs mTmr mBindings diag =
  suggestGlobalSignature isQuickFix mGblSigs diag <> suggestLocalSignature isQuickFix mTmr mBindings diag

suggestGlobalSignature :: Bool -> Maybe GlobalBindingTypeSigsResult -> Diagnostic -> [(T.Text, [TextEdit])]
suggestGlobalSignature isQuickFix mGblSigs Diagnostic {_message, _range}
  | _message
      =~ ("(Top-level binding|Pattern synonym) with no type signature" :: T.Text),
    Just (GlobalBindingTypeSigsResult sigs) <- mGblSigs,
    Just sig <- find (\x -> sameThing (gbSrcSpan x) _range) sigs,
    signature <- T.pack $ gbRendered sig,
    title <- if isQuickFix then "add signature: " <> signature else signature,
    Just action <- gblBindingTypeSigToEdit sig =
    [(title, [action])]
  | otherwise = []

suggestLocalSignature :: Bool -> Maybe TcModuleResult -> Maybe Bindings -> Diagnostic -> [(T.Text, [TextEdit])]
suggestLocalSignature isQuickFix mTmr mBindings Diagnostic {_message, _range = _range@Range {..}}
  | Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, [identifier]) <-
      (T.unwords . T.words $ _message)
        =~~ ("Polymorphic local binding with no type signature: (.*) ::" :: T.Text),
    Just bindings <- mBindings,
    localScope <- getFuzzyScope bindings _start _end,
    -- we can't use srcspan to lookup scoped bindings, because the error message reported by GHC includes the entire binding, instead of simply the name
    Just (name, ty) <- find (\(x, _) -> printName x == T.unpack identifier) localScope >>= \(name, mTy) -> (name,) <$> mTy,
    Just TcModuleResult {tmrTypechecked = TcGblEnv {tcg_rdr_env, tcg_sigs}} <- mTmr,
    -- not a top-level thing, to avoid duplication
    not $ name `elemNameSet` tcg_sigs,
    tyMsg <- showSDocForUser unsafeGlobalDynFlags (mkPrintUnqualified unsafeGlobalDynFlags tcg_rdr_env) $ pprSigmaType ty,
    signature <- T.pack $ printName name <> " :: " <> tyMsg,
    startCharacter <- _character _start,
    startOfLine <- Position (_line _start) startCharacter,
    beforeLine <- Range startOfLine startOfLine,
    title <- if isQuickFix then "add signature: " <> signature else signature,
    action <- TextEdit beforeLine $ signature <> "\n" <> T.replicate startCharacter " " =
    [(title, [action])]
  | otherwise = []

sameThing :: SrcSpan -> Range -> Bool
sameThing s1 s2 = (_start <$> srcSpanToRange s1) == (_start <$> Just s2)

gblBindingTypeSigToEdit :: GlobalBindingTypeSig -> Maybe TextEdit
gblBindingTypeSigToEdit GlobalBindingTypeSig {..}
  | Just Range {..} <- srcSpanToRange $ getSrcSpan gbName,
    startOfLine <- Position (_line _start) 0,
    beforeLine <- Range startOfLine startOfLine =
    Just $ TextEdit beforeLine $ T.pack gbRendered <> "\n"
  | otherwise = Nothing

data Mode
  = -- | always displays type lenses of global bindings, no matter what GHC flags are set
    Always
  | -- | similar to 'Always', but only displays for exported global bindings
    Exported
  | -- |  follows error messages produced by GHC
    Diagnostics
  deriving (Eq, Ord, Show, Read, Enum)

instance A.ToJSON Mode where
  toJSON Always      = "always"
  toJSON Exported    = "exported"
  toJSON Diagnostics = "diagnostics"

instance A.FromJSON Mode where
  parseJSON = A.withText "Mode" $ \case
    "always"      -> pure Always
    "exported"    -> pure Exported
    "diagnostics" -> pure Diagnostics
    _             -> mzero

--------------------------------------------------------------------------------

showDocRdrEnv :: DynFlags -> GlobalRdrEnv -> SDoc -> String
showDocRdrEnv dflags rdrEnv = showSDocForUser dflags (mkPrintUnqualified dflags rdrEnv)

data GetGlobalBindingTypeSigs = GetGlobalBindingTypeSigs
  deriving (Generic, Show, Eq, Ord, Hashable, NFData, Binary)

data GlobalBindingTypeSig = GlobalBindingTypeSig
  { gbName     :: Name,
    gbRendered :: String,
    gbExported :: Bool
  }

gbSrcSpan :: GlobalBindingTypeSig -> SrcSpan
gbSrcSpan GlobalBindingTypeSig {gbName} = getSrcSpan gbName

newtype GlobalBindingTypeSigsResult = GlobalBindingTypeSigsResult [GlobalBindingTypeSig]

instance Show GlobalBindingTypeSigsResult where
  show _ = "<GetTypeResult>"

instance NFData GlobalBindingTypeSigsResult where
  rnf = rwhnf

type instance RuleResult GetGlobalBindingTypeSigs = GlobalBindingTypeSigsResult

rules :: Rules ()
rules = do
  define $ \GetGlobalBindingTypeSigs nfp -> do
    tmr <- use TypeCheck nfp
    let result = gblBindingType (ms_hspp_opts . tmrModSummary <$> tmr) (tmrTypechecked <$> tmr)
    pure ([], result)

gblBindingType :: Maybe DynFlags -> Maybe TcGblEnv -> Maybe GlobalBindingTypeSigsResult
gblBindingType (Just dflags) (Just gblEnv) =
  let exports = availsToNameSet $ tcg_exports gblEnv
      sigs = tcg_sigs gblEnv
      binds = collectHsBindsBinders $ tcg_binds gblEnv
      patSyns = tcg_patsyns gblEnv
      rdrEnv = tcg_rdr_env gblEnv
      showDoc = showDocRdrEnv dflags rdrEnv
      tyEnv = tcg_type_env gblEnv
      isExported name = name `elemNameSet` exports
      prettyPrintTy ty = showDoc (pprSigmaType ty)
      bindToSig id
        | name <- idName id,
          name `elemNameSet` sigs,
          Just ty <- lookupTypeEnv tyEnv name >>= safeTyThingType =
          Just $ GlobalBindingTypeSig name (printName name <> " :: " <> prettyPrintTy ty) (isExported name)
        | otherwise = Nothing
      patToSig p
        | name <- patSynName p,
          name `elemNameSet` sigs,
          -- we don't use pprPatSynType, since it always prints forall
          Just ty <- lookupTypeEnv tyEnv name >>= safeTyThingType =
          Just $ GlobalBindingTypeSig name ("pattern " <> printName name <> " :: " <> prettyPrintTy ty) (isExported name)
        | otherwise = Nothing
      bindings = catMaybes $ bindToSig <$> binds
      patterns = catMaybes $ patToSig <$> patSyns
   in Just . GlobalBindingTypeSigsResult $ bindings <> patterns
gblBindingType _ _ = Nothing
