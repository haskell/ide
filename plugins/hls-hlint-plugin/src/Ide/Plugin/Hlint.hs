{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Ide.Plugin.Hlint
  (
    descriptor
  --, provider
  ) where
import Refact.Apply
import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Exception
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson.Types (ToJSON(..), FromJSON(..), Value(..))
import Data.Binary
import Data.Default
import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Development.IDE
import Development.IDE.Core.Rules (getParsedModuleWithComments, defineNoFile)
import Development.IDE.Core.Shake (getDiagnostics)

#ifdef HLINT_ON_GHC_LIB
import Data.List (nub)
import "ghc-lib" GHC hiding (DynFlags(..), ms_hspp_opts, pm_mod_summary, ms_hspp_file, ms_hspp_buf)
import "ghc-lib-parser" GHC.LanguageExtensions (Extension)
import Development.IDE.GHC.Compat (DynFlags(..), topDir, ms_hspp_opts, pm_mod_summary, ms_hspp_file, ms_hspp_buf)
import Development.IDE.GHC.Util (stringBufferToString)
import "ghc" HscTypes as RealGHC.HscTypes (hsc_dflags)
import qualified "ghc" EnumSet as EnumSet
import Language.Haskell.GhclibParserEx.GHC.Driver.Session as GhclibParserEx (readExtension)
import System.Environment(setEnv, unsetEnv)
import System.FilePath (takeFileName)
import System.IO (hPutStr, noNewlineTranslation, hSetNewlineMode, utf8, hSetEncoding, IOMode(WriteMode), withFile, hClose)
import System.IO.Temp
#else
import Development.IDE.GHC.Compat hiding (DynFlags(..))
import Language.Haskell.GHC.ExactPrint.Parsers (postParseTransform)
import Language.Haskell.GHC.ExactPrint.Delta (deltaOptions)
import Language.Haskell.GHC.ExactPrint.Types (Rigidity(..))
#endif

import Ide.Logger
import Ide.Types
import Ide.Plugin.Config
import Ide.PluginUtils
import Language.Haskell.HLint as Hlint
import Language.LSP.Server
    ( withIndefiniteProgress,
      sendRequest,
      ProgressCancellable(Cancellable) )
import Language.LSP.Types
import qualified Language.LSP.Types      as LSP
import qualified Language.LSP.Types.Lens as LSP

import Text.Regex.TDFA.Text()
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginRules = rules plId
  , pluginCommands =
      [ PluginCommand "applyOne" "Apply a single hint" applyOneCmd
      , PluginCommand "applyAll" "Apply all hints to the file" applyAllCmd
      ]
    , pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
  }

-- This rule only exists for generating file diagnostics
-- so the RuleResult is empty
data GetHlintDiagnostics = GetHlintDiagnostics
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHlintDiagnostics
instance NFData   GetHlintDiagnostics
instance Binary   GetHlintDiagnostics

type instance RuleResult GetHlintDiagnostics = ()

-- | Hlint rules to generate file diagnostics based on hlint hints
-- | This rule is recomputed when:
-- | - The files of interest have changed via `getFilesOfInterest`
-- | - One of those files has been edited via
-- |    - `getIdeas` -> `getParsedModule` in any case
-- |    - `getIdeas` -> `getFileContents` if the hls ghc does not match the hlint default ghc
-- | - The client settings have changed, to honour the `hlintOn` setting, via `getClientConfigAction`
-- | - The hlint specific settings have changed, via `getHlintSettingsRule`
rules :: PluginId -> Rules ()
rules plugin = do
  define $ \GetHlintDiagnostics file -> do
    config <- getClientConfigAction def
    let pluginConfig = configForPlugin config plugin
    let hlintOn' = hlintOn config && plcGlobalOn pluginConfig && plcDiagnosticsOn pluginConfig
    ideas <- if hlintOn' then getIdeas file else return (Right [])
    return (diagnostics file ideas, Just ())

  getHlintSettingsRule (HlintEnabled [])

  action $ do
    files <- getFilesOfInterest
    void $ uses GetHlintDiagnostics $ Map.keys files

  where

      diagnostics :: NormalizedFilePath -> Either ParseError [Idea] -> [FileDiagnostic]
      diagnostics file (Right ideas) =
        [(file, ShowDiag, ideaToDiagnostic i) | i <- ideas, ideaSeverity i /= Ignore]
      diagnostics file (Left parseErr) =
        [(file, ShowDiag, parseErrorToDiagnostic parseErr)]

      ideaToDiagnostic :: Idea -> Diagnostic
      ideaToDiagnostic idea =
        LSP.Diagnostic {
            _range    = srcSpanToRange $ ideaSpan idea
          , _severity = Just LSP.DsInfo
          -- we are encoding the fact that idea has refactorings in diagnostic code
          , _code     = Just (InR $ T.pack $ codePre ++ ideaHint idea)
          , _source   = Just "hlint"
          , _message  = idea2Message idea
          , _relatedInformation = Nothing
          , _tags     = Nothing
        }
        where codePre = if null $ ideaRefactoring idea then "" else "refact:"

      idea2Message :: Idea -> T.Text
      idea2Message idea = T.unlines $ [T.pack $ ideaHint idea, "Found:", "  " <> T.pack (ideaFrom idea)]
                                     <> toIdea <> map (T.pack . show) (ideaNote idea)
        where
          toIdea :: [T.Text]
          toIdea = case ideaTo idea of
            Nothing -> []
            Just i  -> [T.pack "Why not:", T.pack $ "  " ++ i]


      parseErrorToDiagnostic :: ParseError -> Diagnostic
      parseErrorToDiagnostic (Hlint.ParseError l msg contents) =
        LSP.Diagnostic {
            _range    = srcSpanToRange l
          , _severity = Just LSP.DsInfo
          , _code     = Just (InR "parser")
          , _source   = Just "hlint"
          , _message  = T.unlines [T.pack msg,T.pack contents]
          , _relatedInformation = Nothing
          , _tags     = Nothing
        }

      -- This one is defined in Development.IDE.GHC.Error but here
      -- the types could come from ghc-lib or ghc
      srcSpanToRange :: SrcSpan -> LSP.Range
      srcSpanToRange (RealSrcSpan span) = Range {
          _start = LSP.Position {
                _line = srcSpanStartLine span - 1
              , _character  = srcSpanStartCol span - 1}
        , _end   = LSP.Position {
                _line = srcSpanEndLine span - 1
             , _character = srcSpanEndCol span - 1}
        }
      srcSpanToRange (UnhelpfulSpan _) = noRange

getIdeas :: NormalizedFilePath -> Action (Either ParseError [Idea])
getIdeas nfp = do
  logm $ "hlint:getIdeas:file:" ++ show nfp
  (flags, classify, hint) <- useNoFile_ GetHlintSettings

  let applyHints' (Just (Right modEx)) = Right $ applyHints classify hint [modEx]
      applyHints' (Just (Left err)) = Left err
      applyHints' Nothing = Right []

  fmap applyHints' (moduleEx flags)

  where moduleEx :: ParseFlags -> Action (Maybe (Either ParseError ModuleEx))
#ifndef HLINT_ON_GHC_LIB
        moduleEx _flags = do
          mbpm <- getParsedModule nfp
          return $ createModule <$> mbpm
          where createModule pm = Right (createModuleEx anns modu)
                  where anns = pm_annotations pm
                        modu = pm_parsed_source pm
#else
        moduleEx flags = do
          mbpm <- getParsedModule nfp
          -- If ghc was not able to parse the module, we disable hlint diagnostics
          case mbpm of
            Nothing -> return Nothing
            Just pm -> do
                flags' <- setExtensions flags
                let hspp@(fp, contents) = getHsppPathAndContents pm
                (fp', contents') <- case contents of
                                      Just c -> return hspp
                                      Nothing -> getPathAndContents
                Just <$> (liftIO $ parseModuleEx flags' fp' contents')

        getHsppPathAndContents m =
            (ms_hspp_file modsum, stringBufferToString <$> ms_hspp_buf modsum)
          where modsum = pm_mod_summary m

        getPathAndContents = do
            (_, contents) <- getFileContents nfp
            let fp = fromNormalizedFilePath nfp
            return (fp, T.unpack <$> contents)

        setExtensions flags = do
          hlintExts <- getExtensions flags nfp
          logm $ "hlint:getIdeas:setExtensions:" ++ show hlintExts
          return $ flags { enabledExtensions = hlintExts }

getExtensions :: ParseFlags -> NormalizedFilePath -> Action [Extension]
getExtensions pflags nfp = do
    dflags <- getFlags
    let hscExts = EnumSet.toList (extensionFlags dflags)
    let hscExts' = mapMaybe (GhclibParserEx.readExtension . show) hscExts
    let hlintExts = nub $ enabledExtensions pflags ++ hscExts'
    return hlintExts
  where getFlags :: Action DynFlags
        getFlags = do
          (modsum, _) <- use_ GetModSummary nfp
          return $ ms_hspp_opts modsum
#endif

-- ---------------------------------------------------------------------

data HlintUsage
  = HlintEnabled { cmdArgs :: [String] }
  | HlintDisabled
  deriving Show

data GetHlintSettings = GetHlintSettings
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHlintSettings
instance NFData   GetHlintSettings
instance NFData Hint where rnf = rwhnf
instance NFData Classify where rnf = rwhnf
instance NFData ParseFlags where rnf = rwhnf
instance Show Hint where show = const "<hint>"
instance Show ParseFlags where show = const "<parseFlags>"
instance Binary GetHlintSettings

type instance RuleResult GetHlintSettings = (ParseFlags, [Classify], Hint)

getHlintSettingsRule :: HlintUsage -> Rules ()
getHlintSettingsRule usage =
    defineNoFile $ \GetHlintSettings ->
      liftIO $ case usage of
          HlintEnabled cmdArgs -> argsSettings cmdArgs
          HlintDisabled -> fail "hlint configuration unspecified"

-- ---------------------------------------------------------------------

codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState plId (CodeActionParams _ _ docId _ context) = Right . LSP.List . map InR <$> liftIO getCodeActions
  where

    getCodeActions = do
        diags <- getDiagnostics ideState
        let docNfp = toNormalizedFilePath' <$> uriToFilePath' (docId ^. LSP.uri)
            numHintsInDoc = length
              [d | (nfp, _, d) <- diags
                 , validCommand d
                 , Just nfp == docNfp
              ]
        -- We only want to show the applyAll code action if there is more than 1
        -- hint in the current document
        if numHintsInDoc > 1 then do
          pure $ applyAllAction:applyOneActions
        else
          pure applyOneActions

    applyAllAction =
      let args = Just [toJSON (docId ^. LSP.uri)]
          cmd = mkLspCommand plId "applyAll" "Apply all hints" args
        in LSP.CodeAction "Apply all hints" (Just LSP.CodeActionQuickFix) Nothing Nothing Nothing Nothing (Just cmd)

    applyOneActions :: [LSP.CodeAction]
    applyOneActions = mapMaybe mkHlintAction (filter validCommand diags)

    -- |Some hints do not have an associated refactoring
    validCommand (LSP.Diagnostic _ _ (Just (InR code)) (Just "hlint") _ _ _) =
        "refact:" `T.isPrefixOf` code
    validCommand _ =
        False

    LSP.List diags = context ^. LSP.diagnostics

    mkHlintAction :: LSP.Diagnostic -> Maybe LSP.CodeAction
    mkHlintAction diag@(LSP.Diagnostic (LSP.Range start _) _s (Just (InR code)) (Just "hlint") _ _ _) =
      Just . codeAction $ mkLspCommand plId "applyOne" title (Just args)
     where
       codeAction cmd = LSP.CodeAction title (Just LSP.CodeActionQuickFix) (Just (LSP.List [diag])) Nothing Nothing Nothing (Just cmd)
       -- we have to recover the original ideaHint removing the prefix
       ideaHint = T.replace "refact:" "" code
       title = "Apply hint: " <> ideaHint
       -- need 'file', 'start_pos' and hint title (to distinguish between alternative suggestions at the same location)
       args = [toJSON (AOP (docId ^. LSP.uri) start ideaHint)]
    mkHlintAction (LSP.Diagnostic _r _s _c _source _m _ _) = Nothing

-- ---------------------------------------------------------------------

applyAllCmd :: CommandFunction IdeState Uri
applyAllCmd ide uri = do
  let file = maybe (error $ show uri ++ " is not a file.")
                    toNormalizedFilePath'
                   (uriToFilePath' uri)
  withIndefiniteProgress "Applying all hints" Cancellable $ do
    logm $ "hlint:applyAllCmd:file=" ++ show file
    res <- liftIO $ applyHint ide file Nothing
    logm $ "hlint:applyAllCmd:res=" ++ show res
    case res of
      Left err -> pure $ Left (responseError (T.pack $ "hlint:applyAll: " ++ show err))
      Right fs -> do
        _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing fs) (\_ -> pure ())
        pure $ Right Null

-- ---------------------------------------------------------------------

data ApplyOneParams = AOP
  { file      :: Uri
  , start_pos :: Position
  -- | There can be more than one hint suggested at the same position, so HintTitle is used to distinguish between them.
  , hintTitle :: HintTitle
  } deriving (Eq,Show,Generic,FromJSON,ToJSON)

type HintTitle = T.Text

data OneHint = OneHint
  { oneHintPos :: Position
  , oneHintTitle :: HintTitle
  } deriving (Eq, Show)

applyOneCmd :: CommandFunction IdeState ApplyOneParams
applyOneCmd ide (AOP uri pos title) = do
  let oneHint = OneHint pos title
  let file = maybe (error $ show uri ++ " is not a file.") toNormalizedFilePath'
                   (uriToFilePath' uri)
  let progTitle = "Applying hint: " <> title
  withIndefiniteProgress progTitle Cancellable $ do
    logm $ "hlint:applyOneCmd:file=" ++ show file
    res <- liftIO $ applyHint ide file (Just oneHint)
    logm $ "hlint:applyOneCmd:res=" ++ show res
    case res of
      Left err -> pure $ Left (responseError (T.pack $ "hlint:applyOne: " ++ show err))
      Right fs -> do
        _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing fs) (\_ -> pure ())
        pure $ Right Null

applyHint :: IdeState -> NormalizedFilePath -> Maybe OneHint -> IO (Either String WorkspaceEdit)
applyHint ide nfp mhint =
  runExceptT $ do
    let runAction' :: Action a -> IO a
        runAction' = runAction "applyHint" ide
    let errorHandlers = [ Handler $ \e -> return (Left (show (e :: IOException)))
                        , Handler $ \e -> return (Left (show (e :: ErrorCall)))
                        ]
    ideas <- bimapExceptT showParseError id $ ExceptT $ runAction' $ getIdeas nfp
    let ideas' = maybe ideas (`filterIdeas` ideas) mhint
    let commands = map ideaRefactoring ideas'
    liftIO $ logm $ "applyHint:apply=" ++ show commands
    let fp = fromNormalizedFilePath nfp
    (_, mbOldContent) <- liftIO $ runAction' $ getFileContents nfp
    oldContent <- maybe (liftIO $ T.readFile fp) return mbOldContent
    (modsum, _) <- liftIO $ runAction' $ use_ GetModSummary nfp
    let dflags = ms_hspp_opts modsum
    -- set Nothing as "position" for "applyRefactorings" because
    -- applyRefactorings expects the provided position to be _within_ the scope
    -- of each refactoring it will apply.
    -- But "Idea"s returned by HLint point to starting position of the expressions
    -- that contain refactorings, so they are often outside the refactorings' boundaries.
    -- Example:
    -- Given an expression "hlintTest = reid $ (myid ())"
    -- Hlint returns an idea at the position (1,13)
    -- That contains "Redundant brackets" refactoring at position (1,20):
    --
    -- [("src/App/Test.hs:5:13: Warning: Redundant bracket\nFound:\n  reid $ (myid ())\nWhy not:\n  reid $ myid ()\n",[Replace {rtype = Expr, pos = SrcSpan {startLine = 5, startCol = 20, endLine = 5, endCol = 29}, subts = [("x",SrcSpan {startLine = 5, startCol = 21, endLine = 5, endCol = 28})], orig = "x"}])]
    --
    -- If we provide "applyRefactorings" with "Just (1,13)" then
    -- the "Redundant bracket" hint will never be executed
    -- because SrcSpan (1,20,??,??) doesn't contain position (1,13).
#ifdef HLINT_ON_GHC_LIB
    let writeFileUTF8NoNewLineTranslation file txt =
            withFile file WriteMode $ \h -> do
                hSetEncoding h utf8
                hSetNewlineMode h noNewlineTranslation
                hPutStr h (T.unpack txt)
    -- Setting a environment variable with the libdir used by ghc-exactprint.
    -- It is a workaround for an error caused by the use of a hadcoded at compile time libdir
    -- in ghc-exactprint that makes dependent executables non portables.
    -- See https://github.com/alanz/ghc-exactprint/issues/96.
    -- WARNING: this code is not thread safe, so if you try to apply several async refactorings
    -- it could fail. That case is not very likely so we assume the risk.
    let withRuntimeLibdir :: IO a -> IO a
        withRuntimeLibdir = bracket_ (setEnv key $ topDir dflags) (unsetEnv key)
            where key = "GHC_EXACTPRINT_GHC_LIBDIR"
    res <-
        liftIO $ withSystemTempFile (takeFileName fp) $ \temp h -> do
            hClose h
            writeFileUTF8NoNewLineTranslation temp oldContent
            (pflags, _, _) <- runAction' $ useNoFile_ GetHlintSettings
            exts <- runAction' $ getExtensions pflags nfp
            -- We have to reparse extensions to remove the invalid ones
            let (enabled, disabled, _invalid) = parseExtensions $ map show exts
            let refactExts = map show $ enabled ++ disabled
            (Right <$> withRuntimeLibdir (applyRefactorings Nothing commands temp refactExts))
                `catches` errorHandlers
#else
    mbParsedModule <- liftIO $ runAction' $ getParsedModuleWithComments nfp
    res <-
        case mbParsedModule of
            Nothing -> throwE "Apply hint: error parsing the module"
            Just pm -> do
                let anns = pm_annotations pm
                let modu = pm_parsed_source pm
                -- apply-refact uses RigidLayout
                let rigidLayout = deltaOptions RigidLayout
                (anns', modu') <-
                    ExceptT $ return $ postParseTransform (Right (anns, [], dflags, modu)) rigidLayout
                liftIO $ (Right <$> applyRefactorings' Nothing commands anns' modu')
                            `catches` errorHandlers
#endif
    case res of
      Right appliedFile -> do
        let uri = fromNormalizedUri (filePathToUri' nfp)
        let wsEdit = diffText' True (uri, oldContent) (T.pack appliedFile) IncludeDeletions
        liftIO $ logm $ "hlint:applyHint:diff=" ++ show wsEdit
        ExceptT $ return (Right wsEdit)
      Left err ->
        throwE err
    where
          -- | If we are only interested in applying a particular hint then
          -- let's filter out all the irrelevant ideas
          filterIdeas :: OneHint -> [Idea] -> [Idea]
          filterIdeas (OneHint (Position l c) title) ideas =
            let title' = T.unpack title
                ideaPos = (srcSpanStartLine &&& srcSpanStartCol) . toRealSrcSpan . ideaSpan
            in filter (\i -> ideaHint i == title' && ideaPos i == (l+1, c+1)) ideas

          toRealSrcSpan (RealSrcSpan real) = real
          toRealSrcSpan (UnhelpfulSpan x) = error $ "No real source span: " ++ show x

          showParseError :: Hlint.ParseError -> String
          showParseError (Hlint.ParseError location message content) =
            unlines [show location, message, content]

-- | Map over both failure and success.
bimapExceptT :: Functor m => (e -> f) -> (a -> b) -> ExceptT e m a -> ExceptT f m b
bimapExceptT f g (ExceptT m) = ExceptT (fmap h m) where
  h (Left e)  = Left (f e)
  h (Right a) = Right (g a)
{-# INLINE bimapExceptT #-}
