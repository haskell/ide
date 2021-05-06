{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE NoMonoLocalBinds  #-}
{-# LANGUAGE RankNTypes #-}

module Wingman.LanguageServer.Metaprogram (hoverProvider) where

import           Control.Applicative (empty)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Generics.Aliases (mkQ, GenericQ)
import           Data.Generics.Schemes (everything)
import           Data.List (find)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Traversable
import           Development.IDE (positionToRealSrcLoc)
import           Development.IDE (realSrcSpanToRange)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake (IdeState (..))
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat
import           GhcPlugins (unpackFS, containsSpan, realSrcLocSpan)
import           Ide.Types
import           Language.LSP.Types
import           Prelude hiding (span)
import           Prelude hiding (span)
import           TcRnTypes (tcg_binds)
import           Wingman.GHC
import           Wingman.LanguageServer
import           Wingman.Metaprogramming.Parser (attempt_it)
import           Wingman.StaticPlugin (pattern WingmanMetaprogram)
import Wingman.Judgements.SYB (everythingContaining)


------------------------------------------------------------------------------
-- | Provide the "empty case completion" code lens
hoverProvider :: PluginMethodHandler IdeState TextDocumentHover
hoverProvider state plId (HoverParams (TextDocumentIdentifier uri) pos _)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      let loc = positionToRealSrcLoc nfp pos

      cfg <- getTacticConfig plId
      liftIO $ fromMaybeT (Right Nothing) $ do
        -- guard $ hasFeature FeatureEmptyCase $ cfg_feature_set cfg

        holes <- emptyCaseScrutinees state nfp $ RealSrcSpan $ realSrcLocSpan loc

        fmap (Right . Just) $
          case (find (flip containsSpan (realSrcLocSpan loc) . unTrack . fst) holes) of
            Just (trss, program) -> do
              let tr_range = fmap realSrcSpanToRange trss
              HoleJudgment{hj_jdg=jdg, hj_ctx=ctx} <- judgementForHole state nfp tr_range cfg
              pure $ Hover
                { _contents = HoverContents
                            $ MarkupContent MkMarkdown
                            $ either T.pack T.pack
                            $ attempt_it ctx jdg
                            $ T.unpack program
                , _range = Just $ unTrack tr_range
                }
            Nothing -> empty
hoverProvider _ _ _ = pure $ Right Nothing


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT def = fmap (fromMaybe def) . runMaybeT


emptyCaseScrutinees
    :: IdeState
    -> NormalizedFilePath
    -> SrcSpan
    -> MaybeT IO [(Tracked 'Current RealSrcSpan, T.Text)]
emptyCaseScrutinees state nfp ss = do
    let stale a = runStaleIde "emptyCaseScrutinees" state nfp a

    TrackedStale tcg tcg_map <- fmap (fmap tmrTypechecked) $ stale TypeCheck

    let scrutinees = traverse (metaprogramQ ss . tcg_binds) tcg
    for scrutinees $ \aged@(unTrack -> (ss, program)) -> do
      case ss of
        RealSrcSpan r   -> do
          rss' <- liftMaybe $ mapAgeTo tcg_map $ unsafeCopyAge aged r
          pure (rss', program)
        UnhelpfulSpan _ -> empty


metaprogramQ :: SrcSpan -> GenericQ [(SrcSpan, T.Text)]
metaprogramQ ss = everythingContaining ss $ mkQ mempty $ \case
  L new_span (WingmanMetaprogram program) -> pure (new_span, T.pack $ unpackFS $ program)
  (_ :: LHsExpr GhcTc) -> mempty
