{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

-- | A plugin that uses tactics to synthesize code
module Ide.Plugin.Tactic
  ( descriptor
  , tacticTitle
  , TacticCommand (..)
  ) where

import           Control.Arrow
import           Control.DeepSeq (NFData)
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Binary (Binary)
import qualified Data.ByteString.Char8 as BS
import           Data.Coerce
import           Data.Generics.Aliases (mkQ)
import           Data.Generics.Schemes (everything)
import           Data.Hashable (Hashable)
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Development.IDE (ShowDiagnostic(ShowDiag))
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service (runAction)
import           Development.IDE.Core.Shake (GetModificationTime (..), defineEarlyCutoff, define, use, useWithStale, IdeState (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error (realSrcSpanToRange)
import           Development.IDE.Spans.LocalBindings (getDefiningBindings)
import           Development.Shake (RuleResult, Rules, Action)
import           DynFlags (xopt)
import qualified FastString
import           GHC.Generics (Generic)
import           GHC.LanguageExtensions.Type (Extension (LambdaCase))
import           Ide.Plugin (mkLspCommand)
import           Ide.Plugin.Tactic.Auto
import           Ide.Plugin.Tactic.Context
import           Ide.Plugin.Tactic.GHC
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Metaprogramming
import           Ide.Plugin.Tactic.Range
import           Ide.Plugin.Tactic.Tactics
import           Ide.Plugin.Tactic.TestTypes
import           Ide.Plugin.Tactic.Types
import           Ide.TreeTransform (transform, graft, useAnnotatedSource)
import           Ide.Types
import           Language.Haskell.LSP.Core (clientCapabilities)
import           Language.Haskell.LSP.Types
import           OccName
import           SrcLoc (containsSpan)
import           System.Timeout


descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
    { pluginCommands
        = fmap (\tc ->
            PluginCommand
              (tcCommandId tc)
              (tacticDesc $ tcCommandName tc)
              (tacticCmd $ commandTactic tc))
              [minBound .. maxBound]
    , pluginCodeActionProvider = Just codeActionProvider
    , pluginRules = tacticRules
    }

data GetMetaprogram = GetMetaprogram
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Hashable, NFData, Binary)

type instance RuleResult GetMetaprogram = Metaprogram

data GetMetaprogramCache = GetMetaprogramCache
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Hashable, NFData, Binary)

type instance RuleResult GetMetaprogramCache = MetaprogramCache

tacticRules :: Rules ()
tacticRules = do
  defineEarlyCutoff $ \GetMetaprogram nfp -> do
    mtime <- use GetModificationTime nfp
    contents <- liftIO $ T.readFile $ fromNormalizedFilePath nfp
    pure
      $ (Just $ BS.pack $ show mtime ,)
      $ case parseMetaprogram (fromNormalizedFilePath nfp) contents of
          Left err ->
            ( [ (nfp
                , ShowDiag
                , Diagnostic
                    (Range (Position 0 0)
                           (Position 0 0))
                    Nothing
                    Nothing
                    Nothing
                    (T.pack err)
                    Nothing
                    Nothing
                )
              ]
            , Nothing
            )
          Right mp -> ([], Just mp)
  define $ \GetMetaprogramCache _ -> do
    files <- liftIO getKnownFiles
    mps <- fmap catMaybes $ traverse (use GetMetaprogram) files
    pure ([], Just $ buildCache mps)


tacticDesc :: T.Text -> T.Text
tacticDesc name = "fill the hole using the " <> name <> " tactic"

------------------------------------------------------------------------------
-- | A 'TacticProvider' is a way of giving context-sensitive actions to the LS
-- UI.
type TacticProvider
    = DynFlags
   -> MetaprogramCache
   -> PluginId
   -> Uri
   -> Range
   -> Judgement
   -> IO [CAResult]


------------------------------------------------------------------------------
-- | Construct a 'CommandId'
tcCommandId :: TacticCommand -> CommandId
tcCommandId c = coerce $ T.pack $ "tactics" <> show c <> "Command"


------------------------------------------------------------------------------
-- | The name of the command for the LS.
tcCommandName :: TacticCommand -> T.Text
tcCommandName = T.pack . show

------------------------------------------------------------------------------
-- | Mapping from tactic commands to their contextual providers. See 'provide',
-- 'filterGoalType' and 'filterBindingType' for the nitty gritty.
commandProvider :: TacticCommand -> TacticProvider
commandProvider Auto  = provide Auto ""
commandProvider Intros =
  filterGoalType isFunction $
    provide Intros ""
commandProvider Destruct =
  filterBindingType destructFilter $ \occ _ ->
    provide Destruct $ T.pack $ occNameString occ
commandProvider Homomorphism =
  filterBindingType homoFilter $ \occ _ ->
    provide Homomorphism $ T.pack $ occNameString occ
commandProvider DestructLambdaCase =
  requireExtension LambdaCase $
    filterGoalType (isJust . lambdaCaseable) $
      provide DestructLambdaCase ""
commandProvider HomomorphismLambdaCase =
  requireExtension LambdaCase $
    filterGoalType ((== Just True) . lambdaCaseable) $
      provide HomomorphismLambdaCase ""
commandProvider RunMetaprogram =
  allMetaprograms $ \mp ->
    provide RunMetaprogram mp


------------------------------------------------------------------------------
-- | A mapping from tactic commands to actual tactics for refinery.
commandTactic :: TacticCommand -> Maybe Metaprogram -> OccName -> TacticsM ()
commandTactic Auto _ _                   = auto
commandTactic Intros _ _                 = intros
commandTactic Destruct _ occ             = destruct occ
commandTactic Homomorphism _ occ         = homo occ
commandTactic DestructLambdaCase _ _     = destructLambdaCase
commandTactic HomomorphismLambdaCase _ _ = homoLambdaCase
commandTactic RunMetaprogram (Just mp) _ = mp_program mp
-- TODO(sandy): better error here
commandTactic RunMetaprogram Nothing _   = throwError NoApplicableTactic


------------------------------------------------------------------------------
-- | We should show homos only when the goal type is the same as the binding
-- type, and that both are usual algebraic types.
homoFilter :: Type -> Type -> Bool
homoFilter (algebraicTyCon -> Just t1) (algebraicTyCon -> Just t2) = t1 == t2
homoFilter _ _ = False


------------------------------------------------------------------------------
-- | We should show destruct for bindings only when those bindings have usual
-- algebraic types.
destructFilter :: Type -> Type -> Bool
destructFilter _ (algebraicTyCon -> Just _) = True
destructFilter _ _ = False


runIde :: IdeState -> Action a -> IO a
runIde state = runAction "tactic" state


codeActionProvider :: CodeActionProvider
codeActionProvider _conf state plId (TextDocumentIdentifier uri) range _ctx
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri =
      fromMaybeT (Right $ List []) $ do
        (_, jdg, _, dflags, mpc) <- judgementForHole state nfp range
        actions <- lift $
          -- This foldMap is over the function monoid.
          foldMap commandProvider [minBound .. maxBound]
            dflags
            mpc
            plId
            uri
            range
            jdg
        pure $ Right $ List actions
codeActionProvider _ _ _ _ _ _ = pure $ Right $ codeActions []


codeActions :: [CodeAction] -> List CAResult
codeActions = List . fmap CACodeAction


------------------------------------------------------------------------------
-- | Terminal constructor for providing context-sensitive tactics. Tactics
-- given by 'provide' are always available.
provide :: TacticCommand -> T.Text -> TacticProvider
provide tc name _ _ plId uri range _ = do
  let title = tacticTitle tc name
      params = TacticParams { file = uri , range = range , var_name = name }
  cmd <- mkLspCommand plId (tcCommandId tc) title (Just [toJSON params])
  pure
    $ pure
    $ CACodeAction
    $ CodeAction title (Just CodeActionQuickFix) Nothing Nothing
    $ Just cmd


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
requireExtension :: Extension -> TacticProvider -> TacticProvider
requireExtension ext tp dflags mpc plId uri range jdg =
  case xopt ext dflags of
    True  -> tp dflags mpc plId uri range jdg
    False -> pure []


------------------------------------------------------------------------------
-- | Restrict a 'TacticProvider', making sure it appears only when the given
-- predicate holds for the goal.
filterGoalType :: (Type -> Bool) -> TacticProvider -> TacticProvider
filterGoalType p tp dflags mpc plId uri range jdg =
  case p $ unCType $ jGoal jdg of
    True  -> tp dflags mpc plId uri range jdg
    False -> pure []


------------------------------------------------------------------------------
-- | Multiply a 'TacticProvider' for each binding, making sure it appears only
-- when the given predicate holds over the goal and binding types.
filterBindingType
    :: (Type -> Type -> Bool)  -- ^ Goal and then binding types.
    -> (OccName -> Type -> TacticProvider)
    -> TacticProvider
filterBindingType p tp dflags mpc plId uri range jdg =
  let hy = jHypothesis jdg
      g  = jGoal jdg
   in fmap join $ for (M.toList hy) $ \(occ, CType ty) ->
        case p (unCType g) ty of
          True  -> tp occ ty dflags mpc plId uri range jdg
          False -> pure []

allMetaprograms
    :: (T.Text -> TacticProvider)
    -> TacticProvider
allMetaprograms f dflags mpc@(MetaprogramCache cache) plId uri range jdg =
  fmap join $ for (M.keys cache) $ \key -> f key dflags mpc plId uri range jdg


data TacticParams = TacticParams
    { file :: Uri -- ^ Uri of the file to fill the hole in
    , range :: Range -- ^ The range of the hole
    , var_name :: T.Text
    }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


------------------------------------------------------------------------------
-- | Find the last typechecked module, and find the most specific span, as well
-- as the judgement at the given range.
judgementForHole
    :: IdeState
    -> NormalizedFilePath
    -> Range
    -> MaybeT IO (Range, Judgement, Context, DynFlags, MetaprogramCache)
judgementForHole state nfp range = do
  (asts, amapping) <- MaybeT $ runIde state $ useWithStale GetHieAst nfp
  range' <- liftMaybe $ fromCurrentRange amapping range

  (binds, _) <- MaybeT $ runIde state $ useWithStale GetBindings nfp

  -- Ok to use the stale 'ModIface', since all we need is its 'DynFlags'
  -- which don't change very often.
  (modsum, _) <- MaybeT $ runIde state $ useWithStale GetModSummaryWithoutTimestamps nfp
  let dflags = ms_hspp_opts modsum

  (rss, goal) <- liftMaybe $ join $ listToMaybe $ M.elems $ flip M.mapWithKey (getAsts $ hieAst asts) $ \fs ast ->
      case selectSmallestContaining (rangeToRealSrcSpan (FastString.unpackFS fs) range') ast of
        Nothing -> Nothing
        Just ast' -> do
          let info = nodeInfo ast'
          ty <- listToMaybe $ nodeType info
          guard $ ("HsUnboundVar","HsExpr") `S.member` nodeAnnotations info
          pure (nodeSpan ast', ty)

  resulting_range <- liftMaybe $ toCurrentRange amapping $ realSrcSpanToRange rss
  (tcmod, _) <- MaybeT $ runIde state $ useWithStale TypeCheck nfp
  mpc <- MaybeT $ runIde state $ use GetMetaprogramCache nfp
  let tcg = fst $ tm_internals_ $ tmrModule tcmod
      tcs = tm_typechecked_source $ tmrModule tcmod
      ctx = mkContext
              mpc
              (mapMaybe (sequenceA . (occName *** coerce))
                $ getDefiningBindings binds rss)
              tcg
      hyps = hypothesisFromBindings rss binds
  pure ( resulting_range
       , mkFirstJudgement
           hyps
           (isRhsHole rss tcs)
           (maybe
              mempty
              (uncurry M.singleton . fmap pure)
                $ getRhsPosVals rss tcs)
           goal
       , ctx
       , dflags
       , mpc
       )



tacticCmd :: (Maybe Metaprogram -> OccName -> TacticsM ()) -> CommandFunction TacticParams
-- TODO(sandy): This is gross; it is reusing var_name as a metaprogram name as well
tacticCmd tac lf state (TacticParams uri range var_name)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri =
      fromMaybeT (Right Null, Nothing) $ do
        (range', jdg, ctx, dflags, mpc) <- judgementForHole state nfp range
        let span = rangeToRealSrcSpan (fromNormalizedFilePath nfp) range'
        pm  <- MaybeT $ useAnnotatedSource "tacticsCmd" state nfp
        let mp = M.lookup var_name $ unMetaprogramCache mpc

        x <- lift $ timeout 2e8 $
          case runTactic ctx jdg
                $ tac mp
                $ mkVarOcc
                $ T.unpack var_name of
            Left err ->
              pure $ (, Nothing)
                $ Left
                $ ResponseError InvalidRequest (T.pack $ show err) Nothing
            Right rtr -> do
              traceMX "solns" $ rtr_other_solns rtr
              let g = graft (RealSrcSpan span) $ rtr_extract rtr
                  response = transform dflags (clientCapabilities lf) uri g pm
              pure $ case response of
                Right res -> (Right Null , Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams res))
                Left err -> (Left $ ResponseError InternalError (T.pack err) Nothing, Nothing)
        pure $ case x of
          Just y -> y
          Nothing -> (, Nothing)
                   $ Left
                   $ ResponseError InvalidRequest "timed out" Nothing
tacticCmd _ _ _ _ =
  pure ( Left $ ResponseError InvalidRequest (T.pack "Bad URI") Nothing
       , Nothing
       )


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT def = fmap (fromMaybe def) . runMaybeT

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe a = MaybeT $ pure a


------------------------------------------------------------------------------
-- | Is this hole immediately to the right of an equals sign?
isRhsHole :: RealSrcSpan -> TypecheckedSource -> Bool
isRhsHole rss tcs = everything (||) (mkQ False $ \case
  TopLevelRHS _ _ (L (RealSrcSpan span) _) -> containsSpan rss span
  _ -> False
  ) tcs


------------------------------------------------------------------------------
-- | Compute top-level position vals of a function
getRhsPosVals :: RealSrcSpan -> TypecheckedSource -> Maybe (OccName, [OccName])
getRhsPosVals rss tcs = getFirst $ everything (<>) (mkQ mempty $ \case
  TopLevelRHS name ps
      (L (RealSrcSpan span)  -- body with no guards and a single defn
         (HsVar _ (L _ hole)))
    | containsSpan rss span  -- which contains our span
    , isHole $ occName hole  -- and the span is a hole
    -> First $ do
        patnames <- traverse getPatName ps
        pure (occName name, patnames)
  _ -> mempty
  ) tcs



-- TODO(sandy): Make this more robust
isHole :: OccName -> Bool
isHole = isPrefixOf "_" . occNameString

