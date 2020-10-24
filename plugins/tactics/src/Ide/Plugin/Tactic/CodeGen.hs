{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}

module Ide.Plugin.Tactic.CodeGen where

import           Control.Monad.Except
import           Control.Monad.State (MonadState)
import           Control.Monad.State.Class (modify)
import           Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Traversable
import           DataCon
import           Development.IDE.GHC.Compat
import           GHC.Exts
import           GHC.SourceGen (RdrNameStr)
import           GHC.SourceGen.Binds
import           GHC.SourceGen.Expr
import           GHC.SourceGen.Overloaded
import           GHC.SourceGen.Pat
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Machinery
import           Ide.Plugin.Tactic.Naming
import           Ide.Plugin.Tactic.Types
import           Name
import           Type hiding (Var)


useOccName :: MonadState TacticState m => Judgement -> OccName -> m ()
useOccName jdg name =
  case M.lookup name $ jHypothesis jdg of
    Just{}  -> modify $ withUsedVals $ S.insert name
    Nothing -> pure ()


destructMatches
    :: (DataCon -> Judgement -> Rule)
       -- ^ How to construct each match
    -> ([(OccName, CType)] -> Judgement -> Judgement)
       -- ^ How to derive each match judgement
    -> CType
       -- ^ Type being destructed
    -> Judgement
    -> RuleM (Trace, [RawMatch])
destructMatches f f2 t jdg = do
  let hy = jHypothesis jdg
      g  = jGoal jdg
  case splitTyConApp_maybe $ unCType t of
    Nothing -> throwError $ GoalMismatch "destruct" g
    Just (tc, apps) -> do
      let dcs = tyConDataCons tc
      case dcs of
        [] -> throwError $ GoalMismatch "destruct" g
        _ -> fmap unzipTrace $ for dcs $ \dc -> do
          let args = dataConInstOrigArgTys' dc apps
          names <- mkManyGoodNames hy args
          let hy' = zip names $ coerce args
              dcon_name = nameOccName $ dataConName dc

          let j = f2 hy'
                $ withPositionMapping dcon_name names
                $ introducingPat hy'
                $ withNewGoal g jdg
          (tr, sg) <- f dc j
          modify $ withIntroducedVals $ mappend $ S.fromList names
          pure ( rose ("match " <> show dc <> " {" <>
                          intercalate ", " (fmap show names) <> "}")
                    $ pure tr
               , match [mkDestructPat dc names] $ unLoc sg
               )


------------------------------------------------------------------------------
-- | Produces a pattern for a data con and the names of its fields.
mkDestructPat :: DataCon -> [OccName] -> Pat GhcPs
mkDestructPat dcon names
  | isTupleDataCon dcon =
      tuple pat_args
  | otherwise =
      infixifyPatIfNecessary dcon $
        conP
          (coerceName $ dataConName dcon)
          pat_args
  where
    pat_args = fmap bvar' names


infixifyPatIfNecessary :: DataCon -> Pat GhcPs -> Pat GhcPs
infixifyPatIfNecessary dcon x
  | dataConIsInfix dcon =
      case x of
        ConPatIn op (PrefixCon [lhs, rhs]) ->
          ConPatIn op $ InfixCon lhs rhs
        y -> y
  | otherwise = x



unzipTrace :: [(Trace, a)] -> (Trace, [a])
unzipTrace l =
  let (trs, as) = unzip l
   in (rose mempty trs, as)


-- | Essentially same as 'dataConInstOrigArgTys' in GHC,
--   but we need some tweaks in GHC >= 8.8.
--   Since old 'dataConInstArgTys' seems working with >= 8.8,
--   we just filter out non-class types in the result.
dataConInstOrigArgTys' :: DataCon -> [Type] -> [Type]
dataConInstOrigArgTys' con ty =
    let tys0 = dataConInstArgTys con ty
    in filter (maybe True (not . isClassTyCon) . tyConAppTyCon_maybe) tys0

------------------------------------------------------------------------------
-- | Combinator for performing case splitting, and running sub-rules on the
-- resulting matches.

destruct' :: (DataCon -> Judgement -> Rule) -> OccName -> Judgement -> Rule
destruct' f term jdg = do
  when (isDestructBlacklisted jdg) $ throwError NoApplicableTactic
  let hy = jHypothesis jdg
  case find ((== term) . fst) $ toList hy of
    Nothing -> throwError $ UndefinedHypothesis term
    Just (_, t) -> do
      useOccName jdg term
      (tr, ms)
          <- destructMatches
               f
               (\cs -> setParents term (fmap fst cs) . destructing term)
               t
               jdg
      pure ( rose ("destruct " <> show term) $ pure tr
           , noLoc $ case' (var' term) ms
           )


------------------------------------------------------------------------------
-- | Combinator for performign case splitting, and running sub-rules on the
-- resulting matches.
destructLambdaCase' :: (DataCon -> Judgement -> Rule) -> Judgement -> Rule
destructLambdaCase' f jdg = do
  when (isDestructBlacklisted jdg) $ throwError NoApplicableTactic
  let g  = jGoal jdg
  case splitFunTy_maybe (unCType g) of
    Just (arg, _) | isAlgType arg ->
      fmap (fmap noLoc $ lambdaCase) <$>
        destructMatches f (const id) (CType arg) jdg
    _ -> throwError $ GoalMismatch "destructLambdaCase'" g


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Judgement
    -> DataCon            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (Trace, LHsExpr GhcPs)
buildDataCon jdg dc apps = do
  let args = dataConInstOrigArgTys' dc apps
      dcon_name = nameOccName $ dataConName dc
  (tr, sgs)
      <- fmap unzipTrace
       $ traverse ( \(arg, n) ->
                    newSubgoal
                  . filterSameTypeFromOtherPositions dcon_name n
                  . blacklistingDestruct
                  . flip withNewGoal jdg
                  $ CType arg
                  ) $ zip args [0..]
  pure
    . (rose (show dc) $ pure tr,)
    $ mkCon dc sgs


mkCon :: DataCon -> [LHsExpr GhcPs] -> LHsExpr GhcPs
mkCon dcon (fmap unLoc -> args)
  | isTupleDataCon dcon =
      noLoc $ tuple args
  | dataConIsInfix dcon
  , (lhs : rhs : args') <- args =
      noLoc $ foldl' (@@) (op lhs (coerceName dcon_name) rhs) args'
  | otherwise =
      noLoc $ foldl' (@@) (bvar' $ occName $ dcon_name) args
  where
    dcon_name = dataConName dcon



coerceName :: HasOccName a => a -> RdrNameStr
coerceName = fromString . occNameString . occName


------------------------------------------------------------------------------
-- | Like 'var', but works over standard GHC 'OccName's.
var' :: Var a => OccName -> a
var' = var . fromString . occNameString


------------------------------------------------------------------------------
-- | Like 'bvar', but works over standard GHC 'OccName's.
bvar' :: BVar a => OccName -> a
bvar' = bvar . fromString . occNameString

