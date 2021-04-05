module Wingman.Context where

import           Bag
import           Control.Arrow
import           Control.Monad.Reader
import           Data.Foldable.Extra (allM)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           GhcPlugins (ExternalPackageState (eps_inst_env), piResultTys)
import           InstEnv (lookupInstEnv, InstEnvs(..), is_dfun)
import           OccName
import           TcRnTypes
import           TcType (tcSplitTyConApp, tcSplitPhiTy)
import           TysPrim (alphaTys)
import           Wingman.FeatureSet (FeatureSet)
import           Wingman.Judgements.Theta
import           Wingman.Types


mkContext
    :: FeatureSet
    -> [(OccName, CType)]
    -> TcGblEnv
    -> ExternalPackageState
    -> KnownThings
    -> [Evidence]
    -> Context
mkContext features locals tcg eps kt ev = Context
  { ctxDefiningFuncs = locals
  , ctxModuleFuncs = fmap splitId
                   . (getFunBindId =<<)
                   . fmap unLoc
                   . bagToList
                   $ tcg_binds tcg
  , ctxFeatureSet = features
  , ctxInstEnvs =
      InstEnvs
        (eps_inst_env eps)
        (tcg_inst_env tcg)
        (tcVisibleOrphanMods tcg)
  , ctxKnownThings = kt
  , ctxTheta = evidenceToThetaType ev
  }


splitId :: Id -> (OccName, CType)
splitId = occName &&& CType . idType


getFunBindId :: HsBindLR GhcTc GhcTc -> [Id]
getFunBindId (AbsBinds _ _ _ abes _ _ _)
  = abes >>= \case
      ABE _ poly _ _ _ -> pure poly
      _                -> []
getFunBindId _ = []


getCurrentDefinitions :: MonadReader Context m => m [(OccName, CType)]
getCurrentDefinitions = asks ctxDefiningFuncs


getKnownThing :: MonadReader Context m => (KnownThings -> a) -> m a
getKnownThing f = asks $ f . ctxKnownThings


getKnownInstance :: MonadReader Context m => (KnownThings -> Class) -> [Type] -> m (Maybe (Class, PredType))
getKnownInstance f tys = do
  cls <- getKnownThing f
  getInstance cls tys


getInstance :: MonadReader Context m => Class -> [Type] -> m (Maybe (Class, PredType))
getInstance cls tys = do
  env <- asks ctxInstEnvs
  let (mres, _, _) = lookupInstEnv False env cls tys
  case mres of
    ((inst, mapps) : _) -> do
      -- Get the instantiated type of the dictionary
      let df = piResultTys (idType $ is_dfun inst) $ zipWith fromMaybe alphaTys mapps
      -- pull off its resulting arguments
      let (theta, df') = tcSplitPhiTy df
      traceMX "looking for instances" theta
      allM hasClassInstance theta >>= \case
        True -> pure $ do
          traceMX "solved instance for" df'
          Just (cls, df')
        False -> pure Nothing
    _ -> pure Nothing


hasClassInstance :: MonadReader Context m => PredType -> m Bool
hasClassInstance predty = do
  theta <- asks ctxTheta
  case S.member (CType predty) theta of
    True -> do
      traceMX "cached instance for " predty
      pure True
    False -> do
      let (con, apps) = tcSplitTyConApp predty
      case tyConClass_maybe con of
        Nothing -> pure False
        Just cls -> fmap isJust $ getInstance cls apps

