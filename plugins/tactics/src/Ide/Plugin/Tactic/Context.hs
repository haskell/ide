{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Ide.Plugin.Tactic.Context where

import Bag
import Control.Arrow
import Control.Monad.Reader
import Development.IDE.GHC.Compat
import Ide.Plugin.Tactic.Types
import OccName
import TcRnTypes


mkContext :: MetaprogramCache -> [(OccName, CType)] -> TcGblEnv -> Context
mkContext mpc locals tcg_env = Context
  { ctxDefiningFuncs = locals
  , ctxMetaprogramCache = mpc
  , ctxModuleFuncs
      = fmap splitId
      . (getFunBindId =<<)
      . fmap unLoc
      . bagToList
      $ tcg_binds tcg_env
  }


splitId :: Id -> (OccName, CType)
splitId = occName &&& CType . idType


getFunBindId :: HsBindLR GhcTc GhcTc -> [Id]
getFunBindId (AbsBinds _ _ _ abes _ _ _)
  = abes >>= \case
      ABE _ poly _ _ _ -> pure poly
      _ -> []
getFunBindId _ = []


getCurrentDefinitions :: MonadReader Context m => m [(OccName, CType)]
getCurrentDefinitions = asks ctxDefiningFuncs


getModuleHypothesis :: MonadReader Context m => m [(OccName, CType)]
getModuleHypothesis = asks ctxModuleFuncs


getMetaprogramCache :: MonadReader Context m => m MetaprogramCache
getMetaprogramCache = asks ctxMetaprogramCache

