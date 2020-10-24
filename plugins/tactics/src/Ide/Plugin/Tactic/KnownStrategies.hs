{-# LANGUAGE LambdaCase #-}

module Ide.Plugin.Tactic.KnownStrategies where

import           Control.Monad (guard)
import           Control.Monad.Error.Class
import qualified Data.Map as M
import qualified Data.Text as T
import           Ide.Plugin.Tactic.Context (getMetaprogramCache, getCurrentDefinitions)
import           Ide.Plugin.Tactic.Machinery (tracing)
import           Ide.Plugin.Tactic.Tactics
import           Ide.Plugin.Tactic.Types
import           OccName (mkVarOcc)
import           Refinery.Tactic


knownMetaprograms :: MetaprogramCache -> [TacticsM ()]
knownMetaprograms (MetaprogramCache mpc) = do
  mp <- fmap snd $ M.toList mpc
  guard $ mp_known_by_auto mp
  pure $ tracing (T.unpack $ mp_name mp) $ mp_program mp


knownStrategies :: TacticsM ()
knownStrategies = do
  mpc <- getMetaprogramCache
  choice $ knownMetaprograms mpc <> [ deriveFmap ]


known :: String -> TacticsM () -> TacticsM ()
known name t = do
  getCurrentDefinitions >>= \case
    [(def, _)] | def == mkVarOcc name ->
      tracing ("known " <> name) t
    _ -> throwError NoApplicableTactic


deriveFmap :: TacticsM ()
deriveFmap = known "fmap" $ do
  try intros
  overAlgebraicTerms homo
  choice
    [ overFunctions apply >> auto' 2
    , assumption
    , recursion
    ]

