{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Ide.Plugin.Tactic.Types
  ( module Ide.Plugin.Tactic.Types
  , module Ide.Plugin.Tactic.Debug
  , OccName
  , Name
  , Type
  , TyVar
  , Span
  , Range
  ) where

import           Control.DeepSeq
import           Control.Lens hiding (Context)
import           Control.Monad.Reader
import           Data.Coerce
import           Data.Function
import           Data.Generics.Product (field)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tree
import           Development.IDE.GHC.Compat hiding (Node)
import           Development.IDE.Types.Location
import           GHC.Generics
import           Ide.Plugin.Tactic.Debug
import           OccName
import           Refinery.Tactic
import           Type


------------------------------------------------------------------------------
-- | A wrapper around 'Type' which supports equality and ordering.
newtype CType = CType { unCType :: Type }

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType

instance Show CType where
  show  = unsafeRender . unCType

instance Show OccName where
  show  = unsafeRender

instance Show Var where
  show  = unsafeRender

instance Show TCvSubst where
  show  = unsafeRender

instance Show (LHsExpr GhcPs) where
  show  = unsafeRender

instance Show DataCon where
  show  = unsafeRender


------------------------------------------------------------------------------
data TacticState = TacticState
    { ts_skolems   :: !([TyVar])
    , ts_unifier   :: !(TCvSubst)
    , ts_used_vals :: !(Set OccName)
    , ts_intro_vals :: !(Set OccName)
    , ts_recursion_stack :: ![Bool]
    } deriving stock (Show, Generic)


defaultTacticState :: TacticState
defaultTacticState =
  TacticState mempty emptyTCvSubst mempty mempty mempty


withRecursionStack
  :: ([Bool] -> [Bool]) -> TacticState -> TacticState
withRecursionStack f =
  field @"ts_recursion_stack" %~ f


withUsedVals :: (Set OccName -> Set OccName) -> TacticState -> TacticState
withUsedVals f =
  field @"ts_used_vals" %~ f


withIntroducedVals :: (Set OccName -> Set OccName) -> TacticState -> TacticState
withIntroducedVals f =
  field @"ts_intro_vals" %~ f



------------------------------------------------------------------------------
-- | The current bindings and goal for a hole to be filled by refinery.
data Judgement' a = Judgement
  { _jHypothesis :: !(Map OccName a)
  , _jDestructed :: !(Set OccName)
    -- ^ These should align with keys of _jHypothesis
  , _jPatternVals :: !(Set OccName)
    -- ^ These should align with keys of _jHypothesis
  , _jBlacklistDestruct :: !(Bool)
  , _jWhitelistSplit :: !(Bool)
  , _jPositionMaps :: !(Map OccName [[OccName]])
  , _jAncestry     :: !(Map OccName (Set OccName))
  , _jIsTopHole    :: !Bool
  , _jGoal         :: !(a)
  }
  deriving stock (Eq, Ord, Generic, Functor, Show)

type Judgement = Judgement' CType


newtype ExtractM a = ExtractM { unExtractM :: Reader Context a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Context)

------------------------------------------------------------------------------
-- | Orphan instance for producing holes when attempting to solve tactics.
instance MonadExtract (Trace, LHsExpr GhcPs) ExtractM where
  hole = pure (mempty, noLoc $ HsVar noExtField $ noLoc $ Unqual $ mkVarOcc "_")


------------------------------------------------------------------------------
-- | Reasons a tactic might fail.
data TacticError
  = UndefinedHypothesis OccName
  | GoalMismatch String CType
  | UnsolvedSubgoals [Judgement]
  | UnificationError CType CType
  | NoProgress
  | NoApplicableTactic
  | AlreadyDestructed OccName
  | IncorrectDataCon DataCon
  | RecursionOnWrongParam OccName Int OccName
  | UnhelpfulDestruct OccName
  | UnhelpfulSplit OccName
  deriving stock (Eq)

instance Show TacticError where
    show (UndefinedHypothesis name) =
      occNameString name <> " is not available in the hypothesis."
    show (GoalMismatch tac (CType typ)) =
      mconcat
        [ "The tactic "
        , tac
        , " doesn't apply to goal type "
        , unsafeRender typ
        ]
    show (UnsolvedSubgoals _) =
      "There were unsolved subgoals"
    show (UnificationError (CType t1) (CType t2)) =
        mconcat
          [ "Could not unify "
          , unsafeRender t1
          , " and "
          , unsafeRender t2
          ]
    show NoProgress =
      "Unable to make progress"
    show NoApplicableTactic =
      "No tactic could be applied"
    show (AlreadyDestructed name) =
      "Already destructed " <> unsafeRender name
    show (IncorrectDataCon dcon) =
      "Data con doesn't align with goal type (" <> unsafeRender dcon <> ")"
    show (RecursionOnWrongParam call p arg) =
      "Recursion on wrong param (" <> show call <> ") on arg"
        <> show p <> ": " <> show arg
    show (UnhelpfulDestruct n) =
      "Destructing patval " <> show n <> " leads to no new types"
    show (UnhelpfulSplit n) =
      "Splitting constructor " <> show n <> " leads to no new goals"


------------------------------------------------------------------------------
type TacticsM  = TacticT Judgement (Trace, LHsExpr GhcPs) TacticError TacticState ExtractM
type RuleM     = RuleT Judgement (Trace, LHsExpr GhcPs) TacticError TacticState ExtractM
type Rule      = RuleM (Trace, LHsExpr GhcPs)

type Trace = Rose String


------------------------------------------------------------------------------
-- | The Reader context of tactics and rules
data Context = Context
  { ctxDefiningFuncs :: ![(OccName, CType)]
    -- ^ The functions currently being defined
  , ctxModuleFuncs :: ![(OccName, CType)]
    -- ^ Everything defined in the current module
  , ctxMetaprogramCache :: !MetaprogramCache
  }


emptyContext :: Context
emptyContext = Context mempty mempty mempty


newtype Rose a = Rose (Tree a)
  deriving stock (Eq, Functor, Generic)

instance Show (Rose String) where
  show = unlines . dropEveryOther . lines . drawTree . coerce

dropEveryOther :: [a] -> [a]
dropEveryOther []           = []
dropEveryOther [a]          = [a]
dropEveryOther (a : _ : as) = a : dropEveryOther as

instance Semigroup a => Semigroup (Rose a) where
  Rose (Node a as) <> Rose (Node b bs) = Rose $ Node (a <> b) (as <> bs)

instance Monoid a => Monoid (Rose a) where
  mempty = Rose $ Node mempty mempty

rose :: (Eq a, Monoid a) => a -> [Rose a] -> Rose a
rose a [Rose (Node a' rs)] | a' == mempty = Rose $ Node a rs
rose a rs = Rose $ Node a $ coerce rs


------------------------------------------------------------------------------
data Metaprogram = Metaprogram
  { mp_name             :: !Text
  , mp_known_by_auto    :: !Bool
  , mp_show_code_action :: !Bool
  , mp_program          :: !(TacticsM ())
  }
  deriving stock Generic

emptyMetaprogram :: Metaprogram
emptyMetaprogram = Metaprogram "" False False (pure ())

instance NFData Metaprogram where
  rnf (!(Metaprogram !_ !_ !_ !_)) = ()


instance Show Metaprogram where
  show = T.unpack . mp_name


newtype MetaprogramCache = MetaprogramCache
  { unMetaprogramCache :: Map Text Metaprogram
  }
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NFData)

-- | The results of 'Ide.Plugin.Tactic.Machinery.runTactic'
data RunTacticResults = RunTacticResults
  { rtr_trace       :: Trace
  , rtr_extract     :: LHsExpr GhcPs
  , rtr_other_solns :: [(Trace, LHsExpr GhcPs)]
  } deriving Show

