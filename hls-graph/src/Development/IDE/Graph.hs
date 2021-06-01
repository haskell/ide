{-# LANGUAGE PatternSynonyms #-}

module Development.IDE.Graph(
    shakeOptions,
    Rules,
    Action, action,
    actionFinally, actionBracket, actionCatch,
    Shake.ShakeException(..),
    -- * Configuration
    ShakeOptions(shakeAllowRedefineRules, shakeThreads, shakeFiles, shakeExtra),
    getShakeExtra, getShakeExtraRules, newShakeExtra,
    -- * Explicit parallelism
    parallel,
    -- * Oracle rules
    Shake.ShakeValue, Shake.RuleResult,
    -- * Special rules
    alwaysRerun,
    -- * Batching
    reschedule,
    ) where

import qualified Development.Shake as Shake
import Development.IDE.Graph.Internal.Action
import Development.IDE.Graph.Internal.Options
import Development.IDE.Graph.Internal.Rules
import Development.IDE.Graph.Internal.Types
