{-# LANGUAGE RecordWildCards #-}

module Development.IDE.Graph.Internal.Options where

import Data.Dynamic
import Control.Monad.Trans.Reader
import Development.IDE.Graph.Internal.Types

data ShakeOptions = ShakeOptions {
    shakeThreads            :: Int,
    shakeFiles              :: FilePath,
    shakeExtra              :: Maybe Dynamic,
    shakeAllowRedefineRules :: Bool,
    shakeTimings            :: Bool
    }

shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions 0 ".shake" Nothing False False

getShakeExtra :: Typeable a => Action (Maybe a)
getShakeExtra = do
    extra <- Action $ asks $ databaseExtra . actionDatabase
    pure $ fromDynamic extra

getShakeExtraRules :: Typeable a => Rules (Maybe a)
getShakeExtraRules = do
    extra <- Rules $ asks rulesExtra
    pure $ fromDynamic extra

newShakeExtra :: Typeable a => a -> Maybe Dynamic
newShakeExtra = Just . toDyn
