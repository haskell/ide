

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Development.IDE.Graph.Internal.Types where

import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.Typeable
import Data.Dynamic
import Control.Monad.Fail
import Control.Monad.IO.Class
import Development.IDE.Graph.Internal.Ids
import Control.Concurrent.Extra
import Development.IDE.Graph.Internal.Intern
import Control.Applicative
import Development.Shake.Classes
import qualified Data.ByteString as BS
import Data.Maybe


unwrapDynamic :: forall a . Typeable a => Dynamic -> a
unwrapDynamic x = fromMaybe (error msg) $ fromDynamic x
    where msg = "unwrapDynamic failed: Expected " ++ show (typeRep (Proxy :: Proxy a)) ++
                ", but got " ++ show (dynTypeRep x)

---------------------------------------------------------------------
-- RULES

type TheRules = Map.HashMap TypeRep Dynamic

newtype Rules a = Rules (ReaderT SRules IO a)
    deriving (Monad, Applicative, Functor, MonadIO, MonadFail)

data SRules = SRules {
    rulesExtra :: !Dynamic,
    rulesActions :: !(IORef [Action ()]),
    rulesMap :: !(IORef TheRules)
    }


---------------------------------------------------------------------
-- ACTIONS

newtype Action a = Action {fromAction :: ReaderT SAction IO a}
    deriving (Monad, Applicative, Functor, MonadIO, MonadFail)

data SAction = SAction {
    actionDatabase :: !Database,
    actionDeps :: !(IORef (Maybe [Id])) -- Nothing means always rerun
    }


---------------------------------------------------------------------
-- DATABASE

newtype Step = Step Int
    deriving (Eq,Ord)

data Key = forall a . (Typeable a, Eq a, Hashable a, Show a) => Key a

instance Eq Key where
    Key a == Key b = Just a == cast b

instance Hashable Key where
    hashWithSalt i (Key x) = hashWithSalt i (typeOf x, x)

instance Show Key where
    show (Key x) = show x

newtype Value = Value Dynamic

data Database = Database {
    databaseExtra :: Dynamic,
    databaseRules :: TheRules,
    databaseStep :: !(IORef Step),
    -- Hold the lock while mutating Ids/Values
    databaseLock :: !Lock,
    databaseIds :: !(IORef (Intern Key)),
    databaseValues :: !(Ids (Key, Status))
    }

data Status
    = Clean Result
    | Dirty (Maybe Result)
    | Running (IO Result) (Maybe Result)

data Result = Result {
    resultValue :: !Value,
    resultBuilt :: !Step,
    resultChanged :: !Step,
    resultDeps :: !(Maybe [Id]), -- Nothing = alwaysRerun
    resultData :: BS.ByteString
    }


---------------------------------------------------------------------
-- INSTANCES

instance Semigroup a => Semigroup (Rules a) where
    a <> b = liftA2 (<>) a b

instance Monoid a => Monoid (Rules a) where
    mempty = pure mempty
