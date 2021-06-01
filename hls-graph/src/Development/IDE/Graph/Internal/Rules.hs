-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Development.IDE.Graph.Internal.Rules where

import           Control.Monad.Fail
import           Control.Monad.IO.Class
import qualified Data.ByteString                       as BS
import           Development.IDE.Graph.Internal.Action
import qualified Development.Shake                     as Shake
import           Development.Shake.Classes
import qualified Development.Shake.Rule                as Shake

newtype Rules a = Rules {fromRules :: Shake.Rules a}
    deriving (Monoid, Semigroup, Monad, Applicative, Functor, MonadIO, MonadFail)

action :: Action a -> Rules ()
action x = do
    ref <- Rules $ asks rulesActions
    liftIO $ modifyIORef' ref (void x:)

addRule
    :: forall key value .
       (Shake.RuleResult key ~ value, Typeable key, Hashable key, Eq key, Typeable value)
    => (key -> Maybe BS.ByteString -> Shake.RunMode -> Action (Shake.RunResult value))
    -> Rules ()
addRule f = do
    ref <- Rules $ asks rulesMap
    liftIO $ modifyIORef' ref $ Map.insert (typeRep (Proxy :: Proxy key)) (toDyn f2)
    where
        f2 :: Key -> Maybe BS.ByteString -> Shake.RunMode -> Action (Shake.RunResult Value)
        f2 (Key a) b c = do
            v <- f (fromJust $ cast a :: key) b c
            v <- liftIO $ evaluate v
            pure $ (Value . toDyn) <$> v

runRule
    :: TheRules -> Key -> Maybe BS.ByteString -> Shake.RunMode -> Action (Shake.RunResult Value)
runRule rules key@(Key t) bs mode = case Map.lookup (typeOf t) rules of
    Nothing -> liftIO $ errorIO "Could not find key"
    Just x -> unwrapDynamic x key bs mode

runRules :: Dynamic -> Rules () -> IO (TheRules, [Action ()])
runRules rulesExtra (Rules rules) = do
    rulesActions <- newIORef []
    rulesMap <- newIORef Map.empty
    runReaderT rules SRules{..}
    (,) <$> readIORef rulesMap <*> readIORef rulesActions
