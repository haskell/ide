{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Development.IDE.Graph.Internal.Action where

import qualified Development.Shake as Shake
import Development.Shake.Classes
import Control.Exception
import Control.Concurrent.Async
import System.Exit
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Development.IDE.Graph.Internal.Database
import Development.IDE.Graph.Internal.Types
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Concurrent.Async


alwaysRerun :: Action ()
alwaysRerun = do
    ref <- Action $ asks actionDeps
    liftIO $ writeIORef ref Nothing

-- No-op for now
reschedule :: Double -> Action ()
reschedule _ = pure ()

parallel :: [Action a] -> Action [a]
parallel [] = pure []
parallel [x] = fmap (:[]) x
parallel xs = do
    a <- Action ask
    deps <- liftIO $ readIORef $ actionDeps a
    case deps of
        Nothing ->
            -- if we are already in the rerun mode, nothing we do is going to impact our state
            liftIO $ mapConcurrently (ignoreState a) xs
        Just deps -> do
            (newDeps, res) <- liftIO $ unzip <$> mapConcurrently (usingState a) xs
            liftIO $ writeIORef (actionDeps a) $ (deps ++) <$> concatMapM id newDeps
            pure res
    where
        ignoreState a x = do
            ref <- newIORef Nothing
            runReaderT (fromAction x) a{actionDeps=ref}

        usingState a x = do
            ref <- newIORef $ Just []
            res <- runReaderT (fromAction x) a{actionDeps=ref}
            deps <- readIORef ref
            pure (deps, res)

isAsyncException :: SomeException -> Bool
isAsyncException e
    | Just (_ :: AsyncCancelled) <- fromException e = True
    | Just (_ :: AsyncException) <- fromException e = True
    | Just (_ :: ExitCode) <- fromException e = True
    | otherwise = False


actionCatch :: Exception e => Action a -> (e -> Action a) -> Action a
actionCatch a b = do
    v <- Action ask
    Action $ lift $ catchJust f (runReaderT (fromAction a) v) (\x -> runReaderT (fromAction (b x)) v)
    where
        -- Catch only catches exceptions that were caused by this code, not those that
        -- are a result of program termination
        f e | isAsyncException e = Nothing
            | otherwise = fromException e

actionBracket :: IO a -> (a -> IO b) -> (a -> Action c) -> Action c
actionBracket a b c = do
    v <- Action ask
    Action $ lift $ bracket a b (\x -> runReaderT (fromAction (c x)) v)

actionFinally :: Action a -> IO b -> Action a
actionFinally a b = do
    v <- Action ask
    Action $ lift $ finally (runReaderT (fromAction a) v) b

apply1 :: (Shake.RuleResult key ~ value, Shake.ShakeValue key, Typeable value) => key -> Action value
apply1 k = head <$> apply [k]

apply :: (Shake.RuleResult key ~ value, Shake.ShakeValue key, Typeable value) => [key] -> Action [value]
apply ks = do
    db <- Action $ asks actionDatabase
    (is, vs) <- liftIO $ build db ks
    ref <- Action $ asks actionDeps
    deps <- liftIO $ readIORef ref
    whenJust deps $ \deps ->
        liftIO $ writeIORef ref $ Just $ is ++ deps
    pure vs

runActions :: Database -> [Action a] -> IO [a]
runActions db xs = do
    deps <- newIORef Nothing
    runReaderT (fromAction $ parallel xs) $ SAction db deps
