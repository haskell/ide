-- We deliberately want to ensure the function we add to the rule database
-- has the constraints we need on it when we get it out.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Development.IDE.Graph.Internal.Database where

import Development.IDE.Graph.Internal.Intern
import Development.IDE.Graph.Internal.Types
import Data.Dynamic
import qualified Development.IDE.Graph.Internal.Intern as Intern
import qualified Development.IDE.Graph.Internal.Ids as Ids
import Control.Concurrent.Extra
import Data.IORef.Extra
import Control.Monad
import Development.Shake.Classes
import qualified Development.Shake as Shake
import Data.Maybe
import Control.Concurrent.Async
import System.IO.Unsafe
import Development.IDE.Graph.Internal.Rules
import qualified Development.Shake.Rule as Shake
import Control.Exception
import Control.Monad.Trans.Reader
import Data.Tuple.Extra
import Data.Either

newDatabase :: Dynamic -> TheRules -> IO Database
newDatabase databaseExtra databaseRules = do
    databaseStep <- newIORef $ Step 0
    databaseLock <- newLock
    databaseIds <- newIORef Intern.empty
    databaseValues <- Ids.empty
    pure Database{..}

incDatabase :: Database -> IO ()
incDatabase db = do
    modifyIORef' (databaseStep db) $ \(Step i) -> Step $ i + 1
    Ids.forMutate (databaseValues db) $ second $ \case
        Clean x -> Dirty (Just x)
        Dirty x -> Dirty x
        Running _ x -> Dirty x


build
    :: forall key value . (Shake.RuleResult key ~ value, Typeable key, Show key, Hashable key, Eq key, Typeable value)
    => Database -> [key] -> IO ([Id], [value])
build db keys = do
    (ids, vs) <- fmap unzip $ builder db $ map (Right . Key) keys
    pure (ids, map (asV . resultValue) vs)
    where
        asV :: Value -> value
        asV (Value x) = unwrapDynamic x

builder
    :: Database -> [Either Id Key] -> IO [(Id, Result)]
builder db@Database{..} keys = do
    -- Async things that I own and am responsible for killing
    ownedAsync <- newIORef []
    flip onException (cleanupAsync ownedAsync) $ do

        -- Things that I need to force before my results are ready
        toForce <- newIORef []

        results <- withLock databaseLock $ do
            forM keys $ \idKey -> do
                id <- case idKey of
                    Left id -> pure id
                    Right key -> do
                        ids <- readIORef databaseIds
                        case Intern.lookup key ids of
                            Just v -> pure v
                            Nothing -> do
                                (ids, id) <- pure $ Intern.add key ids
                                writeIORef' databaseIds ids
                                return id

                status <- Ids.lookup databaseValues id
                val <- case fromMaybe (fromRight undefined idKey, Dirty Nothing) status of
                    (_, Clean r) -> pure r
                    (_, Running act _) -> do
                        -- we promise to force everything in todo before reading the results
                        -- so the following unsafePerformIO isn't actually unsafe
                        let (force, val) = splitIO act
                        modifyIORef toForce (force:)
                        pure val
                    (key, Dirty s) -> do
                        -- Important we don't lose any Async things we create
                        act <- uninterruptibleMask $ \restore -> do
                            -- the child actions should always be spawned unmasked
                            -- or they can't be killed
                            async <- async $ restore $ check db key id s
                            modifyIORef ownedAsync (async:)
                            pure $ wait async
                        Ids.insert databaseValues id (key, Running act s)
                        let (force, val) = splitIO act
                        modifyIORef toForce (force:)
                        pure val

                pure (id, val)

        sequence_ =<< readIORef toForce
        pure results

cleanupAsync :: IORef [Async a] -> IO ()
cleanupAsync ref = mapConcurrently_ uninterruptibleCancel =<< readIORef ref


-- Check if we need to run the database.
check :: Database -> Key -> Id -> Maybe Result -> IO Result
check db key id result@(Just me@Result{resultDeps=Just deps}) = do
    res <- builder db $ map Left deps
    let dirty = all (\(_,dep) -> resultBuilt me < resultChanged dep) res
    let mode = if dirty then Shake.RunDependenciesChanged else Shake.RunDependenciesSame
    spawn db key id mode result
check db key id result = spawn db key id Shake.RunDependenciesChanged result


-- Spawn a new computation to run the action.
spawn :: Database -> Key -> Id -> Shake.RunMode -> Maybe Result -> IO Result
spawn db@Database{..} key id mode result = do
    let act = runRule databaseRules key (fmap resultData result) mode
    deps <- newIORef $ Just []
    Shake.RunResult{..} <- runReaderT (fromAction act) $ SAction db deps
    built <- readIORef databaseStep
    deps <- readIORef deps
    let changed = if runChanged == Shake.ChangedRecomputeDiff then built else maybe built resultChanged result
    let res = Result runValue built changed deps runStore
    withLock databaseLock $
        Ids.insert databaseValues id (key, Clean res)
    pure res

data Box a = Box {fromBox :: a}

splitIO :: IO a -> (IO (), a)
splitIO act = do
    let act2 = Box <$> act
    let res = unsafePerformIO act2
    (void $ evaluate res, fromBox res)
