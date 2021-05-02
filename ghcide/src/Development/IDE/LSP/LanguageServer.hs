      -- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

-- WARNING: A copy of DA.Daml.LanguageServer, try to keep them in sync
-- This version removes the daml: handling
module Development.IDE.LSP.LanguageServer
    ( runLanguageServer
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson                            (Value)
import           Data.Maybe
import qualified Data.Set                              as Set
import qualified Data.Text                             as T
import           Development.IDE.LSP.Server
import           Development.IDE.Session               (runWithDb)
import           Ide.Types                             (traceWithSpan)
import qualified Language.LSP.Server                   as LSP
import           Language.LSP.Types
import           System.IO
import           UnliftIO.Async
import           UnliftIO.Concurrent
import           UnliftIO.Directory
import           UnliftIO.Exception

import           Development.IDE.Core.FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Shake
import           Development.IDE.Core.Tracing
import           Development.IDE.LSP.HoverDefinition
import           Development.IDE.Types.Logger

import           System.IO.Unsafe                      (unsafeInterleaveIO)

runLanguageServer
    :: forall config. (Show config)
    => LSP.Options
    -> Handle -- input
    -> Handle -- output
    -> (FilePath -> IO FilePath) -- ^ Map root paths to the location of the hiedb for the project
    -> config
    -> (config -> Value -> Either T.Text config)
    -> LSP.Handlers (ServerM config)
    -> (LSP.LanguageContextEnv config -> VFSHandle -> Maybe FilePath -> HieDb -> IndexQueue -> IO IdeState)
    -> IO ()
runLanguageServer options inH outH getHieDbLoc defaultConfig onConfigurationChange userHandlers getIdeState = do

    -- This MVar becomes full when the server thread exits or we receive exit message from client.
    -- LSP loop will be canceled when it's full.
    clientMsgVar <- newEmptyMVar
    -- Forcefully exit
    let exit = void $ tryPutMVar clientMsgVar ()

    -- The set of requests ids that we have received but not finished processing
    pendingRequests <- newTVarIO Set.empty
    -- The set of requests that have been cancelled and are also in pendingRequests
    cancelledRequests <- newTVarIO Set.empty

    let cancelRequest reqId = atomically $ do
            queued <- readTVar pendingRequests
            -- We want to avoid that the list of cancelled requests
            -- keeps growing if we receive cancellations for requests
            -- that do not exist or have already been processed.
            when (reqId `elem` queued) $
                modifyTVar cancelledRequests (Set.insert reqId)
    let clearReqId reqId = atomically $ do
            modifyTVar pendingRequests (Set.delete reqId)
            modifyTVar cancelledRequests (Set.delete reqId)
        -- We implement request cancellation by racing waitForCancel against
        -- the actual request handler.
    let waitForCancel reqId = atomically $ do
            cancelled <- readTVar cancelledRequests
            unless (reqId `Set.member` cancelled) retry

    let ideHandlers = mconcat
          [ setIdeHandlers
          , userHandlers
          ]

    -- Send everything over a channel, since you need to wait until after initialise before
    -- LspFuncs is available
    clientMsgChan :: Chan ReactorMessage <- newChan

    let asyncHandlers = mconcat
          [ ideHandlers
          , cancelHandler cancelRequest
          , exitHandler exit
          ]
          -- Cancel requests are special since they need to be handled
          -- out of order to be useful. Existing handlers are run afterwards.


    let serverDefinition = LSP.ServerDefinition
            { LSP.onConfigurationChange = onConfigurationChange
            , LSP.defaultConfig = defaultConfig
            , LSP.doInitialize = handleInit exit clearReqId waitForCancel clientMsgChan
            , LSP.staticHandlers = asyncHandlers
            , LSP.interpretHandler = \(env, st) -> LSP.Iso (LSP.runLspT env . flip runReaderT (clientMsgChan,st)) liftIO
            , LSP.options = modifyOptions options
            }

    void $ waitAnyCancel =<< traverse async
        [ void $ LSP.runServerWithHandles
            inH
            outH
            serverDefinition
        , void $ readMVar clientMsgVar
        ]

    where
        handleInit
          :: IO () -> (SomeLspId -> IO ()) -> (SomeLspId -> IO ()) -> Chan ReactorMessage
          -> LSP.LanguageContextEnv config -> RequestMessage Initialize -> IO (Either err (LSP.LanguageContextEnv config, IdeState))
        handleInit exitClientMsg clearReqId waitForCancel clientMsgChan env (RequestMessage _ _ m params) = otTracedHandler "Initialize" (show m) $ \sp -> do
            traceWithSpan sp params
            let root = LSP.resRootPath env

            dir <- getCurrentDirectory
            dbLoc <- getHieDbLoc dir

            -- The database needs to be open for the duration of the reactor thread, but we need to pass in a reference
            -- to 'getIdeState', so we use this dirty trick
            dbMVar <- newEmptyMVar
            ~(hiedb,hieChan) <- unsafeInterleaveIO $ takeMVar dbMVar

            ide <- getIdeState env (makeLSPVFSHandle env) root hiedb hieChan

            let initConfig = parseConfiguration params
                l = ideLogger ide
            logInfo l $ T.pack $ "Registering ide configuration: " <> show initConfig
            registerIdeConfiguration (shakeExtras ide) initConfig

            let handleServerException (Left e) = do
                    logError (ideLogger ide) $
                        T.pack $ "Fatal error in server thread: " <> show e
                    exitClientMsg
                    throwIO e
                handleServerException _ = pure ()
            _ <- flip forkFinally handleServerException $ runWithDb l dbLoc $ \hiedb hieChan -> do
              putMVar dbMVar (hiedb,hieChan)
              forever $ do
                msg <- readChan clientMsgChan
                -- We dispatch notifications synchronously and requests asynchronously
                -- This is to ensure that all file edits and config changes are applied before a request is handled
                case msg of
                    ReactorNotification act -> do
                      catch act $ \(e :: SomeException) ->
                        logError (ideLogger ide) $ T.pack $
                          "Unexpected exception on notification, please report!\n" ++
                          "Exception: " ++ show e
                    ReactorRequest _id act k -> void $ async $
                      checkCancelled ide clearReqId waitForCancel _id act k
            pure $ Right (env,ide)

        checkCancelled
          :: IdeState -> (SomeLspId -> IO ()) -> (SomeLspId -> IO ()) -> SomeLspId
          -> IO () -> (ResponseError -> IO ()) -> IO ()
        checkCancelled ide clearReqId waitForCancel _id act k =
            flip finally (clearReqId _id) $
                catch (do
                    -- We could optimize this by first checking if the id
                    -- is in the cancelled set. However, this is unlikely to be a
                    -- bottleneck and the additional check might hide
                    -- issues with async exceptions that need to be fixed.
                    cancelOrRes <- race (waitForCancel _id) act
                    case cancelOrRes of
                        Left () -> do
                            logDebug (ideLogger ide) $ T.pack $ "Cancelled request " <> show _id
                            k $ ResponseError RequestCancelled "" Nothing
                        Right res -> pure res
                ) $ \(e :: SomeException) -> do
                    logError (ideLogger ide) $ T.pack $
                        "Unexpected exception on request, please report!\n" ++
                        "Exception: " ++ show e
                    k $ ResponseError InternalError (T.pack $ show e) Nothing


cancelHandler :: (SomeLspId -> IO ()) -> LSP.Handlers (ServerM c)
cancelHandler cancelRequest = LSP.notificationHandler SCancelRequest $ \NotificationMessage{_params=CancelParams{_id}} ->
  liftIO $ cancelRequest (SomeLspId _id)

exitHandler :: IO () -> LSP.Handlers (ServerM c)
exitHandler exit = LSP.notificationHandler SExit $ const $ do
    (_, ide) <- ask
    liftIO $ logDebug (ideLogger ide) "Received exit message"
    -- flush out the Shake session to record a Shake profile if applicable
    liftIO $ shakeShut ide
    liftIO exit

modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x = x{ LSP.textDocumentSync   = Just $ tweakTDS origTDS
                   }
    where
        tweakTDS tds = tds{_openClose=Just True, _change=Just TdSyncIncremental, _save=Just $ InR $ SaveOptions Nothing}
        origTDS = fromMaybe tdsDefault $ LSP.textDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing

