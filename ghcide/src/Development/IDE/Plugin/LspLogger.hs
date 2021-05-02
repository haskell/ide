module Development.IDE.Plugin.LspLogger (lspLogger) where

import           Control.Monad.Extra          (whenJust)
import           Control.Monad.IO.Class
import qualified Data.Aeson                   as A
import           Data.IORef
import           Development.IDE.Types.Logger
import           Ide.Types
import qualified Language.LSP.Server          as LSP
import           Language.LSP.Types

-- | A logger that sends messages to the LSP client
lspLogger :: IO (Logger, PluginDescriptor a)
lspLogger = do
    lspEnvRef <- newIORef Nothing
    let plugin = (defaultPluginDescriptor "lspLogging"){
            pluginNotificationHandlers =
                mkPluginNotificationHandler SInitialized $ \_ _ _ ->
                    liftIO $ readIORef lspEnvRef >>= writeIORef lspEnvRef
        }
        logger = Logger $ \_p msg -> do
            env <- readIORef lspEnvRef
            whenJust env $ \env ->
                LSP.runLspT env (LSP.sendNotification (SCustomMethod "ghcide/log") (A.String msg))
    return (logger, plugin)
