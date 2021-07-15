module Ide.Plugin.CallHierarchy where

import           Development.IDE
import qualified Ide.Plugin.CallHierarchy.Internal as X
import           Ide.Types
import           Language.LSP.Types

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { Ide.Types.pluginHandlers = mkPluginHandler STextDocumentPrepareCallHierarchy X.prepareCallHierarchy
                            <> mkPluginHandler SCallHierarchyIncomingCalls X.incomingCalls
                            <> mkPluginHandler SCallHierarchyOutgoingCalls X.outgoingCalls
  }
