{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins where

import           Ide.PluginUtils                   (pluginDescToIdePlugins)
import           Ide.Types                         (IdePlugins)

-- fixed plugins
import           Development.IDE                   (IdeState)
import           Development.IDE.Plugin.HLS.GhcIde as GhcIde
import           Ide.Plugin.Example                as Example
import           Ide.Plugin.Example2               as Example2

-- haskell-language-server optional plugins

#if callHierarchy
import           Ide.Plugin.CallHierarchy          as CallHierarchy
#endif

#if class
import           Ide.Plugin.Class                  as Class
#endif

#if haddockComments
import           Ide.Plugin.HaddockComments        as HaddockComments
#endif

#if eval
import           Ide.Plugin.Eval                   as Eval
#endif

#if importLens
import           Ide.Plugin.ExplicitImports        as ExplicitImports
#endif

#if refineImports
import           Ide.Plugin.RefineImports          as RefineImports
#endif

#if retrie
import           Ide.Plugin.Retrie                 as Retrie
#endif

#if tactic
import           Ide.Plugin.Tactic                 as Tactic
#endif

#if hlint
import           Ide.Plugin.Hlint                  as Hlint
#endif

#if moduleName
import           Ide.Plugin.ModuleName             as ModuleName
#endif

#if pragmas
import           Ide.Plugin.Pragmas                as Pragmas
#endif

#if splice
import           Ide.Plugin.Splice                 as Splice
#endif

-- formatters

#if floskell
import           Ide.Plugin.Floskell               as Floskell
#endif

#if fourmolu
import           Ide.Plugin.Fourmolu               as Fourmolu
#endif

#if ormolu
import           Ide.Plugin.Ormolu                 as Ormolu
#endif

#if stylishHaskell
import           Ide.Plugin.StylishHaskell         as StylishHaskell
#endif

#if brittany
import           Ide.Plugin.Brittany               as Brittany
#endif

-- ---------------------------------------------------------------------

-- | The plugins configured for use in this instance of the language
-- server.
-- These can be freely added or removed to tailor the available
-- features of the server.

idePlugins :: Bool -> IdePlugins IdeState
idePlugins includeExamples = pluginDescToIdePlugins allPlugins
  where
    allPlugins = if includeExamples
                   then basePlugins ++ examplePlugins
                   else basePlugins
    basePlugins =
#if pragmas
      Pragmas.descriptor  "pragmas" :
#endif
#if floskell
      Floskell.descriptor "floskell" :
#endif
#if fourmolu
      Fourmolu.descriptor "fourmolu" :
#endif
#if tactic
      Tactic.descriptor "tactics" :
#endif
#if ormolu
      Ormolu.descriptor   "ormolu" :
#endif
#if stylishHaskell
      StylishHaskell.descriptor "stylish-haskell" :
#endif
#if retrie
      Retrie.descriptor "retrie" :
#endif
#if brittany
      Brittany.descriptor "brittany" :
#endif
#if callHierarchy
      CallHierarchy.descriptor "callHierarchy":
#endif
#if class
      Class.descriptor "class" :
#endif
#if haddockComments
      HaddockComments.descriptor "haddockComments" :
#endif
#if eval
      Eval.descriptor "eval" :
#endif
#if importLens
      ExplicitImports.descriptor "importLens" :
#endif
#if refineImports
      RefineImports.descriptor "refineImports" :
#endif
#if moduleName
      ModuleName.descriptor "moduleName" :
#endif
#if hlint
      Hlint.descriptor "hlint" :
#endif
#if splice
      Splice.descriptor "splice" :
#endif
    -- The ghcide descriptors should come last so that the notification handlers
    -- (which restart the Shake build) run after everything else
      GhcIde.descriptors
    examplePlugins =
      [Example.descriptor  "eg"
      ,Example2.descriptor "eg2"
      ]
