{-# LANGUAGE CPP #-}

module Wingman.StaticPlugin
  ( staticPlugin
  , metaprogramHoleName
  , pattern WingmanMetaprogram
  , pattern MetaprogramSyntax
  ) where

import Data.Data
import Development.IDE.GHC.Compat
import GHC.LanguageExtensions.Type (Extension(EmptyCase, QuasiQuotes))
import Generics.SYB
import GhcPlugins hiding ((<>))
import Ide.Types


staticPlugin :: DynFlagsModifications
staticPlugin = mempty
  { dynFlagsModifyGlobal =
      \df -> allowEmptyCaseButWithWarning
           $ flip gopt_unset Opt_SortBySubsumHoleFits
           $ df
             { refLevelHoleFits = Just 0
             , maxRefHoleFits   = Just 0
             , maxValidHoleFits = Just 0
#if __GLASGOW_HASKELL__ >= 808
             , staticPlugins = staticPlugins df <> [metaprogrammingPlugin]
#endif
             }
#if __GLASGOW_HASKELL__ >= 808
  , dynFlagsModifyParser = enableQuasiQuotes
#endif
  }


pattern MetaprogramSourceText :: SourceText
pattern MetaprogramSourceText = SourceText "wingman-meta-program"



pattern WingmanMetaprogram :: FastString -> HsExpr p
pattern WingmanMetaprogram mp
  <- HsSCC _ MetaprogramSourceText (StringLiteral NoSourceText mp)
      (L _ ( HsVar _ _))


enableQuasiQuotes :: DynFlags -> DynFlags
enableQuasiQuotes = flip xopt_set QuasiQuotes


-- | Wingman wants to support destructing of empty cases, but these are a parse
-- error by default. So we want to enable 'EmptyCase', but then that leads to
-- silent errors without 'Opt_WarnIncompletePatterns'.
allowEmptyCaseButWithWarning :: DynFlags -> DynFlags
allowEmptyCaseButWithWarning =
  flip xopt_set EmptyCase . flip wopt_set Opt_WarnIncompletePatterns


#if __GLASGOW_HASKELL__ >= 808
metaprogrammingPlugin :: StaticPlugin
metaprogrammingPlugin =
    StaticPlugin $ PluginWithArgs (defaultPlugin { parsedResultAction = worker })  []
  where
    worker :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
    worker _ _ pm = pure $ pm { hpm_module = addMetaprogrammingSyntax $ hpm_module pm }
#endif

metaprogramHoleName :: OccName
metaprogramHoleName = mkVarOcc "_$metaprogram"


mkMetaprogram :: SrcSpan -> FastString -> HsExpr GhcPs
mkMetaprogram ss mp =
  HsSCC noExtField MetaprogramSourceText (StringLiteral NoSourceText mp)
    $ L ss
    $ HsVar noExtField
    $ L ss
    $ mkRdrUnqual
    $ metaprogramHoleName


addMetaprogrammingSyntax :: Data a => a -> a
addMetaprogrammingSyntax =
  everywhere $ mkT $ \case
    L ss (MetaprogramSyntax mp) ->
      L ss $ mkMetaprogram ss mp
    (x :: LHsExpr GhcPs) -> x


pattern MetaprogramSyntax :: FastString -> HsExpr GhcPs
pattern MetaprogramSyntax mp <-
    HsSpliceE _ (HsQuasiQuote _ _ (occNameString . rdrNameOcc -> "wingman") _ mp)
  where
    MetaprogramSyntax mp =
      HsSpliceE noExtField $
        HsQuasiQuote
          noExtField
          (mkRdrUnqual $ mkVarOcc "splice")
          (mkRdrUnqual $ mkVarOcc "wingman")
          noSrcSpan
          mp

