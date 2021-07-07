module Compat.HieDebug
  ( module GHC.Iface.Ext.Debug
  , ppHie ) where
import GHC.Iface.Ext.Debug

import GHC.Iface.Ext.Types (HieAST)
import GHC.Utils.Outputable (Outputable(ppr), SDoc)

ppHie :: Outputable a => HieAST a -> SDoc
ppHie = ppr
