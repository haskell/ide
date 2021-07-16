{-
Forked from GHC v9.2.1 to work around the readFile side effect in mkHiefile

Main functions for .hie file generation
-}
{- HLINT ignore -}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Compat.HieAst ( mkHieFile, enrichHie ) where

import           GHC.Data.Maybe      (expectJust)
import           GHC.Driver.Env
import           GHC.Hs
import           GHC.Tc.Types        (TcGblEnv)
import           GHC.Types.Avail     (Avails)
import           GHC.Unit.Module     (ml_hs_file)
import           GHC.Unit.Module.ModSummary (ModSummary(..))

import           GHC.Iface.Ext.Ast   (enrichHie, mkHieFileWithSource)
import           GHC.Iface.Ext.Types

import qualified Data.ByteString     as BS


type RenamedSource     = ( HsGroup GhcRn, [LImportDecl GhcRn]
                         , Maybe [(LIE GhcRn, Avails)]
                         , Maybe LHsDocString )

-- | Construct an 'HieFile' from the outputs of the typechecker.
mkHieFile :: ModSummary
          -> TcGblEnv
          -> RenamedSource
          -> BS.ByteString -> Hsc HieFile
mkHieFile ms ts rs src = do
  let src_file = expectJust "mkHieFile" (ml_hs_file $ ms_location ms)
  mkHieFileWithSource src_file src ms ts rs
