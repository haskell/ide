{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.CallHierarchy.Query where

import           Database.SQLite.Simple
import           GHC
import           HieDb                          (HieDb (getConn), Symbol (..),
                                                 toNsChar)
import qualified HieDb
import           Ide.Plugin.CallHierarchy.Types
import           Module
import           Name

incomingCalls :: HieDb -> Symbol -> IO [Vertex]
incomingCalls (getConn -> conn) symbol = do
  let (o, m, u) = parseSymbol symbol
  query conn "SELECT mods.mod, defs.occ, mods.hs_src, defs.sl, defs.sc, \
              \defs.el, defs.ec, refs.sl, refs.sc, refs.el, refs.ec \
              \FROM refs \
              \JOIN decls ON decls.hieFile = refs.hieFile \
              \JOIN defs ON defs.hieFile = decls.hieFile AND defs.occ = decls.occ \
              \JOIN mods ON mods.hieFile = decls.hieFile \
              \where \
              \(refs.occ = ? AND refs.mod = ? AND refs.unit = ?) \
              \AND \
              \(decls.occ != ? OR mods.mod != ? OR mods.unit != ?) \
              \AND \
              \((refs.sl = decls.sl AND refs.sc > decls.sc) OR (refs.sl > decls.sl)) \
              \AND \
              \((refs.el = decls.el AND refs.ec <= decls.ec) OR (refs.el < decls.el))" (o, m, u, o, m, u)

outgoingCalls :: HieDb -> Symbol -> IO [Vertex]
outgoingCalls (getConn -> conn) symbol = do
  let (o, m, u) = parseSymbol symbol
  query conn "SELECT rm.mod, defs.occ, rm.hs_src, defs.sl, defs.sc, defs.el, defs.ec, \
            \refs.sl, refs.sc, refs.el, refs.ec \
            \from refs \
            \JOIN defs ON defs.occ = refs.occ \
            \JOIN decls rd ON rd.hieFile = defs.hieFile AND rd.occ = defs.occ \
            \JOIN mods rm ON rm.mod = refs.mod AND rm.unit = refs.unit AND rm.hieFile = defs.hieFile \
            \JOIN decls ON decls.hieFile = refs.hieFile \
            \JOIN mods ON mods.hieFile = decls.hieFile \
            \where \
            \(decls.occ = ? AND mods.mod = ? AND mods.unit = ?)  \
            \AND \
            \(defs.occ != ? OR rm.mod != ? OR rm.unit != ?) \
            \AND \
            \((refs.sl = decls.sl AND refs.sc >  decls.sc) OR (refs.sl > decls.sl)) \
            \AND \
            \((refs.el = decls.el AND refs.ec <= decls.ec) OR (refs.el < decls.el))" (o, m, u, o, m, u)

parseSymbol :: Symbol -> (String, String, String)
parseSymbol Symbol{..} =
  let o = toNsChar (occNameSpace symName) : occNameString symName
      m = moduleNameString $ moduleName symModule
      u = unitIdString $ moduleUnitId symModule
  in  (o, m, u)
