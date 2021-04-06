{-# LANGUAGE NumericUnderscores #-}
-- | Logging utilities for reporting heap statistics
module Development.IDE.Main.HeapStats ( withHeapStats ) where

import           GHC.Stats
import           Development.IDE.Types.Logger       (Logger, logInfo)
import           Control.Concurrent.Async
import qualified Data.Text as T
import           Data.Word
import           Control.Monad
import           Control.Concurrent
import           Text.Printf                        (printf)

-- | Interval at which to report the latest heap statistics.
heapStatsInterval :: Int
heapStatsInterval = 60_000_000 -- 60s

-- | Report the live bytes and heap size at the last major collection.
logHeapStats :: Logger -> IO ()
logHeapStats l = do
  stats <- getRTSStats
  -- live_bytes is the total amount of live memory in a program
  -- (corresponding to the amount on a heap profile)
  let live_bytes = gcdetails_live_bytes (gc stats)
  -- heap_size is the total amount of memory the RTS is using
  -- this corresponds closer to OS memory usage
      heap_size  = gcdetails_mem_in_use_bytes (gc stats)
      format :: Word64 -> T.Text
      format m = T.pack (printf "%.2fMB" (fromIntegral @Word64 @Double m / 1e6))
      message = "Live bytes: " <> format live_bytes  <> " " <>
                "Heap size: " <> format heap_size
  logInfo l message

-- | An action which logs heap statistics at the 'heapStatsInterval'
heapStatsThread :: Logger -> IO r
heapStatsThread l = forever $ do
  threadDelay heapStatsInterval
  logHeapStats l

-- | A helper function which lauches the 'heapStatsThread' and kills it
-- appropiately when the inner action finishes. It also checks to see
-- if `-T` is enabled.
withHeapStats :: Logger -> IO r -> IO r
withHeapStats l k = do
  enabled <- getRTSStatsEnabled
  if enabled
    then do
      logInfo l ("Logging heap statistics every "
                  <> T.pack (printf "%.2fs" (fromIntegral @Int @Double heapStatsInterval / 1e6)))
      withAsync (heapStatsThread l) (const k)
    else do
      logInfo l "Heap statistics are not enabled (RTS option -T is needed)"
      k
