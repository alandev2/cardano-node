{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Handlers.Shutdown
  (
  -- * Generalised shutdown handling
    withShutdownHandling

  -- * Watch ChainDB for passing a configured slot sync limit threshold,
  --   translating it to a graceful shutdown.
  , maybeSpawnOnSlotSyncedShutdownHandler
  )
where

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race_)
import           Control.Exception
import           Control.Monad
import           Data.Text (Text, pack)
import qualified GHC.IO.Handle.FD as IO (fdToHandle)
import           System.Exit
import qualified System.IO as IO
import qualified System.IO.Error as IO
import           System.Posix.Types (Fd (Fd))

import           Cardano.BM.Data.Tracer (TracingVerbosity (..), severityNotice, trTransformer)
import           Cardano.BM.Trace
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.Tracer
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)
import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo, pointSlot)

import           Cardano.Node.Configuration.POM (NodeConfiguration (..))

-- | We provide an optional cross-platform method to politely request shut down.
-- The parent process passes us the file descriptor number of the read end of a pipe,
-- via the CLI with @--shutdown-ipc FD@
withShutdownHandling
  :: Maybe Fd
  -> Trace IO Text
  -> IO ()
  -- ^ Action to potentially shutdown via file descriptor
  -> IO ()
withShutdownHandling Nothing trace action = do
  traceWith tracer "withShutdownHandling: NO FD WAS PASSED *************"
  action
 where
   tracer :: Tracer IO Text
   tracer = trTransformer MaximalVerbosity (severityNotice trace)
withShutdownHandling (Just fileDescriptor) trace action = do
  traceWith tracer "withShutdownHandling: WE ARE HERE *************"
  race_ (waitForEOF fileDescriptor) action
 where
   tracer :: Tracer IO Text
   tracer = trTransformer MaximalVerbosity (severityNotice trace)

   loop :: IO.Handle -> IO ()
   loop h = do
     b <- IO.hIsClosed h
     if b
     then traceWith tracer "isEOFError: Handle is closed!"
     else do traceWith tracer "isEOFError: Hande still open"
             threadDelay 1000000
             loop h

   waitForEOF :: Fd -> IO ()
   waitForEOF (Fd fd) = do
     traceWith tracer "waitForEOF: Before fdToHandle *************"
     hnd <- IO.fdToHandle fd
     traceWith tracer "waitForEOF: After fdToHandle *************"
     r <- try $ IO.hGetChar hnd
     traceWith tracer "waitForEOF: After hGetChar *************"
     case r of
       Left e
         | IO.isEOFError e -> do
             IO.hClose hnd
             loop hnd
             traceWith tracer "Received shutdown request and shutting node down...throwIO,io-manager-native, wrap hgetchar, is handle closed"
         | otherwise -> do
             traceWith tracer "Received shutdown request but did not encounter EOL in --shutdown-ipc FD"
             throwIO e
       Right inp  -> do
         traceWith tracer
           $ "Received shutdown request but found unexpected input in --shutdown-ipc FD: " <> pack (show inp)

-- | Windows blocking file IO calls like 'hGetChar' are not interruptable by
-- asynchronous exceptions, as used by async 'cancel' (as of base-4.12).
--
-- This wrapper works around that problem by running the blocking IO in a
-- separate thread. If the parent thread receives an async cancel then it
-- will return. Note however that in this circumstance the child thread may
-- continue and remain blocked, leading to a leak of the thread. As such this
-- is only reasonable to use a fixed number of times for the whole process.
--
--wrapUninterruptableIO :: IO a -> IO a
--wrapUninterruptableIO action = async action >>= wait

-- | Spawn a thread that would cause node to shutdown upon ChainDB reaching the
-- configuration-defined slot.
maybeSpawnOnSlotSyncedShutdownHandler
  :: NodeConfiguration
  -> Trace IO Text
  -> ResourceRegistry IO
  -> ChainDB.ChainDB IO blk
  -> IO ()
maybeSpawnOnSlotSyncedShutdownHandler nc trace registry chaindb =
  case ncShutdownOnSlotSynced nc of
    NoMaxSlotNo -> return ()
    MaxSlotNo maxSlot -> do
      traceWith (trTransformer MaximalVerbosity $ severityNotice trace)
        ("will terminate upon reaching " <> pack (show maxSlot))
      spawnSlotLimitTerminator maxSlot
 where
  spawnSlotLimitTerminator :: SlotNo -> IO ()
  spawnSlotLimitTerminator maxSlot =
    void $ forkLinkedWatcher registry "slotLimitTerminator" Watcher {
        wFingerprint = id
      , wInitial     = Nothing
      , wNotify      = \case
          Origin -> pure ()
          At cur -> when (cur >= maxSlot) $ do
            traceWith (trTransformer MaximalVerbosity $ severityNotice trace)
              (("spawnSlotLimitTerminator: reached target " :: String) <> show cur)
            throwIO ExitSuccess
      , wReader      = pointSlot <$> ChainDB.getTipPoint chaindb
      }
