{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Sync (makeSync, syncWithSender) where

import Control.Concurrent
import Network.HTTP.Semantics.IO
import UnliftIO.STM

import Network.HTTP2.H2.Context
import Network.HTTP2.H2.Queue
import Network.HTTP2.H2.Types
import Network.HTTP2.H2.Window
import Debug.Trace (traceM)

syncWithSender
    :: Context
    -> Stream
    -> OutputType
    -> Maybe (TBQueue StreamingChunk)
    -> IO ()
syncWithSender Context{..} strm otyp mtbq = do
    traceM "in syncWithSender"
    var <- newEmptyMVar
    let sync = makeSync strm mtbq (\i -> traceM ("putting in sync: " ++ show i) >> putMVar var i >> traceM "put in sync")
    traceM "enqueueing output"
    enqueueOutput outputQ $ Output strm otyp sync
    traceM "looping"
    loop var sync
  where
    loop var sync = do
        traceM "in loop, taking"
        s <- takeMVar var
        traceM "took"
        case s of
            Done -> traceM "done"
            Cont wait newotyp -> do
                traceM "Cont, waiting"
                wait
                traceM "enqueueing"
                enqueueOutput outputQ $ Output strm newotyp sync
                traceM "looping again"
                loop var sync

makeSync
    :: Stream
    -> Maybe (TBQueue StreamingChunk)
    -> (Sync -> IO ())
    -> Maybe OutputType
    -> IO Bool
makeSync _ _ sync Nothing = traceM "sync: No OutputType" >> sync Done >> return False
makeSync strm mtbq sync (Just otyp) = do
    traceM "sync: checkOpen"
    mwait <- checkOpen strm mtbq
    case mwait of
        Nothing -> traceM "sync: no wait" >> return True
        Just wait -> do
            traceM "sync: wait"
            sync $ Cont wait otyp
            traceM "returning"
            return False

checkOpen :: Stream -> Maybe (TBQueue StreamingChunk) -> IO (Maybe (IO ()))
checkOpen strm mtbq = case mtbq of
    Nothing -> checkStreamWindowSize
    Just tbq -> checkStreaming tbq
  where
    checkStreaming tbq = do
        isEmpty <- atomically $ isEmptyTBQueue tbq
        if isEmpty
            then do
                return $ Just (waitStreaming tbq)
            else checkStreamWindowSize
    -- FLOW CONTROL: WINDOW_UPDATE: send: respecting peer's limit
    checkStreamWindowSize = do
        sws <- getStreamWindowSize strm
        if sws <= 0
            then return $ Just (waitStreamWindowSize strm)
            else return Nothing

{-# INLINE waitStreaming #-}
waitStreaming :: TBQueue a -> IO ()
waitStreaming tbq = atomically $ do
    isEmpty <- isEmptyTBQueue tbq
    checkSTM (not isEmpty)
