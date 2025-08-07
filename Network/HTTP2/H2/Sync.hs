{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.H2.Sync (
    LoopCheck (..),
    newLoopCheck,
    syncWithSender,
    syncWithSender',
    makeOutput,
    makeOutputIO,
    enqueueOutputSIO,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network.Control
import Network.HTTP.Semantics.IO

import Network.HTTP2.H2.Context
import Network.HTTP2.H2.Queue
import Network.HTTP2.H2.Types
import GHC.Stack

syncWithSender
    :: HasCallStack => Context
    -> Stream
    -> OutputType
    -> LoopCheck
    -> IO ()
syncWithSender ctx@Context{..} strm otyp lc = do
    putStrLn "\n\nHTTP2: SYNC WITH SENDER, MAKING OUTPUT\n\n"
    (pop, out) <- makeOutput strm otyp
    putStrLn "\n\nHTTP2: SYNC WITH SENDER, MADE OUTPUT\n\n"
    enqueueOutput outputQ out
    putStrLn "\n\nHTTP2: SYNC WITH SENDER, ENQUEUED OUTPUT\n\n"
    syncWithSender' ctx pop lc

makeOutput :: Stream -> OutputType -> IO (IO Sync, Output)
makeOutput strm otyp = do
    var <- newEmptyMVar
    let push = OutputSync $ \mout -> do
            putStrLn "\n\nHTTP2: OutputSync from makeOutput\n\n"
            case mout of
                Nothing -> do
                    putMVar var Done
                Just ot -> do
                    putMVar var $ Cont ot
        pop = do
            putStrLn "\n\nHTTP2: POPPING\n\n"
            res <- takeMVar var
            putStrLn "\n\nHTTP2: POPPED\n\n"
            return res
        out =
            Output
                { outputStream = strm
                , outputType = otyp
                , outputSync = push
                }
    return (pop, out)

makeOutputIO :: Context -> Stream -> OutputType -> Output
makeOutputIO Context{..} strm otyp = out
  where
    push = OutputSync $ \mout -> do
        putStrLn "\n\nHTTP2: OutputSync from makeOutputIO\n\n"
        case mout of
            Nothing -> return ()
            -- Sender enqueues output again ignoring
            -- the stream TX window.
            Just ot -> enqueueOutput outputQ ot
    out =
        Output
            { outputStream = strm
            , outputType = otyp
            , outputSync = push
            }

enqueueOutputSIO :: HasCallStack => Context -> Stream -> OutputType -> IO ()
enqueueOutputSIO ctx@Context{..} strm otyp = do
    let out = makeOutputIO ctx strm otyp
    enqueueOutput outputQ out

syncWithSender' :: HasCallStack => Context -> IO Sync -> LoopCheck -> IO ()
syncWithSender' Context{..} pop lc = loop
  where
    loop = do
        putStrLn "\n\nHTTP2: before pop\n\n"
        s <- pop
        putStrLn "\n\nHTTP2: after pop\n\n"
        case s of
            Done -> do
                putStrLn "\n\nHTTP2: syncWithSender' Done case\n\n"
                return ()
            Cont newout -> do
                putStrLn "\n\nHTTP2: before checkLoop\n\n"
                cont <- checkLoop lc
                putStrLn $ "\n\nHTTP2: after checkLoop: cont = " ++ show cont ++ "\n\n"
                when cont $ do
                    -- This is justified by the precondition above
                    putStrLn "\n\nHTTP2: enqueueing in syncWithSender' \n\n"
                    enqueueOutput outputQ newout
                    putStrLn "\n\nHTTP2: enqueued in syncWithSender'\n\n"
                    loop

newLoopCheck :: Stream -> Maybe (TBQueue StreamingChunk) -> IO LoopCheck
newLoopCheck strm mtbq = do
    tovar <- newTVarIO False
    return $
        LoopCheck
            { lcTBQ = mtbq
            , lcTimeout = tovar
            , lcWindow = streamTxFlow strm
            }

data LoopCheck = LoopCheck
    { lcTBQ :: Maybe (TBQueue StreamingChunk)
    , lcTimeout :: TVar Bool
    , lcWindow :: TVar TxFlow
    }

checkLoop :: LoopCheck -> IO Bool
checkLoop LoopCheck{..} = atomically $ do
    tout <- readTVar lcTimeout
    if tout
        then return False
        else do
            waitStreaming' lcTBQ
            waitStreamWindowSizeSTM lcWindow
            return True

waitStreaming' :: Maybe (TBQueue a) -> STM ()
waitStreaming' Nothing = return ()
waitStreaming' (Just tbq) = do
    isEmpty <- isEmptyTBQueue tbq
    check (not isEmpty)

waitStreamWindowSizeSTM :: TVar TxFlow -> STM ()
waitStreamWindowSizeSTM txf = do
    w <- txWindowSize <$> readTVar txf
    check (w > 0)
