module PWM (
    Stream(..),
    writeStream,
    executeStream
    ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan()
import Control.Concurrent
import Control.Monad
import Data.Maybe

type Pulse = Int

data Stream = Stream { period :: Int }

data Switch = HIGH | LOW
    deriving Show

createStream :: IO (TChan Pulse)
createStream = newTChanIO

writeStream :: TChan Pulse -> Pulse -> IO ()
writeStream chan pulse = atomically $ writeTChan chan pulse

readStream :: Stream -> TChan Pulse -> (Switch -> IO ()) -> IO ()
readStream stream chan f = (atomically $ readTChan chan) >>= read'
  where
    read' :: Pulse -> IO ()
    read' v = do
        a <- atomically $ tryReadTChan chan
        let d = fromMaybe v a
        let e = period stream - d
        when (e > 0) $ do
            f HIGH
            threadDelay d
            f LOW
            threadDelay e
        read' d

executeStream :: Stream -> (Switch -> IO ()) -> IO (TChan Pulse)
executeStream stream f = do
    chan <- createStream
    _ <- forkIO $ readStream stream chan f
    return chan
