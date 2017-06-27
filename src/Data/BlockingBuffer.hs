module Data.BlockingBuffer where

import Control.Concurrent.Chan.Unagi.Bounded
import Refined

newtype BlockingBuffer a = BlockingBuffer (InChan a, OutChan a)

new :: Refined NonNegative Int -> IO (BlockingBuffer a)
new = fmap BlockingBuffer . newChan . unrefine

put :: BlockingBuffer a -> a -> IO ()
put (BlockingBuffer (chan, _)) = writeChan chan

get :: BlockingBuffer a -> IO a
get (BlockingBuffer (_, chan)) = readChan chan
