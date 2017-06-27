module Control.Concurrent.Async.Extras where

import Control.Concurrent.Async
import Data.Void
import Control.Monad

spawnAll :: [IO Void] -> IO Void
spawnAll = mapM async >=> fmap snd . waitAny
