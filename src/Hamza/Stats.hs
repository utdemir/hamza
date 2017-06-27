{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Hamza.Stats where

import Control.Concurrent.MVar

import Prelude hiding (read)
import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import Control.Concurrent.Async
import System.Metrics as Metrics

import Hamza.Processor
import qualified Data.CircularBuffer as CB

pStats :: Metrics.Store -> T.Text -> Processor a a
pStats store pref =
  Processor $ \read write -> do
    counter <- newMVar 0
    buf <- CB.new @15 0
    let p n = T.concat [pref, ".", n]
    registerCounter (p "processed messages") (readMVar counter) store
    flip (registerGauge (p "messages per second")) store $ do
      (first, last_) <- (,) <$> CB.head buf <*> CB.last buf
      return $ div (first - last_) (fromIntegral $ CB.size buf)
    _ <- async . forever $ do
      r <- readMVar counter
      CB.put buf r
      threadDelay $ 1 * 1000 * 1000
    forever $ read >>= write >> modifyMVar_ counter (\i -> return $! i + 10)
