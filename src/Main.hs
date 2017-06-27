{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Metrics
import System.Remote.Monitoring
import Control.Category

import Hamza.Stats
import Hamza.Processor

main :: IO ()
main = do
  store <- newStore
  registerGcMetrics store
  _ <- forkServerWith store "localhost" 8000
  _ <- runProcessor $ pFromList (repeat ()) >>> pStats store "simple" >>> pDrain
  return ()
