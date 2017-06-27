{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase  #-}

module Hamza.Store where

import qualified Control.Concurrent.Lock as Lock
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import Data.ByteString (ByteString)
import Data.Maybe
import Refined

import qualified Data.LRU as LRU
import qualified Data.BlockingBuffer as BB

data StoreBackend = StoreBackend
  { sbGetAll :: [ByteString] -> IO [(ByteString, ByteString)]
  , sbPutAll :: [(ByteString, ByteString)] -> IO ()
  , sbDeleteAll :: [ByteString] -> IO ()
  }

data Store = Store
  { sGet :: ByteString -> IO (Maybe ByteString)
  , sPut :: ByteString -> ByteString -> IO ()
  , sDelete :: ByteString -> IO ()
  , sFlush :: IO ()
  }

inMemoryStoreBackend :: IO StoreBackend
inMemoryStoreBackend = do
  store <- newMVar M.empty
  return $
    StoreBackend
    { sbGetAll =
        \xs ->
          let q = M.fromList $ map (, ()) xs
          in M.toList . flip M.intersection q <$> readMVar store
    , sbPutAll =
        \xs ->
          let q = M.fromList xs
          in modifyMVar_ store $ return . flip M.union q
    , sbDeleteAll =
        \xs ->
          let q = M.fromList $ map (, ()) xs
          in modifyMVar_ store $ return . flip M.difference q
    }

unoptimizedStore :: StoreBackend -> Store
unoptimizedStore StoreBackend {..} =
  Store
  { sGet = fmap (fmap snd . listToMaybe) . sbGetAll . pure
  , sPut = curry $ sbPutAll . pure
  , sDelete = sbDeleteAll . pure
  , sFlush = return ()
  }

data DBOp
  = Get ByteString (MVar (Maybe ByteString))
  | Put ByteString ByteString
  | Delete ByteString

cachingStore :: StoreBackend -> IO Store
cachingStore underlying = do
  mutex <- Lock.new
  cache <- LRU.new $$(refineTH 1000)

  -- TODO: Actually perform queries on cachingStore
  output <- BB.new $$(refineTH 1000)
  return $
    Store
    { sGet = -- FIXME: Don't get mutex when looking for cache
        \k ->
          Lock.with mutex $
          LRU.lookup cache k >>= \case
            Nothing -> do
              v <- newEmptyMVar
              LRU.insert cache k v
              BB.put output $ Get k v
              readMVar v
            Just v -> readMVar v
    , sPut =
        \k v ->
          Lock.with mutex $ do
            LRU.insert cache k =<< newMVar (Just v)
            BB.put output $ Put k v
    , sDelete =
        \k ->
          Lock.with mutex $ do
            LRU.insert cache k =<< newMVar Nothing
            BB.put output $ Delete k
    , sFlush = undefined -- TODO: Implement cachingStore.flush
    }
