{-# LANGUAGE LambdaCase #-}

module Data.LRU
  ( LRU
  , new
  , insert
  , lookup
  , peek
  , peekMin
  ) where

import Prelude hiding (lookup)
import qualified Data.OrdPSQ as P
import Control.Concurrent.MVar
import Refined

-- | A wrapper around 'OrdPSQ' which automatically drops least
-- recently used elements when given size is reached.
data LRU k v =
  LRU Int
      (MVar (P.OrdPSQ k Integer v, Integer))

-- | Creates a LRU with given max size.
new :: Refined NonNegative Int -> IO (LRU k v)
new maxSize = LRU (unrefine maxSize) <$> newMVar (P.empty, 0)

-- | Insert a new element to LRU. Least recently used element will be
-- dropped if LRU is full.
insert :: Ord k => LRU k v -> k -> v -> IO ()
insert (LRU maxSize mvar) k v =
  modifyMVar_ mvar $ \(lru, prio) ->
    return (shake $ P.insert k prio v lru, prio + 1)
  where
    shake lru =
      if P.size lru <= maxSize
        then lru
        else P.deleteMin lru

-- | Lookup an element from the LRU, and mark it as most recently
-- used.
lookup :: Ord k => LRU k v -> k -> IO (Maybe v)
lookup (LRU _ mvar) k =
  modifyMVar mvar $ \(lru, b) ->
    return $
    case P.lookup k lru of
      Nothing -> ((lru, b), Nothing)
      Just (prio, v) -> ((P.insert k prio v lru, b), Just v)

-- | Lookup an element from the LRU, without marking it as most
-- recently used.
peek :: Ord k => LRU k v -> k -> IO (Maybe v)
peek (LRU _ mvar) k = fmap snd . P.lookup k . fst <$> readMVar mvar

-- | Get the smallest (key, value) pair from the LRU, without marking
-- it as most recently used.
peekMin :: Ord k => LRU k v -> IO (Maybe (k, v))
peekMin (LRU _ mvar) =
  fmap
    (\case
       (k, _, v) -> (k, v)) .
  P.findMin . fst <$>
  readMVar mvar
