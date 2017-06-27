{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.CircularBuffer
  ( new
  , put
  , head
  , last
  , get
  , size
  , toList
  ) where

import Refined
import Prelude hiding (head, last)
import qualified Data.Vector.Mutable as V
import GHC.TypeLits
import Control.Concurrent.MVar
import Data.Proxy

data CircularBuffer (n :: Nat) (a :: *) =
  CircularBuffer (MVar (V.IOVector a, Int))

new ::
     forall n a. (KnownNat n, CmpNat n 1 ~ 'GT)
  => a
  -> IO (CircularBuffer n a)
new a = do
  let size_ = fromIntegral $ natVal @n Proxy
  vec <- V.replicate size_ a
  CircularBuffer <$> newMVar (vec, 0)

put :: forall n a. KnownNat n => CircularBuffer n a -> a -> IO ()
put (CircularBuffer mvar) a =
  modifyMVar_ mvar $ \(vec, head_) -> do
    let size_ = fromIntegral $ natVal @n Proxy
        newHead = mod (head_ + 1) size_
    V.write vec newHead a
    return (vec, newHead)

head :: KnownNat n => CircularBuffer n a -> IO a
head = getIndex 0

last :: KnownNat n => CircularBuffer n a -> IO a
last = getIndex (-1)

get :: KnownNat n => CircularBuffer n a -> Refined (To (n-1)) Int -> IO a
get buf = flip getIndex buf . unrefine

size ::
     forall n a. KnownNat n
  => CircularBuffer n a
  -> Int
size _ = fromInteger $ natVal @n Proxy

getIndex :: forall n a. KnownNat n => Int -> CircularBuffer n a -> IO a
getIndex n (CircularBuffer mvar) =
  withMVar mvar $ \(vec, head_) -> do
    let size_ = fromIntegral $ natVal @n Proxy
        index = mod (head_ - n) size_
    V.read vec index

toList :: forall n a. KnownNat n => CircularBuffer n a -> IO [a]
toList buf =
  let size_ = fromIntegral $ natVal @n Proxy
  in mapM (flip getIndex buf) [0 .. size_ - 1]
