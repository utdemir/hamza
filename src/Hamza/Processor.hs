{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hamza.Processor where

import Control.Concurrent.Async (concurrently)
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Profunctor
import Control.Category
import Prelude hiding (read, (.), id)
import Data.Void
import Data.Typeable
import qualified Data.Vector as V
import Data.Hashable
import Control.Concurrent.MVar
import Control.Monad
import Data.Time.Clock.POSIX
import Data.Functor
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan.Unagi.Bounded
import Data.Semigroup

import Control.Concurrent.Async.Extras (spawnAll)

-- * Core

newtype Processor input output = Processor
  { execProcessor :: IO input -> (output -> IO ()) -> IO Void
  }

instance (Typeable input, Typeable output) =>
         Show (Processor input output) where
  show _ =
    intercalate
      " "
      [ "Processor"
      , show $ typeRep (Proxy @input)
      , show $ typeRep (Proxy @output)
      ]

instance Category Processor where
  id = Processor $ \i o -> forever $ i >>= o
  Processor g . Processor f =
    Processor $ \input output -> do
      m <- newEmptyMVar
      spawnAll [f input (putMVar m), g (takeMVar m) output]

instance Profunctor Processor where
  dimap l r (Processor f) = Processor $ \i o -> f (l <$> i) (o . r)

instance Functor (Processor a) where
  fmap = rmap

instance Semigroup (Processor a b) where
  Processor a <> Processor b =
    Processor $ \input output -> do
      am <- newEmptyMVar
      bm <- newEmptyMVar
      spawnAll
        [ a (takeMVar am) output
        , b (takeMVar bm) output
        , forever $ input >>= (\i -> concurrently (putMVar am i) (putMVar bm i))
        ]

instance Monoid (Processor a b) where
  mempty = Processor $ \input _ -> forever input
  mappend = (<>)

runProcessor :: Processor Void Void -> IO Void
runProcessor processor = execProcessor processor bottom (const $ return ())
  where
    bottom :: IO a
    bottom = forever $ threadDelay 1000000000

-- * Constructors

simpleProcessor :: (input -> IO [output]) -> Processor input output
simpleProcessor f =
  Processor $ \input output -> forever (input >>= f >>= mapM_ output)

pureProcessor :: (input -> [output]) -> Processor input output
pureProcessor = simpleProcessor . (pure .)

-- * Utils

pPrint :: Show a => Processor a a
pPrint = simpleProcessor $ \i -> print i $> pure i

pDrain :: Processor a Void
pDrain = mempty

pFromList :: [a] -> Processor Void a
pFromList xs = Processor $ \_ w -> mapM_ w xs >> error "pProducer finished"

pCollect :: (a -> Maybe a') -> Processor a a'
pCollect f = pureProcessor $ maybe [] pure . f

pFilter :: (a -> Bool) -> Processor a a
pFilter p = pCollect $ \a -> if p a then Just a else Nothing

pEither ::
     Processor a a' -> Processor b b' -> Processor (Either a b) (Either a' b')
pEither leftsProcessor rightsProcessor =
  Processor $ \input output ->
    forever $ do
      left <- newEmptyMVar
      right <- newEmptyMVar
      spawnAll
        [ forever $
          input >>= \case
            Left a -> putMVar left a
            Right b -> putMVar right b
        , execProcessor leftsProcessor (takeMVar left) (output . Left)
        , execProcessor rightsProcessor (takeMVar right) (output . Right)
        ]

pLefts :: Processor a a' -> Processor (Either a b) (Either a' b)
pLefts = flip pEither id

pRights :: Processor b b' -> Processor (Either a b) (Either a b')
pRights = pEither id

pPartitionedThreaded ::
     Hashable partitionKey => Int -> (a -> partitionKey) -> Processor a b -> Processor a b
pPartitionedThreaded threadCount partition_ processor =
  Processor $ \read write -> do
    mvars <- V.replicateM threadCount newEmptyMVar
    let threads =
          flip map (V.toList mvars) $ \mvar ->
            execProcessor processor (takeMVar mvar) write
        distributor =
          forever $ do
            msg <- read
            let tid = hash (partition_ msg) `mod` threadCount
            putMVar (V.unsafeIndex mvars tid) msg
    spawnAll (distributor : threads)

pThreaded :: Int -> Processor a b -> Processor a b
pThreaded threadCount processor =
  Processor $ \read write ->
    spawnAll $ replicate threadCount (execProcessor processor read write)

pBuffer :: Int -> Processor a a
pBuffer size =
  Processor $ \read write -> do
    (in_, out) <- newChan size
    spawnAll
      [forever $ read >>= writeChan in_, forever $ readChan out >>= write]

--------------------------------------------------------------------------------

data Batch a = Batch
  { bContents :: [a]
  , bLength :: Int
  , bLastFlushed :: POSIXTime
  }

batchEmpty :: IO (Batch a)
batchEmpty = Batch [] 0 <$> getPOSIXTime

batchAdd :: a -> Batch a -> Batch a
batchAdd x (Batch xs i t) = Batch (x:xs) (i+1) t

-- FIXME: Wait properly before timing out
pBatch :: Int -> Int -> Processor a (NE.NonEmpty a)
pBatch batchSize window =
  Processor $ \read write -> do
    batch <- batchEmpty >>= newMVar
    spawnAll
      [ forever $ do
          threadDelay $ 1000 * 1000
          modifyMVar_ batch $ \b -> do
            now <- getPOSIXTime
            if bLastFlushed b + fromIntegral window < now
              then case NE.nonEmpty . reverse $ bContents b of
                Just xs -> write xs >> batchEmpty
                Nothing -> batchEmpty
              else return b
      , forever $ do
          a <- read
          modifyMVar_ batch $ \b ->
            let new = batchAdd a b
            in if bLength new >= batchSize
                 then write (NE.fromList . reverse $ bContents b) >> batchEmpty
                 else return new
      ]

pUnbatch :: Processor [a] a
pUnbatch = simpleProcessor return
