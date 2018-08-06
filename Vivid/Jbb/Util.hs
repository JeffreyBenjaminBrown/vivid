module Vivid.Jbb.Util where

import Control.Monad.ST
import qualified Data.Vector as V
import Data.Vector.Algorithms.Search
  (binarySearchLBy, binarySearchRBy, Comparison)


-- | There's a Hackage package for this surely that's faster, but
-- it's not compatible with the stack snapshot I'm using.
unique :: Eq a => [a] -> [a]
unique [] = []
unique (a:as) = a : (unique $ filter (not . (==) a) as)


-- | = Functions for working with time

-- | time0 is the first time that had phase 0
nextPhase0 :: RealFrac a => a -> a -> a -> a
nextPhase0 time0 period now =
  fromIntegral (ceiling $ (now - time0) / period ) * period + time0

-- | PITFALL ? if lastPhase0 or nextPhase0 was called precisely at phase0,
-- both would yield the same result. Since time is measured in microseconds
-- there is exactly a one in a million chance of that.
prevPhase0 :: RealFrac a => a -> a -> a -> a
prevPhase0 time0 period now =
  fromIntegral (floor $ (now - time0) / period ) * period + time0


-- | = Functions to find a range of items of interest in a sorted vector.

-- | 0-indexed. Returns the first index you could insert `a` and preserve
-- sortedness (shoving whatever was there before to the right).
-- If none such, returns length of vector.
firstIndexGTE :: Comparison a -> V.Vector a -> a -> Int
firstIndexGTE comp v a = runST $ do
  v' <- V.thaw v
  return =<< binarySearchLBy comp v' a

-- | 0-indexed. Returns the last index you could insert `a` and preserve
-- sortedness (shoving whatever was there before to the right).
-- If none such, returns length of vector.
lastIndexJustGTE :: Comparison a -> V.Vector a -> a -> Int
lastIndexJustGTE comp v a = runST $ do
  v' <- V.thaw v
  return =<< binarySearchRBy comp v' a
