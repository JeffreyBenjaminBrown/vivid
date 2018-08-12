{-# LANGUAGE ScopedTypeVariables #-}

module Vivid.Jbb.Util (
  lcmRatios

  -- | = lists
  , unique
  , interleave

  -- | = time
  , nextPhase0
  , prevPhase0

  -- | = vectors
  , divideAtMaxima
  , firstIndexGTE
  , lastIndexJustGTE
  ) where

import Control.Monad.ST
import Data.Ratio
import qualified Data.Vector as V
import Data.Vector.Algorithms.Search
  (binarySearchLBy, binarySearchRBy, Comparison)


lcmRatios :: Rational -> Rational -> Rational
lcmRatios x y = let (a,b) = (numerator x, denominator x)
                    (c,d) = (numerator y, denominator y)
                    l = lcm b d
                in lcm (a * div l b) (c * div l d) % l


-- | = Functions for lists

-- | There's a Hackage package for this surely that's faster, but
-- it's not compatible with the stack snapshot I'm using.
unique :: Eq a => [a] -> [a]
unique [] = []
unique (a:as) = a : (unique $ filter (not . (==) a) as)

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (zipWith (\x y -> [x]++[y]) xs ys)


-- | = Functions for time

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


-- | == Functions on Vectors

-- | example:
-- > x
-- [(1,"gobot"),(2,"gobot"),(3,"gobot"),(4,"gobot"),(5,"gobot"),(6,"gobot")]
-- > divideAtMaxima fst [3,5] $ V.fromList x
-- [[(1,"gobot"),(2,"gobot")]
-- ,[(3,"gobot"),(4,"gobot")]]
divideAtMaxima :: forall a b. Ord b
               => (a->b) -> [b] -> V.Vector a -> [V.Vector a]
divideAtMaxima view upperBounds stuff =
  reverse $ go [] upperBounds stuff where
  go :: [V.Vector a] -> [b] -> V.Vector a -> [V.Vector a]
  -- even if `stuff` is empty, keep going, because the resulting
  -- series of empty lists is important for the interleaving in append'
  go acc []     _               = acc
  go acc (t:ts) vec             =
    let (lt,gte) = V.partition ((< t) . view) vec
    in go (lt : acc) ts gte

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
