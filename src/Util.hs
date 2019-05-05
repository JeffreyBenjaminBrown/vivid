{-# LANGUAGE ScopedTypeVariables #-}

module Util (
  writeTimeAndError

  -- | abbreviations
  , fr -- ^ fromRational
  , tr -- ^ toRational
  , m1 -- ^ M.singleton
  , mfl -- ^ M.fromList

  -- | randomness
  , pickSome
  , pickSome'
  , pickSomeWithout3Clusters
  , no3Clusters

  -- | strings
  , unusedName

  -- | = lists
  , (!!!)
  , unique
  , unique' -- ^ Eq a => [a] -> [a]
  , interleave
  , deleteAll
  , deleteShowQuotes
  , multiPartition -- ^ forall a b. Ord a => [(a,b)] -> [ (a,[b]) ]

  -- | = numbers & time
  , lcmRatios
  , bumpArc
  , overlap
  , nextPhase0
  , prevPhase0

  -- | = vectors
  , divideAtMaxima
  , firstIndexGTE
  , lastIndexLTE
  ) where

import Prelude hiding (abs)
import Control.Monad.ST
import Data.Ratio
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Vector.Algorithms.Search
  (binarySearchLBy, binarySearchRBy, Comparison)

import Vivid (getTime, pick, MonadRandom)


writeTimeAndError :: String -> IO ()
writeTimeAndError msg = do now <- getTime
                           appendFile "errors.txt"
                             $ show now ++ ": " ++ msg


-- | = abbreviations

fr :: Fractional a => Rational -> a
fr = fromRational

tr :: Real a => a -> Rational
tr = toRational

m1 :: k -> a -> M.Map k a
m1 = M.singleton

mfl :: Ord k => [(k, a)] -> M.Map k a
mfl = M.fromList


-- | = Randomness

pickSome :: forall a m. (Eq a, MonadRandom m)
         => Int -> [a] -> m [a]
pickSome 0 _ = return []
pickSome n as = do (b  ::  a ) <- pick as
                   (bs :: [a]) <- pickSome (n-1) (L.delete b as)
                   -- if deleting b isn't enough, consider pickSome'
                   return $ b:bs

pickSome' :: forall a m. (Eq a, MonadRandom m)
         => (a -> [a] -> [a]) -> Int -> [a] -> m [a]
pickSome' _ 0 _ = return []
pickSome' f n as = do (b  ::  a ) <- pick as
                      (bs :: [a]) <- pickSome (n-1) (f b as)
                      return $ b:bs

pickSomeWithout3Clusters :: forall a m. (Num a, Eq a, MonadRandom m)
                         => Int -> [a] -> m [a]
pickSomeWithout3Clusters = pickSome' no3Clusters

no3Clusters :: (Num a, Eq a) => a -> [a] -> [a]
no3Clusters chosen0 remaining0 =
  L.delete chosen0 $ f chosen0 $ g chosen0 $ remaining0 where
  f chosen remaining = if elem       (chosen + 1) remaining
                       then L.delete (chosen - 1) remaining else remaining
  g chosen remaining = if elem       (chosen - 1) remaining
                       then L.delete (chosen + 1) remaining else remaining


-- | = Strings

unusedName :: [String] -> String
unusedName names = head $ (L.\\) allStrings names where
  allStrings = [ c : s | s <- "" : allStrings
                       , c <- ['a'..'z'] ++ ['0'..'9'] ]


-- | = Lenses

-- "Illegal polymorphic type: Lens' a b
--    GHC doesn't yet support impredicative polymorphism"
--overAll :: [Lens' a b] -> (b -> b) -> a -> a
--overAll ls f = foldl (.) (map (flip over f) ls) id ls


-- | = Lists

-- | Like `!!`, it  selects an element from xs,
-- but it wraps over at the end of the list.
-- >>> map ((!!!) [1,3,5]) [0,1,2,3,4,5]
-- [1,3,5,1,3,5]
-- Copied from Sound.Tidal.
(!!!) :: [a] -> Int -> a
(!!!) xs n = xs !! (n `mod` length xs)

-- | There's a Hackage package for this surely that's maybe faster, but
-- it's not compatible with the stack snapshot I'm using.
unique :: (Ord a, Eq a) => [a] -> [a]
unique = S.toList . S.fromList

-- | Slower, but does not require `Ord a`
unique' :: Eq a => [a] -> [a]
unique' [] = []
unique' (a:as) = a : (unique' $ filter (not . (==) a) as)

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (zipWith (\x y -> [x]++[y]) xs ys)

-- | Unlike Data.List.delete, this deletes more than the first instance.
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll a (b:bs) = if a==b then deleteAll a bs
                     else b : deleteAll a bs

deleteShowQuotes :: String -> String
  -- TODO ? `deleteAllSubstrings "\""` would be more natural, and maybe safer.
  -- With this, if someone used '\' on its own, it would disappear.
deleteShowQuotes = deleteAll '\"' . deleteAll '\\'

-- | `multiPartition abs` preserves the order of the `b`s
-- within each `a` group.
multiPartition :: forall a b. Ord a => [(a,b)] -> [ (a,[b]) ]
multiPartition abs = let
  f :: (a,b) -> M.Map a [b] -> M.Map a [b]
  f (a,b) = M.insertWith (++) a [b]
  in M.toList $ foldr f M.empty abs


-- | = Time

-- | least common denominator
lcmRatios :: Rational -> Rational -> Rational
lcmRatios x y = let (a,b) = (numerator x, denominator x)
                    (c,d) = (numerator y, denominator y)
                    l = lcm b d
                in lcm (a * div l b) (c * div l d) % l

bumpArc :: Num a => a -> (a,a) -> (a,a)
bumpArc b (s,t) = (s + b, t + b)

-- | ASSUMES both inputs are well-formed intervals, s.t. start <= end.
-- PITFALL: If either event has zero duration,
-- then they must strictly not overlap. Otherwise their endpoints can meet.
overlap :: (Num a, Ord a) => (a,a) -> (a,a) -> Bool
overlap (a,b) (c,d) | a == b =
                      b >= c && b <= d
                    | c == d =
                      c >= a && c <= b
                    | otherwise =
                         a <= c && b > c
                      || c <= a && d > a

-- | `time0` is the first time that had phase 0
-- | TODO ? rewrite using div', mod' from Data.Fixed
nextPhase0 :: RealFrac a => a -> a -> a -> a
nextPhase0 time0 period now =
  fromIntegral x * period + time0
  where x :: Int = ceiling $ (now - time0) / period

-- | PITFALL ? if lastPhase0 or nextPhase0 was called precisely at phase0,
-- both would yield the same result. Since time is measured in microseconds
-- there is exactly a one in a million chance of that.
-- | TODO ? rewrite using div', mod' from Data.Fixed
prevPhase0 :: RealFrac a => a -> a -> a -> a
prevPhase0 time0 period now =
  fromIntegral x * period + time0
  where x :: Int = floor $ (now - time0) / period


-- | == Vectors

-- | example
-- > prin $ divideAtMaxima fst [3.1,4.9,7] $ V.fromList $ map (,()) [1..8]
-- [(1.0,()),(2.0,()),(3.0,())]
-- [(4.0,())]
-- [(5.0,()),(6.0,())]
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

-- | 0-indexed. Returns the first index where you could insert `a` and
-- preserve sortedness (shoving whatever was there before to the right).
-- If none such, returns length of vector.
firstIndexGTE :: Comparison a -> V.Vector a -> a -> Int
firstIndexGTE comp v a = runST $ do
  v' <- V.thaw v
  return =<< binarySearchLBy comp v' a

-- | 0-indexed. Returns the last index where you could insert `a` and
-- preserve sortedness (shoving whatever was there before to the right).
-- If none such, returns length of vector.
lastIndexLTE :: Comparison a -> V.Vector a -> a -> Int
lastIndexLTE comp v a = runST $ do
  v' <- V.thaw v
  return =<< binarySearchRBy comp v' a
