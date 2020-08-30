{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Util (
    Filename, FileSubPath
  , NumScale(..)

  -- | = shorthand
  , fi -- ^ fromIntegral
  , fr -- ^ fromRational
  , tr -- ^ toRational

  -- | = IO and error
  , writeTimeAndError -- ^ String -> IO ()

  -- | = randomness
  , pickSome
  , pickSome'
  , pickSomeWithout3Clusters
  , no3Clusters

  -- | = strings
  , unusedName
  , lines'

  -- | = lists
  , (!!!)
  , unique
  , unique' -- ^ Eq a => [a] -> [a]
  , interleave
  , interleaves -- ^ [[a]] -> [a]
  , deleteAll
  , deleteShowQuotes
  , multiPartition -- ^ forall a b. Ord a => [(a,b)] -> [ (a,[b]) ]

  -- | = numbers & time
  , numBetween -- ^ (Num a, Ord a) => a -> a -> a -> Bool
  , dot        -- ^ Num a => (a,a) -> (a,a) -> a
  , taxiMetric -- ^ Num a => (a,a) -> (a,a) -> a
  , pairAdd    -- ^ Num a => (a,a) -> (a,a) -> (a,a)
  , pairMul    -- ^ Num a => a -> (a,a) -> (a,a)
  , linScale
  , logScale
  , myMod      -- ^ Int -> Int -> Int

  , lcmRatios
  , bumpArc
  , overlap

  -- | = vectors
  , divideAtMaxima
  , firstIndexGTE
  , lastIndexLTE
  ) where

import Prelude
import Control.Monad.ST
import Data.Fixed
import Data.Ratio
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Vector.Algorithms.Search
  (binarySearchLBy, binarySearchRBy, Comparison)
import Vivid (getTime, pick, MonadRandom)


type Filename = String

data NumScale = Lin | Log
  deriving (Show, Eq, Ord)

-- | the part before the filename, i.e. before the first `/`
type FileSubPath = String


-- | = shorthand, universal enough to be here
-- (most shorthand is in Abbrevs.hs instead).

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fr :: Fractional a => Rational -> a
fr = fromRational

tr :: Real a => a -> Rational
tr = toRational


-- | = error IO

writeTimeAndError :: String -> IO ()
writeTimeAndError msg = do now <- getTime
                           appendFile "errors.txt"
                             $ show now ++ ": " ++ msg


-- | = Randomness

pickSome :: forall a m. (Eq a, MonadRandom m)
         => Int -> [a] -> m [a]
pickSome 0 _ = return []
pickSome n as = do
  b  ::  a  <- pick as
  bs :: [a] <- pickSome (n-1) (L.delete b as)
    -- if deleting b isn't enough, consider pickSome'
  return $ b:bs

pickSome' :: forall a m. (Eq a, MonadRandom m)
  => (a -> [a] -> [a]) -> Int -> [a] -> m [a]
pickSome' _ 0 _ = return []
pickSome' f n as = do
  b  ::  a   <- pick as
  bs :: [a] <- pickSome (n-1) (f b as)
  return $ b:bs

pickSomeWithout3Clusters ::
  forall a m. (Num a, Eq a, MonadRandom m)
  => Int -> [a] -> m [a]
pickSomeWithout3Clusters = pickSome' no3Clusters

no3Clusters :: (Num a, Eq a) => a -> [a] -> [a]
no3Clusters chosen0 remaining0 =
  L.delete chosen0 $ f chosen0 $ g chosen0 $ remaining0 where
  f chosen remaining =
    if elem       (chosen + 1) remaining
    then L.delete (chosen - 1) remaining else remaining
  g chosen remaining =
    if elem       (chosen - 1) remaining
    then L.delete (chosen + 1) remaining else remaining


-- | = Strings

unusedName :: [String] -> String
unusedName names = head $ (L.\\) allStrings names where
  allStrings = [ c : s | s <- "" : allStrings
                       , c <- ['a'..'z'] ++ ['0'..'9'] ]

-- | `lines'` is like `lines` except you specify the separator,
-- and it works for more than characters.
-- PITFALL: `lines' d s` will ignore the first character of `s`if it is `d`
lines' :: Eq a => a -> [a] -> [[a]]
lines' _ [] = []
lines' separator s = let
  s' = if separator == head s then tail s else s
  (chunk, rest) = span (/= separator) s'
  in chunk : lines' separator rest


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
unique :: Ord a => [a] -> [a]
unique = S.toList . S.fromList

-- | like `mod` but centered, rather than minimized, at 0
myMod :: Integral a => a -> a -> a
myMod x b =
  let m = mod' x b
  in if m > div b 2
     then m-b else m

-- | Slower, but does not require `Ord a`
unique' :: Eq a => [a] -> [a]
unique' [] = []
unique' (a:as) = a : (unique' $ filter (not . (==) a) as)

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (zipWith (\x y -> [x]++[y]) xs ys)

interleaves :: [[a]] -> [a]
interleaves ls = case any null ls of
  True -> []
  False -> map head ls ++ interleaves (map tail ls)

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
multiPartition = let
  f :: (a,b) -> M.Map a [b] -> M.Map a [b]
  f (a,b) = M.insertWith (++) a [b]
  in M.toList . foldr f M.empty


-- | = numbers & time

numBetween :: (Num a, Ord a) => a -> a -> a -> Bool
numBetween low high x = x >= low && x <= high

dot :: Num a => (a,a) -> (a,a) -> a
dot (a,b) (c,d) = a*c + b*d

taxiMetric :: Num a => (a,a) -> (a,a) -> a
taxiMetric (a,b) (c,d) = abs (a-c) + abs (b-d)

pairAdd :: Num a => (a,a) -> (a,a) -> (a,a)
pairAdd (a,b) (c,d) = (a+c, b+d)

pairMul :: Num a => a -> (a,a) -> (a,a)
pairMul n (a,b) = (n*a,n*b)

linScale :: Floating a
         => (a,a) -- ^ The input range
         -> (a,a) -- ^ The output range.
         -> a     -- ^ The input.
         -> a
linScale (a,b) (c,d) e = let
  perc0 = (e - a) / (b - a) -- like a %-age: how far e is from a to b
  perc1 = (d - c)*perc0 + c -- like perc0, but from lc to ld
  in perc1

-- | `logScale (a,b) (c,d) e` will transform an linear input in `(a,b)`
-- to an exponential output in `(c,d)`.
-- If `c/d` is near 1, this should have very little effect;
-- if it's near 0, a very noticeable one.
-- Surprisingly, the logarithm is irrelevant.
logScale :: Floating a
         => (a,a) -- ^ The input range
         -> (a,a) -- ^ The output range. PITFALL: Both must be positive.
         -> a     -- ^ The input
         -> a
logScale (a,b) (c,d) e =
  let lc = log c
      ld = log d
      perc0 = (e - a) / (b - a) -- like a %-age: how far e is from a to b
      perc1 = (ld - lc)*perc0 + lc -- like perc0, but from lc to ld
  in exp perc1

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
-- preserve sortedness (shoving whatever was there before to a later spot).
-- If none such, returns length of vector, because that's where to insert.
firstIndexGTE :: Comparison a -> V.Vector a -> a -> Int
firstIndexGTE comp v a = runST $ do
  v' <- V.thaw v
  return =<< binarySearchLBy comp v' a

-- | 0-indexed. Returns the last index where you could insert `a` and
-- preserve sortedness (shoving whatever was there to a later spot).
-- If none such, returns length of vector.
lastIndexLTE :: Comparison a -> V.Vector a -> a -> Int
lastIndexLTE comp v a = runST $ do
  v' <- V.thaw v
  return =<< binarySearchRBy comp v' a
