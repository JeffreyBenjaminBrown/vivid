{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.JI.Lib where

import           Control.Lens
import qualified Data.List as L
import qualified Data.Set as S
import           Data.Ratio
import           Data.Ord (comparing)

import Montevideo.Util.Edo
import Montevideo.JI.Harmonics


type Cents = Float

-- | 'nearOctaveMultiples 50 $ cents (7/4)` returns pairs in which
-- the first member is a power of 7/4 (a muliple of 'cents 7/4')
-- that's close to a power of 2.
-- BUG: If the multiple is just shy of 1200 cents, it should be included.
-- Instead it only counts when it is just over 1200.
nearOctaveMultiples :: Cents -> Cents -> [(Int, Cents)]
nearOctaveMultiples tolerance generator =
  filter ((< tolerance) . snd) $
  take 150 $
  zip [0..] $
  map (fromIntegral . flip mod 1200 . round) $
  [0, generator ..]

-- | List all the 13-odd-limit JI chords not involving unity.
-- Kind of silly -- reading the output it becomes obvious how to
-- recreate this list in your head.
jiChords :: [[Int]]
jiChords = do
  let rs = [3,5..13] -- [1,3/2,5/4,9/8,11/8,13/8]
  a <- rs
  b <- rs
  c <- rs
  S.toList . S.fromList . filter ((> 2) . length)
    $ [ L.sort . S.toList . S.fromList
        $ [a,b,c]]

minNotes, maxNotes :: Integer
minNotes = 12
maxNotes = 130

tols :: [Integer]
tols = -- This list can have any length.
  -- It describes the maximum error for the first harmonics.
  (*10) <$> [5,6,6]
--    example:
--    [ 20 -- approx 3/2 to within 2 cents
--    , 40 -- 5/4 to within 4 cents
--    , 60 -- 7/4
--    , 80 ] -- 11/8

-- | terms: r = ratio
--          d = denominator
--          n = numerator
--          e = edo frac

compareScales :: IO ()
compareScales = myPrint looking

myPrint :: forall a t. (Foldable t, Show a)
  => t a -> IO ()
myPrint = mapM_ $ putStrLn . show

looking :: [(Integer, [Integer])]
looking = let
  f (_, _errs) =
    ( and $ zipWith (>=) tols $ map abs _errs )
  in filter f matrix -- & map (_2 %~ take 5)

matrix :: [(Integer, [Integer])]
matrix =
  [ (d, map (round . (^. _2 . _3)) $ bests d)
  | d <- [minNotes .. maxNotes] ]

-- | A damage measure.
-- Unless the `map (uncurry ...)` clause is commented out,
-- lower primes weigh more.
-- For instance, here's a way to find the best
-- (by one definition) of the first 60 EDOs.
-- myPrint $ filter ((< 290) . snd) $ [(n, errorSum [4..8] n) | n <- [1..60]]
errorSum
  :: [Double] -- ^ How to weigh the first 5 primes.
      -- For example, if weights is [4..8],
      -- then prime 3 weighs twice what 13 does, with
      -- other primes' weights scaling linearly between.
      -- If zipped with (repeat 1), the weights are uniform.
  -> Integer
  -> Integer

errorSum weights =
  round . sum . map abs
  . ( let
        mean = sum weights / fromIntegral (length weights)
        weights' = (/mean) <$> weights
      -- Normalizing by the mean makes `errorSum`
      -- values comparable across different weights.
      in map (uncurry (/))
      . flip zip weights')
  . map (^. _2 . _3)
  . bests

truth :: Floating c => Int -> [(c,Rational)]
truth p = f <$> harmonics p
  where f h = (cents h, h)

type Report = (Integer, Rational, Integer, Integer, Integer)

-- | `intervals d` shows how the notes of
-- `d`-edo approximate `just_intervals`.
intervals :: Integer -> IO ()
intervals = px . whatIsThis

px :: [Report] -> IO ()
px = mapM_ putStrLn . map f where
  f :: Report -> String
  f (i,r,j,k,l) =
    let t = "\t"
    in show i ++ t ++ show r ++ "\t\t" ++
       show j ++ t ++ show k ++ t ++ show l

whatIsThis :: Integer -> [Report]
whatIsThis d = map f just_intervals where
  less x = round $ x / 10
  f (c,r) = let
    (note, cNote, errNote) = best d r
    in (less c, r, note, cNote, less errNote)

just_intervals :: [(Double, Rational)]
just_intervals = let
  f x = if x >= 2 then x/2 else x
  pair n = (cents $ fromRational n, n)
  in map (pair . f) $ L.sort $ lim_15

errSearch :: [Integer] -> [(Integer,Double)]
errSearch edos =
  zip edos $
  map (sum_errs $ map (\x -> 1/x**(3/2)) [2..]) edos

-- | `sum_errs d` gives the sum of the absolute values of the errors
-- of `d`-edo in approximating the harmonics of interest.
sum_errs
  :: [Double] -- ^ How to weight the harmonics.
  -> Integer
  -> Double
sum_errs weights d = sum $ map square $ zipWith (*) weights $
                     abs . err_of_best <$> bests d
  where
    square x = x*x
    err_of_best (_,(_,_,e)) = e

bests :: Integer -> [(Rational, (Integer, Integer, Double))]
bests d = (\r -> (r, best d r))
          <$> [ 3%2,5%4,7%4,11%8,13%8,
                17%16,19%16,23%16,29%16,31%16,
                33%32 ]

-- | TODO ? This could be more efficient, ala Thanos.best'.
best :: Integer -> Rational -> (Integer, Integer, Double)
best d r = L.minimumBy (comparing $ abs . (^. _3))
           $ errs d r

errs :: Floating a
     => Integer -- ^ the edo
     -> Rational -- ^ what to approximate
     -> [ ( Integer -- ^ a step of the edo
          , Integer -- ^ that step in cents
          , a)]     -- ^ error of that step
errs n r =
  [ ( i
    , round $ octavesToDents $ fromIntegral i / fromIntegral n
    , err r           $ fromIntegral i / fromIntegral n )
  | i <- [0..n-1 :: Integer] ]

err :: Floating a
  => Rational -- ^ what to approximate
  -> a        -- ^ the EDO approximating it
  -> a        -- ^ a step of that EDO
err true_frac approx_edo =
  octavesToDents approx_edo - cents true_frac
