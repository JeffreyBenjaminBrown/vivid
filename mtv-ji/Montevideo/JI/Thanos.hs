-- | Thanos tunings are high-EDO tunings for stringed instruments,
-- in which one skips some frets.
-- Each tuning is defined by the number of frets skipped (the "modulus"),
-- and the space in EDO steps between adjacent strings (the "spacing").

module Montevideo.JI.Thanos where

import Prelude hiding (String) -- PITFALL: Weird.

import Control.Lens
import           Data.List
import           Data.Ord
import           Data.Ratio

import Montevideo.Util
import Montevideo.JI.Lib


type Edo = Int -- ^ e.g. 31
type Modulus = Int -- ^ The Thanos tuning will keep every nth note, where n is the modulus. For instance, in Kite's tuning the modulus is 2 (and the Edo is 41).
type Interval = Int -- ^ A value from an Edo system. For instance,
                    -- the perfect fifth in Edo 31 is Interval 18.

-- | Also a value from the Edo system, with the particular interpretation
-- that it rerpresents the space between adjacent strings.
-- For instance, in the Kite tuning, this would be 13, because
-- on a Kite guitar each pair of adjacent strings is tuned 13\41 apart.
type Spacing = Interval

type String = Int
type Fret = Int
type FretDistance = Int -- ^ The distance between two frets.

data ThanosReport = ThanosReport
  { report_fretDistance :: FretDistance
  , report_intervalReports :: [IntervalReport] }

data IntervalReport = IntervalReport
  { ir_InEdo  :: Interval -- ^ This
  , ir_Ratio  :: Rational -- ^ and this represent the same note
  , ir_String :: String   -- ^ This
  , ir_Fret   :: Fret }   -- ^ and this represent where it lies closest.


-- | `thanosReports` explores a lot of ways one might
-- tune a guitar to achieve a certain `edo`.
--
-- For instance, to check how including only every third fret
-- works to play in 58 edo, run
-- `thanosReports 3 58 <whatever>`.
thanosReports
  :: Modulus -- ^ Make this `n` to keep every `n`th fret.
  -> Edo -- ^ The EDO to examine.
  -> FretDistance -- ^ The farthest you want to stretch in 12-edo frets.
  -> IO ()
thanosReports modulus edo maxStretchIn12Edo = let
  spacings = filter (feasibleSpacing modulus) [5 .. 50]
  r1 = [ (s, thanosReport modulus s $ fi edo)
       | s <- spacings ]
  r2 = filter f r1 where
    f = (<= fi maxStretchIn12Edo)
        . ((*) $ 12 * fi modulus / fi edo)
        . fi . fst . snd
  in mapM_ (\(spacing, (maxFretDiff, formatted)) -> do
              putStrLn $ "\nmod " ++ show modulus
                ++ ", spaced " ++ show spacing ++ "\\" ++ show edo
                ++ ", max reach: " ++ show maxFretDiff
                ++ ", or in 12-edo, "
                ++ show ( fi (12 * maxFretDiff * modulus)
                         / fi edo)
              myPrint $ formatted )
     r2

thanosReport :: Modulus -> Spacing -> Edo
             -> (FretDistance, [((Interval, Rational), (String, Fret))])
thanosReport modulus spacing edo = let
  notes :: [(Interval, Rational)] =
    primeIntervals edo
  results :: [(String, Fret)] =
    map (shortest modulus spacing . fst) notes
  maxFretDiff :: FretDistance = let
    results' = 0 : map snd results
    in maximum results' - minimum results'
  formatted = zip notes results
  in (maxFretDiff, formatted)

thanosReport' :: Modulus -> Spacing -> Edo
             -> ThanosReport
thanosReport' modulus spacing edo = let
  (fd, pairPairs) = thanosReport modulus spacing edo
  f ((a,b),(c,d)) = IntervalReport
    { ir_InEdo  = a
    , ir_Ratio  = b
    , ir_String = c
    , ir_Fret   = d }
  in ThanosReport
     { report_fretDistance = fd
     , report_intervalReports = map f pairPairs }

-- | On a guitar there can be multiple ways to play a given interval.
-- This gives the shortest one.
shortest :: Modulus
         -> Spacing
         -> Interval -- ^ A step of the Edo one would like to approximate.
         -- For instance, since Kite wanted to be able to reach 24\41 easily,
         -- his list surely included the number 24. (24\41 ~ 3/2).
         -> ( String, Fret )
shortest modulus spacing edoStep =
  let spaces = zip [0..] $ fmap (*spacing) [0..20]
      a = fmap (_2 %~ (edoStep -)) spaces
      b = filter ((== 0) . flip mod modulus . snd) a
      (string,fret) = minimumBy (comparing (abs . snd)) b
  in (string, div fret modulus)

-- | A modulus-spacing pair is feasible iff they are relatively prime.
-- For instance, in the Kite tuning (which is of course feasible),
-- the modulus is 2, and the spacing is 13.
feasibleSpacing :: Modulus -> Spacing -> Bool
feasibleSpacing modulus spacing =
  elem 1 $ fmap (flip mod modulus . (*) spacing) [1..modulus]

primeIntervals :: Edo
           -> [ ( Interval
                , Rational)] -- ^ The ratio the Interval represents.
primeIntervals edo = let
  primes = [5/4, 11/8, 3%2, 13/8, 7/4, 2]
  edoValues = map (fi . (^. _1) . best (fi edo)) primes
  in zip edoValues primes
