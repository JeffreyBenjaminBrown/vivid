-- | Thanos tunings are high-EDO tunings for stringed instruments,
-- in which one skips some frets.
-- Each tuning is defined by the number of frets skipped (the "modulus"),
-- and the space in EDO steps between adjacent strings (the "spacing").

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Montevideo.JI.Thanos.Thanos where

import Prelude hiding (span)

import Control.Lens
import           Data.List hiding (span)
import           Data.Ord
import           Data.Ratio

import Montevideo.Util hiding (tr)
import Montevideo.JI.Thanos.SearchParams
import Montevideo.JI.Lib


-- * Types

type Modulus = Int -- ^ The Thanos tuning will keep every nth note, where n is the modulus. For instance, in Kite's tuning the modulus is 2 (and the Edo is 41).

-- | Also a value from the Edo system, with the particular interpretation
-- that it rerpresents the space between adjacent strings.
-- For instance, in the Kite tuning, this would be 13, because
-- on a Kite guitar each pair of adjacent strings is tuned 13\41 apart.
type Spacing = Interval

type GuitarString = Int
type Fret = Int
type FretDistance = Int -- ^ The distance between two frets.

data ThanosReport = ThanosReport
  { tReport_edo :: Edo
  , tReport_fpo :: Float -- ^ frets per octave
  , tReport_modulus :: Modulus
  , tReport_spacing :: Spacing
  , tReport_spacing12 :: Float
  , tReport_fretSpan_lim5 :: FretDistance
  , tReport_fretSpan12_lim5 :: Float
  , tReport_fretSpan_lim7 :: FretDistance
  , tReport_fretSpan12_lim7 :: Float
  , tReport_fretSpan_lim13 :: FretDistance
  , tReport_fretSpan12_lim13 :: Float
  , tReport_intervalReports :: [IntervalReport] }
  deriving (Eq, Ord, Show)

data IntervalReport = IntervalReport
  { ir_Edo    :: Interval -- ^ This ...
  , ir_Ratio  :: Rational -- ^ and this represent the same note
  , ir_GuitarString :: GuitarString   -- ^ This ...
  , ir_Fret   :: Fret }   -- ^ and this represent where it lies closest.
  deriving (Eq, Ord)

instance Show IntervalReport where
  show r = show (ir_Edo r) ++ " steps = " ++ show (ir_Ratio r)
           ++ ": string "  ++ show (ir_GuitarString r)
           ++ " fret "     ++ show (ir_Fret r)


-- ** Search

-- | All tunings that satisfy the search parameters.
validTunings :: [Int]
validTunings = [ a
               | a <- [minEdo .. maxEdo]
               , not $ null $ go a ]

-- | Edit his function (and the parameters like "maxEdo" above)
-- to suit your search priorities.
--
-- Pr.pPrint $ reverse $ go 130
go :: Int -> [ThanosReport]
go a = id
  $ sortBy   (comparing                    tReport_fretSpan_lim13)
  $ sortBy   (comparing $ (*(-1))        . tReport_spacing)
  $ sortBy   (comparing                    tReport_fretSpan_lim7)
  $ sortBy   (comparing                    tReport_fretSpan_lim5)

  $ filter ( (<= max12edoFretSpan_lim13) . tReport_fretSpan_lim13)
  $ filter ( (>= minSpacingIn12edo)      . tReport_spacing12 )

--  $ filter ( (>= minFretsPerOctave)      . tReport_fpo)
--  $ filter ( (<= maxFretsPerOctave)      . tReport_fpo)
--  $ filter ( (<= max12edoFretSpan_lim7)  . tReport_fretSpan_lim7)
--  $ filter ( (<= max12edoFretSpan_lim5)  . tReport_fretSpan_lim5)
--  $ filter ( (<= 2 * bestFifth 12)       . bestFifth
--                                         . tReport_edo )
--  $ filter ((< 0.2) . edoError           . tReport_edo)
  $ concatMap edoThanosReports
  [a]
  -- [minEdo .. maxEdo]

-- | PITFALL: This looks like it generates an impossibly big amount of data.
-- Fortunately, laziness upstream means most reports are dropped
-- after only being very slightly evaluated.
edoThanosReports :: Edo -> [ThanosReport]
edoThanosReports edo =
  [ thanosReport edo modulus spacing
  | modulus <- [1 .. maxModulus]
  , spacing <- [1 .. div edo 2]
    -- The top of this range restricts spacings to ones that are
    -- no greater than a tritone apart.
  , relativelyPrime modulus spacing ]

-- | To find the five best (in terms of MSE) EDOs below 100:
-- > myPrint $ take 5 $ L.sortBy (comparing snd) $ edoErrors 100
-- (87,4.2125642e-2)
-- (46,6.6611946e-2)
-- (41,7.22863e-2)
-- (94,7.296734e-2)
-- (31,7.34341e-2)
edoErrors :: Int -> [(Edo, Float)]
edoErrors n =
  sortBy (comparing snd) $
  [ (e, edoError e)
  | e <- [1..n] ]

edoError :: Edo -> Float
edoError e = let
  component :: (Int, Rational) -> Float
  component (prime, r) = let
    lr :: Float = log (fromRational r) / log 2
    in (fi (round $ fi e * lr) / fi e - lr)**2 -- Approximation error.
       / log (fi prime) -- So simpler intervals weigh more.
       * (fi e)**2 -- Measures the error (I think) in terms of edo steps,
                   -- putting big and small edos on roughly equal footings.
  in sum $ map component $ primesAnd1 31


-- * Generate a report

thanosReport :: Edo -> Modulus -> Spacing
             -> ThanosReport
thanosReport edo modulus spacing = let
  (fd5, fd7, fd13, pairPairs) = thanosReport' edo modulus spacing
  mkIntervalReport ((a,b),(c,d)) = IntervalReport
    { ir_Edo  = a
    , ir_Ratio  = b
    , ir_GuitarString = c
    , ir_Fret   = d }
  in ThanosReport
     { tReport_edo = edo
     , tReport_fpo = fi edo / fi modulus
     , tReport_modulus = modulus
     , tReport_spacing = spacing
     , tReport_spacing12 = fi spacing * 12 / fi edo
     , tReport_fretSpan_lim5 = fd5
     , tReport_fretSpan12_lim5 = reportSpan_in12Edo edo modulus fd5
     , tReport_fretSpan_lim7 = fd7
     , tReport_fretSpan12_lim7 = reportSpan_in12Edo edo modulus fd7
     , tReport_fretSpan_lim13 = fd13
     , tReport_fretSpan12_lim13 = reportSpan_in12Edo edo modulus fd13
     , tReport_intervalReports =
       sortBy (comparing $ Data.Ratio.numerator . ir_Ratio) $
       map mkIntervalReport pairPairs }

reportSpan_in12Edo :: Edo -> Modulus -> FretDistance -> Float
reportSpan_in12Edo e m d =
  12 * fi d * fi m / fi e

thanosReport'
  :: Edo -> Modulus -> Spacing
  -> ( FretDistance -- ^ 5-limit maximum prime fret reach
     , FretDistance -- ^ 7-limit maximum prime fret reach
     , FretDistance -- ^ 13-limit maximum prime fret reach
     , [((Interval, Rational), (GuitarString, Fret))])
thanosReport' edo modulus spacing = let
  notes :: [(Interval, Rational)] =
    primeIntervals 31 edo
  layout :: [[(GuitarString, Fret)]] =
    -- The outer list has length 6.
    -- Each inner list represents the closest places a given prime lies.
    -- Most of them will probably be length 1.
    map (shortWaysToReach modulus spacing . fst) notes
  cs :: [[(GuitarString,Fret)]] = choices layout
  maxFretDiff :: Int --how many primesAnd1 to use: 4 (7-limit) or 6 (13-limit)
              -> [(GuitarString,Fret)] -> FretDistance
  maxFretDiff n choice = let
    frets = 0 : map snd (take n choice)
    in maximum frets - minimum frets
  theChoice = minimumBy (comparing $ maxFretDiff 6) cs
  formatted = zip notes theChoice
  in ( maxFretDiff 3 theChoice,
       maxFretDiff 4 theChoice,
       maxFretDiff 6 theChoice,
       formatted )

-- | Each inner list of `choices ll` is a different way of selecting one
-- element from each of the inner lists of `ll`.
-- The length of each inner list of `choices ll` is the length of `ll`.
-- For instance,
-- > choices [[1],[2,3],[4,5,6]]
-- [[1,2,4],[1,2,5],[1,2,6],[1,3,4],[1,3,5],[1,3,6]]

choices :: [[a]] -> [[a]]
choices []       = [[]]
choices (xs:xss) = do y  <- xs
                      ys <- choices xss
                      return (y:ys)

-- | On a guitar there can be multiple ways to play a given interval.
-- This gives the shortest ones.
-- Specifically, it gives all that are no more than three times as big as the shortest.
-- That's better than just keeping the shortest one,
-- because in the case of ties or near-ties,
-- the bigger one might work better with the other intervals.

shortWaysToReach
  :: Modulus
  -> Spacing
  -> Interval -- ^ A step of the Edo one would like to approximate.
  -- For instance, since Kite wanted to be able to reach 24\41 easily,
  -- his list surely included the number 24. (24\41 ~ 3/2).
  -> [(GuitarString, Fret)]

shortWaysToReach modulus spacing edoStep = let
  spaceMultiples :: [(Int, Spacing)] = let
    strings = let mm = max (modulus*2) maxModulus
              in [-mm .. mm]
    in (if isForGuitar then filter ((/= 0) . fst) else id) $
       -- If searching for guitar tunings, then we don't want
       -- string 0 to be a candidate for where to play the note,
       -- since that string is already busy playing the root note.
       -- Number of strings must be at least the modulus,
       -- or else some notes might be unplayable.
       zip strings $ fmap (*spacing) strings
  targeting :: [(Int, Spacing)] = fmap (_2 %~ (edoStep -)) spaceMultiples
    -- Whatever in this list has a `snd` closest to 0,
    -- its `fst` is a good first guess for which string we want.
  existant = filter ((== 0) . flip mod modulus . snd) targeting
    -- Only these solutions from "targeting" actually exist.
  fewestFrets = minimum $ map (abs . snd) existant
  fewFrets = filter f existant where
    f (_,frets) = abs frets <= max
                  (3 * fewestFrets) alwaysConsiderAtLeastThisManyFrets
  in map (_2 %~ flip div modulus) fewFrets

-- | PITFALL: This is BAD for big numbers.
-- But I doubt it matters for my purposes.
-- todo ? speed
relativelyPrime :: Modulus -> Spacing -> Bool
relativelyPrime modulus spacing =
  elem 1 [modulus, spacing]
  || elem 1 ( fmap
              (flip mod modulus . (*) spacing)
              [1..modulus] )

-- | How to play the primesAnd1 in a given edo.
primeIntervals :: Edo
               -> Int
               -> [ ( Interval
                    , Rational)] -- ^ The ratio the Interval represents.
primeIntervals edo oddLimit = let
  edoValues = map (best' edo) $ primesOctave1 oddLimit
  in zip edoValues $ primesOctave1 oddLimit

primesOctave1 :: Int -> [Rational]
primesOctave1 = map snd . primesAnd1

-- * These are the intervals I care about.
-- 2 should be included because if you can't easily reach the octave,
-- it's not much help to be able to reach the other notes,
-- unless you never plan on inverting your chords.
--
-- TODO These aren't primes. Rename this and upstream callers accordingly.
primesAnd1 :: Int -> [(Int, Rational)]
primesAnd1 oddLimit =
  takeWhile ( (<= oddLimit) . fst ) $
  [ (1, 1%1),
    (2, 2%1) ] ++
  [ (i, fi i % fi (2 ^ floor (log (fi i) / log 2) ) ) |
    i <- [3,5..] ]

badness :: Int -> Int -> Edo -> Float
badness oddLimit nPrimes e = let
  primesHere :: [(Int, Rational)] = take nPrimes $ primesAnd1 oddLimit
  in sum $ map (abs . bestError e) $
     [ fi n / fi d
     | d <- [1 .. maximum $ map fst primesHere]
     , n <- [1 .. d] ]

bestFifth :: Edo -> Float
bestFifth edo = abs $ bestError edo $ 3/2

-- | PITFALL: This is the signed error.
-- Take its absolute value, or square it, or etc. before minimizing it.
bestError :: Edo -> Rational -> Float
bestError e r = fi (best' e r) / fi e - log (fr r) / log 2
