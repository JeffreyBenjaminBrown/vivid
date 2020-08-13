-- | Thanos tunings are high-EDO tunings for stringed instruments,
-- in which one skips some frets.
-- Each tuning is defined by the number of frets skipped (the "modulus"),
-- and the space in EDO steps between adjacent strings (the "spacing").

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Montevideo.JI.Thanos where

import Prelude hiding (String) -- PITFALL: Weird.

import Control.Lens
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Ratio

import Montevideo.Util


-- * Types

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
  { report_edo :: Edo
  , report_modulus :: Modulus
  , report_spacing :: Spacing
  , report_spacing12 :: Float
  , report_fretSpan :: FretDistance
  , report_fretSpan12 :: Float
  , report_intervalReports :: [IntervalReport] }
  deriving (Eq, Ord, Show)

data IntervalReport = IntervalReport
  { ir_Edo  :: Interval -- ^ This
  , ir_Ratio  :: Rational -- ^ and this represent the same note
  , ir_String :: String   -- ^ This
  , ir_Fret   :: Fret }   -- ^ and this represent where it lies closest.
  deriving (Eq, Ord)

instance Show IntervalReport where
  show r = show (ir_Edo r) ++ " steps = " ++ show (ir_Ratio r)
           ++ ": string "  ++ show (ir_String r)
           ++ " fret "     ++ show (ir_Fret r)


-- ** Search

-- * Configure the search

minEdo = 30 -- ^ Don't consider any edos smaller than this.
maxEdo = 200 -- ^ Don't consider any edos bigger than this.
errorAsGoodAs = 12 -- ^ If 12, restrict search to edos with an MSE no greater than that of 12-edo
max12edoFretSpan = 5 -- ^ Drop edos for which some 13-limit interval requires a stretch greater than this many frets of 12-edo.
minSpacingIn12edo = 3 -- ^ If this is 3, consider only spacings
  -- between strings that are at least 3\12.
minFretsPerOctave = 15
maxFretsPerOctave = 35


-- * Do it.

-- | myPrint $ L.sortBy (comparing (^. _3)) effs
effs :: [ (Edo
          , Float
          , FretDistance -- ^ PITFALL: Measured in 12-edo frets.
          )] = let
  efs :: [(Edo, Float)] =
    sortBy (comparing snd) $
    filter ((< edoError errorAsGoodAs) . snd) $
    filter ((> minEdo) . fst) $
    edoErrors maxEdo
  f :: (Edo, Float) -> Maybe (Edo, Float, FretDistance)
  f (e,err) = let span = case bestTunings e of
                    [] -> Nothing
                    (a:_) -> Just $ report_fretSpan a
              in case span of Nothing -> Nothing
                              Just span -> Just (e,err,span)
  in catMaybes $ map f efs

-- | How I'm using this:
-- x = bestTunings 87 -- generate some results
-- Pr.pPrint $ x !! 0 -- look at the first one (or the second, etc.)
bestTunings :: Edo -> [ThanosReport]
bestTunings edo =
  sortBy   (comparing             report_fretSpan)
  $ sortBy (comparing $ (*(-1)) . report_spacing)
  $ filter ( (\fpo -> fpo >= minFretsPerOctave &&
                      fpo <= maxFretsPerOctave) .
             (\tr -> fi (report_edo tr) / fi (report_modulus tr) ) )
  $ filter ( (< max12edoFretSpan) . report_fretSpan12)
  $ edoReports edo

edoReports :: Edo -> [ThanosReport]
edoReports edo =
  [ thanosReport edo modulus spacing
  | modulus <- [1..11] -- 20 seems like a safe upper limit.
    -- (The Kite tuning has a modulus of 2. Most tunings use a modulus of 1.)
  , spacing <- [floor (minSpacingIn12edo * fi edo / 12) .. div edo 2]
    -- The top of this range restricts spacings to ones that are
    -- no greater than a tritone apart.
  , feasibleSpacing modulus spacing ]

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
       / log (fi prime) -- So that lower primes weigh more.
       * (fi e)**2 -- Measures the error (I think) in terms of edo steps,
                   -- putting big and small edos on roughly equal footings.
  in sum $ map component primes


-- * Generate a report

thanosReport :: Edo -> Modulus -> Spacing
             -> ThanosReport
thanosReport edo modulus spacing = let
  (fd, pairPairs) = thanosReport' edo modulus spacing
  f ((a,b),(c,d)) = IntervalReport
    { ir_Edo  = a
    , ir_Ratio  = b
    , ir_String = c
    , ir_Fret   = d }
  in ThanosReport
     { report_edo = edo
     , report_modulus = modulus
     , report_spacing = spacing
     , report_spacing12 = fi spacing * 12 / fi edo
     , report_fretSpan = fd
     , report_fretSpan12 = reportSpan_in12Edo edo modulus fd
     , report_intervalReports = map f pairPairs }

thanosReport' :: Edo -> Modulus -> Spacing
              -> (FretDistance, [((Interval, Rational), (String, Fret))])
thanosReport' edo modulus spacing = let
  notes :: [(Interval, Rational)] =
    primeIntervals edo
  results :: [(String, Fret)] =
    map (shortest modulus spacing . fst) notes
  maxFretDiff :: FretDistance = let
    results' = 0 : map snd results
    in maximum results' - minimum results'
  formatted = zip notes results
  in (maxFretDiff, formatted)

reportSpan_in12Edo :: Edo -> Modulus -> FretDistance -> Float
reportSpan_in12Edo e m d =
  12 * fi d * fi m / fi e

-- | On a guitar there can be multiple ways to play a given interval.
-- This gives the shortest one.
--
-- TODO This is problematic for intervals that split the difference.
-- For instance, if you can reach up 7 or down 7, which does it pick?
-- Or maybe you can reach up 7 or down 8, but reaching down 8 actually
-- works better given the other intervals.
-- One possible solution: In the case of ties or near-ties,
-- just omit that interval when computing the max reach,
-- and let the user figure it out.
shortest :: Modulus
         -> Spacing
         -> Interval -- ^ A step of the Edo one would like to approximate.
         -- For instance, since Kite wanted to be able to reach 24\41 easily,
         -- his list surely included the number 24. (24\41 ~ 3/2).
         -> ( String, Fret )
shortest modulus spacing edoStep =
  let spaceMultiples = take 15 $ zip [1..] $ fmap (*spacing) [1..]
        -- The list starts at 1, not 0, because I don't want
        -- string 0 to be a candidate for where to play the note,
        -- since it's already busy playing the root frequency.
        -- 15 strings seems like a reasonable maximum to assume on a guitar.
      a = fmap (_2 %~ (edoStep -)) spaceMultiples
      b = filter ((== 0) . flip mod modulus . snd) a
      (string,fret) = minimumBy (comparing (abs . snd)) b
  in (string, div fret modulus)

-- | A modulus-spacing pair is feasible iff they are relatively prime.
-- For instance, in the Kite tuning (which is of course feasible),
-- the modulus is 2, and the spacing is 13.
feasibleSpacing :: Modulus -> Spacing -> Bool
feasibleSpacing modulus spacing =
  elem 1 $ fmap (flip mod modulus . (*) spacing) [1..modulus]

-- | How to play the primes in a given edo.
primeIntervals :: Edo
           -> [ ( Interval
                , Rational)] -- ^ The ratio the Interval represents.
primeIntervals edo = let
  edoValues = map (best edo) primesOctave1
  in zip edoValues primesOctave1

primesOctave1 :: [Rational]
primesOctave1 = map snd primes

-- * These are the primes I care about.
-- 2 should be included because if you can't easily reach the octave,
-- it's not much help to be able to reach the other notes,
-- unless you never plan on inverting your chords.
primes :: [(Int, Rational)]
primes =
  [ (5, 5/4)
  , (11,11/8)
  , (3, 3%2)
  , (13,13/8)
  , (7, 7/4)
  , (2, 2/1)
  ]

-- | Best approximation to a ratio in an edo.
-- For instance, best 12 (3/2) = 7
best :: Edo -> Rational -> Interval
best e r =
  round $ fi e * log (fr r) / log 2
