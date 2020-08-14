-- | Thanos tunings are high-EDO tunings for stringed instruments,
-- in which one skips some frets.
-- Each tuning is defined by the number of frets skipped (the "modulus"),
-- and the space in EDO steps between adjacent strings (the "spacing").
--
-- TODO:  put more detail in `effs`.
-- Make a new type, EdoReport, that details:
--   edo error
--   max fret distance of best tuning, in 12-edo and (edo/modulus)-edo
--   spacing, in 12-edo and (edo/modulus)-edo

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Montevideo.JI.Thanos.Thanos where

import Prelude hiding (span, String) -- PITFALL: Hiding String.

import Control.Lens
import           Data.List hiding (span)
import           Data.Maybe
import           Data.Ord
import           Data.Ratio

import Montevideo.Util hiding (tr)


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

data EdoReport = EdoReport
  { eReport_edo :: Edo
  , eRrport_fretsPerOctave :: Float -- ^ e.g. 20.5 for the Kite tuning
  , eRrport_MSE :: Float
  , eReport_spacing :: Spacing
  , eReport_spacing12 :: Float
  , eReport_fretSpan :: FretDistance
  , eReport_fretSpan12 :: Float }
  deriving (Eq, Ord, Show)

data ThanosReport = ThanosReport
  { tReport_edo :: Edo
  , tReport_modulus :: Modulus
  , tReport_spacing :: Spacing
  , tReport_spacing12 :: Float
  , tReport_fretSpan :: FretDistance
  , tReport_fretSpan12 :: Float
  , tReport_intervalReports :: [IntervalReport] }
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

minEdo, maxEdo, errorAsGoodAs :: Int
minEdo :: Int = 30 -- ^ Don't consider any edos smaller than this.
maxEdo :: Int = 200 -- ^ Don't consider any edos bigger than this.
errorAsGoodAs :: Int = 12 -- ^ If 12, restrict search to edos with an MSE no greater than that of 12-edo

minFretsPerOctave, maxFretsPerOctave, max12edoFretSpan, minSpacingIn12edo :: Float
minFretsPerOctave = 15
maxFretsPerOctave = 35
max12edoFretSpan = 5 -- ^ Drop edos for which some 13-limit interval requires a stretch greater than this many frets of 12-edo.
minSpacingIn12edo = 3 -- ^ If this is 3, consider only spacings
  -- between strings that are at least 3\12.


-- * Do it.

-- | myPrint $ take 10 $ L.sortBy (comparing (^. eRrport_MSE)) effs
effs :: [ ( Edo
          , Float
          , FretDistance -- ^ PITFALL: Measured in 12-edo frets.
          )] = let
  efs :: [(Edo, Float)] =
    sortBy (comparing snd) $
    filter ((< edoError errorAsGoodAs) . snd) $
    filter ((> minEdo) . fst) $
    edoErrors maxEdo
  f :: (Edo, Float) -> Maybe (Edo, Float, FretDistance)
  f (e,err) = let mSpan = case bestTunings e of
                    [] -> Nothing
                    (a:_) -> Just $ tReport_fretSpan a
              in case mSpan of Nothing -> Nothing
                               Just span -> Just (e,err,span)
  in catMaybes $ map f efs

betterThanEffs = let
  in map asReport $ catMaybes $ map bestTuning [minEdo .. maxEdo]

bestTuning :: Edo -> Maybe (Edo, Float, ThanosReport)
bestTuning edo = case bestTunings edo of
  []     -> Nothing
  (tr:_) -> Just (edo, edoError edo, tr)
asReport :: (Edo, Float, ThanosReport) -> EdoReport
asReport (edo,mse,tr) = EdoReport
  { eReport_edo = edo
  , eRrport_fretsPerOctave = fi edo / fi (tReport_modulus tr)
  , eRrport_MSE = mse
  , eReport_spacing = tReport_spacing tr
  , eReport_spacing12 = tReport_spacing12 tr
  , eReport_fretSpan = tReport_fretSpan tr
  , eReport_fretSpan12 = tReport_fretSpan12 tr }

-- | How I'm using this:
-- x = bestTunings 87 -- generate some results
-- Pr.pPrint $ x !! 0 -- look at the first one (or the second, etc.)
bestTunings :: Edo -> [ThanosReport]
bestTunings edo =
  -- sortBy   (comparing             tReport_fretSpan)
  -- sortBy (comparing $ (*(-1)) . tReport_spacing)
  filter ( (\fpo -> fpo >= minFretsPerOctave &&
                       fpo <= maxFretsPerOctave) .
              (\tr -> fi (tReport_edo tr) / fi (tReport_modulus tr) ) )
  $ filter ( (< max12edoFretSpan) . tReport_fretSpan12)
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
     { tReport_edo = edo
     , tReport_modulus = modulus
     , tReport_spacing = spacing
     , tReport_spacing12 = fi spacing * 12 / fi edo
     , tReport_fretSpan = fd
     , tReport_fretSpan12 = reportSpan_in12Edo edo modulus fd
     , tReport_intervalReports = map f pairPairs }

thanosReport' :: Edo -> Modulus -> Spacing
              -> (FretDistance, [((Interval, Rational), (String, Fret))])
thanosReport' edo modulus spacing = let
  notes :: [(Interval, Rational)] =
    primeIntervals edo
  layout :: [[(String, Fret)]] =
    -- Each inner list represents the closest places a given prime lies.
    -- Most of them will probably be length 1.
    map (shorts modulus spacing . fst) notes
  choices :: [[(String,Fret)]] = do -- list monad
    -- Each inner list has length 6, corresponding to the 6 primes,
    -- and represents an available choice of which prime to play where.
    -- TODO There must be a more elegant way to do this. foldM?
    let [a,b,c,d,e,f] :: [[(String,Fret)]] = layout
    a' <- a
    b' <- b
    c' <- c
    d' <- d
    e' <- e
    f' <- f
    return [a',b',c',d',e',f']
  maxFretDiff :: [(String,Fret)] -> FretDistance
  maxFretDiff choice = let
    frets = 0 : map snd choice
    in maximum frets - minimum frets
  choice = minimumBy (comparing maxFretDiff) choices
  formatted = zip notes choice
  in (maxFretDiff choice, formatted)

reportSpan_in12Edo :: Edo -> Modulus -> FretDistance -> Float
reportSpan_in12Edo e m d =
  12 * fi d * fi m / fi e

-- | On a guitar there can be multiple ways to play a given interval.
-- This gives the shortest ones.
-- Specifically, it gives all that are no more than twice as big as the shortest.
-- That's better than just keeping the shortest one,
-- because in the case of ties or near-ties,
-- the bigger one might work better with the other intervals.

shorts :: Modulus
       -> Spacing
       -> Interval -- ^ A step of the Edo one would like to approximate.
       -- For instance, since Kite wanted to be able to reach 24\41 easily,
       -- his list surely included the number 24. (24\41 ~ 3/2).
       -> [(String, Fret)]

shorts modulus spacing edoStep = let
  spaceMultiples = take 8 $ zip [1..] $ fmap (*spacing) [1..]
    -- The list starts at 1, not 0, because I don't want
    -- string 0 to be a candidate for where to play the note,
    -- since it's already busy playing the root frequency.
    -- The list ends at 8 because one probably wants
    -- to be able to play an octave within 8 strings.
  a = fmap (_2 %~ (edoStep -)) spaceMultiples
  b = filter ((== 0) . flip mod modulus . snd) a
  fewestFrets = minimum $ map (abs . snd) b
  fewFrets = filter (\(_,frets) -> abs frets <= 2 * fewestFrets) b
  in map (_2 %~ flip div modulus) fewFrets

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
