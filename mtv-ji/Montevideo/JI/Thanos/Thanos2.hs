-- This is hopefully better than Thanos.

module Montevideo.JI.Thanos.Thanos2 where

import Prelude hiding (span)

import           Control.Lens
import qualified Data.List as L
import           Data.Ord
import qualified Data.Set as S

import Montevideo.Util hiding (tr)
import Montevideo.JI.Thanos.Thanos
import Montevideo.JI.Lib


-- | = User-facing
--
-- For some handy code snippets, see the neighboring file
--   mtv-ji/Montevideo/JI/Thanos/thanos2-handy.hs

bestLayout' :: Int -> Edo -> Int -> Int -> (Area', [LayoutRow'])
bestLayout' oddLimit edo stringGap fretGap =
  bestLayout (edo, stringGap, fretGap) (primesOctave1 oddLimit)
  & _2 %~ map LayoutRow'
  & _1 %~ Area'

-- | Every Edo has a Bosanquet layout,
-- but for some the two basis vectors are not relatively prime.
-- In that case the layout does not cover the entire space,
-- and this function will not terminate.
bosanquet :: Edo -> ((Interval, Interval),
                      ((Int, Int, Int, Float),
                        [LayoutRow']))
bosanquet edo = let
  (stringGap,_,_) = best edo $ (9/8)
  (bestFourth,_,_) = best edo $ (4/3)
  fretGap = bestFourth - 2*stringGap
  in ( (fretGap, stringGap)
     , bestLayout (edo,stringGap,fretGap)
       (primesOctave1 31) & _2 %~ map LayoutRow' )


-- | = Not user-facing

type GString = Int -- ^ Guitar string
type GFret = Int -- ^ Guitar fret
type EdoInterval = Int
type Tuning = ( Edo
              , EdoInterval   -- ^ the string gap
              , EdoInterval ) -- ^ the fret gap
type LayoutRow = (EdoInterval, Rational, GString, GFret)
type Layout = [LayoutRow]

-- | This used to just be the area; now it includes a few other statistics.
-- The name ought to be different,
-- but that would complicate the names of some functions that use it.
type Area = ( Int   -- ^ area
            , Int   -- ^ string width
            , Int   -- ^ fret width
            , Float ) -- ^ fret where octave lies, as a %

-- | For positive ratios, gives the nearest (string, fret)
-- position of the ratio such that the fret is positive.
-- PITFALL: Fails (head of []) if the gaps are not relatively prime.
guitarSpot :: EdoInterval -> EdoInterval -> EdoInterval
           -> (GString, GFret)
guitarSpot stringGap fretGap interval =
  head [ (string, fret) |
         fret <- [0..],
         let evenMultiple = interval - fret*fretGap
             string = div evenMultiple stringGap,
         mod evenMultiple stringGap == 0 ]

-- | The canonical layout,
-- with all frets positive and minimal.
baseLayout :: Tuning -> [Rational] -> Layout
baseLayout (edo, stringGap, fretGap) ratios =
  [ (edoStep, r, string, fret) |
    r <- ratios,
    let edoStep = best edo r ^. _1
        (string, fret) = guitarSpot stringGap fretGap edoStep ]

descendingFrets :: Layout -> [GFret]
descendingFrets gSpots =
  reverse
  $ S.toList . S.fromList -- uniquifies, and sorts low to high
  $ filter (> 0)
  $ map (^. _4) gSpots

layouts :: Tuning  -> [Rational] -> [Layout]
layouts tuning@(_, stringGap, fretGap) ratios =
  let l = baseLayout tuning ratios
      frets = descendingFrets l
      bumpMaxFrets gSpots maxFret = let
        bump s@(edoStep, r, string, fret) =
          if fret == maxFret
          then (edoStep, r, string + fretGap, fret - stringGap)
          else s
        in map bump gSpots
  in scanl bumpMaxFrets l frets

bestLayout :: Tuning -> [Rational] -> (Area, Layout)
bestLayout tuning ratios =
  let ls = layouts tuning ratios
      areaFunc :: Layout -> Area
      areaFunc l = let
        strings = map (^. _3) l
        frets = map (^. _4) l
        stringWidth = maximum strings - minimum strings
        fretWidth = maximum frets - minimum frets
        octaveFret = fi ((l !! 1 ^. _4) - minimum frets)
                     / fi fretWidth
        area = if stringWidth > 7 then 9999999 else
                 (2 +
                  -- PITFALL: Hard-coded parameter.
                  -- Adding something like 2 to the string width penalizes wide frets more than wide strings. Relevant for guitar, not for keyboard.
                  stringWidth) * fretWidth
        in (area, stringWidth, fretWidth, octaveFret)
      l0 = L.minimumBy (comparing areaFunc) ls
  in (areaFunc l0, l0)

-- | PITFALL: Hard-coded parameters,
-- and commented-out guitar optimization.
tunings :: Edo -> [Tuning]
tunings edo = let e = fi edo in
  [ ( edo, stringGap, fretGap ) |
    fretGap <- [ 1 .. round (e * (5/12) :: Float ) ],
    stringGap <- [ fretGap .. round (e * (5/12) ) ],
      -- sometimes I start it from `(round $ e / 6)`
      -- rather than `fretGap`
    fretGap < stringGap,
    relativelyPrime fretGap stringGap
    -- The rest of this is to keep it guitar-friendly
    --let fretsPerOctave = fi edo / fi fretGap,
    --fretsPerOctave >= 8,
    --fretsPerOctave <= 27
  ]

tuningAreaLayouts :: Edo -> [Rational]
                  -> [(Tuning, Area, Layout)]
tuningAreaLayouts edo ratios =
  let ts :: [Tuning] = tunings edo
      bestAreaLayout t = let
        (area,layout) = bestLayout t ratios
        in (t, area, layout)
  in map bestAreaLayout ts

edoTuningAreaLayouts :: [Rational] -> [Edo]
                     -> [(Edo, Tuning, Area, Layout)]
edoTuningAreaLayouts rs edos = let
  etal :: Edo -> [(Edo, Tuning, Area, Layout)]
  etal edo = let
    f (tuning, area, layout) =
      (edo, tuning, area, layout)
    in map f $ tuningAreaLayouts edo rs
  in L.sortOn (^. _3) $ concatMap etal edos

bestEdoLayouts :: [Rational] -> [Edo] -> [EdoTuningReport]
bestEdoLayouts rs es  = let
  etals :: [(Edo, Tuning, Area, Layout)]
    = edoTuningAreaLayouts rs es
  in map mkEdoTuningReport etals

data EdoTuningReport = EdoTuningReport {
  etrEdo :: Edo,
  etrTuning :: Tuning',
  etrArea :: Area',
  etrLayout :: [LayoutRow'] }
  deriving (Show, Eq, Ord)

mkEdoTuningReport :: (Edo, Tuning, Area, Layout)
                  -> EdoTuningReport
mkEdoTuningReport (edo,tuning,area,layout) =
  EdoTuningReport { etrEdo = edo,
                    etrTuning = Tuning' tuning,
                    etrArea = Area' area,
                    etrLayout = map LayoutRow' layout }

data Tuning' = Tuning' Tuning
  deriving (Eq, Ord)
instance Show Tuning' where
  show (Tuning' (e, sg, fg)) =
    "Edo "         ++ show e ++
    "; StringGap " ++ show sg ++
    "; FretGap "   ++ show fg
unTuning :: Tuning' -> Tuning
unTuning (Tuning' t) = t

data Area' = Area' Area
  deriving (Eq, Ord)
instance Show Area' where
  show (Area' (_, sw, fw, oct)) =
    "strings x frets: " ++ show sw ++ " x " ++ show fw ++
    ", octave fraction: " ++ show oct
unArea :: Area' -> Area
unArea (Area' a) = a

data LayoutRow' = LayoutRow' LayoutRow
  deriving (Eq, Ord)
instance Show LayoutRow' where
  show (LayoutRow' (i,r,s,f)) =
    show i ++ " steps; " ++ show r ++
    "; string " ++ show s ++
    "; fret " ++ show f
unLayoutRow' :: LayoutRow' -> LayoutRow
unLayoutRow' (LayoutRow' r) = r
