-- This is hopefully better than Thanos.

module Montevideo.JI.Thanos.Thanos2 where

import Prelude hiding (span)

import           Control.Lens
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Ord
import           Data.Ratio
import qualified Data.Set as S

import Montevideo.Util hiding (tr)
import Montevideo.JI.Thanos.Thanos
import Montevideo.JI.Lib


type GString = Int -- ^ Guitar string
type GFret = Int -- ^ Guitar fret
type EdoInterval = Int
type Tuning = ( Edo
              , EdoInterval   -- ^ the string gap
              , EdoInterval ) -- ^ the fret gap
type LayoutRow = (EdoInterval, Rational, GString, GFret)
type Layout = [LayoutRow]
type Area = Int

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
layouts tuning@(edo, stringGap, fretGap) ratios =
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
bestLayout tuning @ (edo, stringGap, fretGap) ratios =
  let ls = layouts tuning ratios
      area :: Layout -> Int
      area l = let
        strings = map (^. _3) l
        frets = map (^. _4) l
        in (maximum strings - minimum strings)
           * (maximum frets - minimum frets)
      l = L.minimumBy (comparing area) ls
  in (area l, l)

-- | PITFALL: Hard-coded parameters.
tunings :: Edo -> [Tuning]
tunings edo = let e = fi edo in
  [ ( edo, stringGap, fretGap ) |
    fretGap <- [ 1 .. round (e * (5/12) ) ],
    stringGap <- [ round (e / 6) .. round (e * (5/12) ) ],
    relativelyPrime fretGap stringGap,
    -- The rest of this is to keep it guitar-friendly
    let fretsPerOctave = fi edo / fi fretGap,
    fretsPerOctave >= 8,
    fretsPerOctave <= 27 ]

tuningsAndLayouts :: Edo -> [Rational]
                  -> [(Tuning, Area, Layout)]
tuningsAndLayouts edo ratios =
  let ts :: [Tuning] = tunings edo
      f t = let (area, layout) = bestLayout t ratios
            in (t, area, layout)
  in L.sortOn (^. _2) $ map f ts

bestTuningsAndLayout :: Edo -> [Rational]
                     -> (Tuning, Area, Layout)
bestTuningsAndLayout e rs = head $ tuningsAndLayouts e rs

edosTuningsAndLayouts :: [Rational] -> [Edo]
                      -> [(Edo, Tuning, Area, Layout)]
edosTuningsAndLayouts rs edos = let
  etal edo = let (tuning, area, layout) =
                   bestTuningsAndLayout edo rs
             in (edo, tuning, area, layout)
  in L.sortOn (^. _3) $ map etal edos

-- | PITFALL: Hard-coded parameters.
bestEdoLayouts :: [EdoTuningReport]
bestEdoLayouts = let
  etals :: [(Edo, Tuning, Area, Layout)]
    = edosTuningsAndLayouts (primesOctave1 15) [41..99]
  in map mkEdoTuningReport etals

data EdoTuningReport = EdoTuningReport {
  etrEdo :: Edo,
  etrTuning :: Tuning',
  etrArea :: Area,
  etrLayout :: [LayoutRow'] }
  deriving (Show, Eq, Ord)

mkEdoTuningReport :: (Edo, Tuning, Area, Layout)
                  -> EdoTuningReport
mkEdoTuningReport (edo,tuning,area,layout) =
  EdoTuningReport { etrEdo = edo,
                    etrTuning = Tuning' tuning,
                    etrArea = area,
                    etrLayout = map LayoutRow' layout }

data Tuning' = Tuning' Tuning
  deriving (Eq, Ord)
instance Show Tuning' where
  show (Tuning' (e, sg, fg)) =
    "Edo "         ++ show e ++
    "; StringGap " ++ show sg ++
    "; FretGap "   ++ show fg

data LayoutRow' = LayoutRow' LayoutRow
  deriving (Eq, Ord)
instance Show LayoutRow' where
  show (LayoutRow' (i,r,s,f)) =
    show i ++ " steps; " ++ show r ++
    "; string " ++ show s ++
    "; fret " ++ show f
