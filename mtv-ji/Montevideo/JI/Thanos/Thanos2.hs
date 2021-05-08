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
type Layout = [(EdoInterval, GString, GFret)]
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
  [ (edoStep, string, fret) |
    r <- ratios,
    let edoStep = best edo r ^. _1
        (string, fret) = guitarSpot stringGap fretGap edoStep ]

descendingFrets :: Layout -> [GFret]
descendingFrets gSpots =
  reverse
  $ S.toList . S.fromList -- uniquifies, and sorts low to high
  $ filter (> 0)
  $ map (^. _3) gSpots

layouts :: Tuning  -> [Rational] -> [Layout]
layouts tuning@(edo, stringGap, fretGap) ratios =
  let l = baseLayout tuning ratios
      frets = descendingFrets l
      bumpMaxFrets gSpots maxFret = let
        bump s@(edoStep, string, fret) =
          if fret == maxFret
          then (edoStep, string + fretGap, fret - stringGap)
          else s
        in map bump gSpots
  in scanl bumpMaxFrets l frets

bestLayout :: Tuning -> [Rational] -> (Area, Layout)
bestLayout tuning @ (edo, stringGap, fretGap) ratios =
  let ls = layouts tuning ratios
      area :: Layout -> Int
      area l = let
        strings = map (^. _2) l
        frets = map (^. _3) l
        in (maximum strings - minimum strings)
           * (maximum frets - minimum frets)
      l = L.minimumBy (comparing area) ls
  in (area l, l)
