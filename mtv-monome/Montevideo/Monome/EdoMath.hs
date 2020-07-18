-- | Math for equal divisions of the octave.

{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module Montevideo.Monome.EdoMath (
    et31ToFreq      -- ^ EdoConfig -> Pitch EdoApp -> Float
  , xyToEt31        -- ^ EdoConfig -> (X,Y) -> Pitch
  , xyToEt31_st     -- ^ St EdoApp -> (X,Y) -> Pitch EdoApp
  , pcToXys         -- ^ EdoConfig -> PitchClass -> (X,Y) -> [(X,Y)]
  , et31ToLowXY     -- ^ EdoConfig -> PitchClass -> (X,Y)
  , vv, hv          -- ^ EdoConfig -> (X,Y)
  ) where

import Control.Lens

import           Montevideo.JI.Util (fromCents)
import           Montevideo.Monome.Types.Initial
import           Montevideo.Util


et31ToFreq :: EdoConfig -> Pitch EdoApp -> Float
et31ToFreq ec f =
  let two :: Float = realToFrac $ fromCents $
                     10 * (1200 + ec ^. octaveStretchInCents)
  in two**(fi f / fi (ec ^. edo) )

xyToEt31_st :: St EdoApp -> (X,Y) -> Pitch EdoApp
xyToEt31_st st xy =
  xyToEt31 (st ^. stApp . etConfig) $
  pairAdd xy $ pairMul (-1) $ _etXyShift $ _stApp st

-- | `pcToXys ec shift pc` finds all buttons that are enharmonically
-- equal to a given PitchClass, taking into account how the board
-- has been shifted in pitch space.
pcToXys :: EdoConfig -> (X,Y) -> PitchClass EdoApp -> [(X,Y)]
pcToXys ec shift pc = let
  onMonome :: (X,Y) -> Bool -- monome button coordinates are in [0,15]
  onMonome (x,y) = x >= 0  && y >= 0  &&
                   x <= 15 && y <= 15
  in filter onMonome $
     _enharmonicToXYs ec $
     pairAdd (et31ToLowXY ec pc) shift

-- | A superset of all keys that sound the same note
-- (modulo octave) visible on the monome.
--
-- TODO ? (speed) This computes a lot of out-of-range values.
-- (The higher the edo, the lesser this problem.)
_enharmonicToXYs :: EdoConfig -> (X,Y) -> [(X,Y)]
_enharmonicToXYs ec btn = let

  low = et31ToLowXY ec $ xyToEt31 ec btn
  ((v1,v2),(h1,h2)) = (vv ec, hv ec)
  wideGrid = [
    ( i*h1 + j*v1
    , i*h2 + j*v2 )
    | i <- [ 0 ..      div 15 h1 + 1] ,
      j <- [ -- `j` needs a wide range because `hv` might be diagonal.
                  -   (div 15 v2 + 1)
               .. 2 * (div 15 v2 + 1) ] ]
  in map (pairAdd low) wideGrid

-- | `xyToEt31 ec (x,y)` finds the pitch class that (x,y) corresponds to,
-- assuming the board has not been shifted.
--
-- In the PitchClass domain, xyToEt31 and et31ToLowXY are inverses:
-- xyToEt31 <$> et31ToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
xyToEt31 :: EdoConfig -> (X,Y) -> Pitch EdoApp
xyToEt31 ec (x,y) = _spacing ec * x
                    + _skip ec * y

-- | The numerically lowest (closest to the top-left corner)
-- member of a pitch class, if the monome is not shifted (modulo octaves).
--
-- In the PitchClass domain, xyToEt31 and et31ToLowXY are inverses:
-- xyToEt31 <$> et31ToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
et31ToLowXY :: EdoConfig -> PitchClass EdoApp -> (X,Y)
et31ToLowXY ec i = ( div j $ _spacing ec
                   , mod j $ _spacing ec )
  where j = mod i $ _edo ec

-- | `hv` and `vv` form The smallest, most orthogonal set of
-- basis vectors possible for the octave grid.
-- They are most easily understood via example. Suppose `C.edo = 31`,
-- and `C.spacing = 6`. Then to reach the next octave horizontally,
-- one must move right 5 spaces and down 1. To move to the same note
-- in the previous column, one must move down 6 and left 1.
-- Accordingly, the values for hv and vv are these:
-- > hv
-- (5,1)
-- > vv
-- (-1,6)
-- (Remember, coordinates on the monome are expressed CGI-style,
-- with (0,0) in the top-left corner.)

vv, hv :: EdoConfig -> (X,Y)
vv ec = (-1, _spacing ec)
hv ec = let
  edo = _edo ec
  spacing = _spacing ec
  x = -- the first multiple of spacing greater than or equal to edo
    head $ filter (>= edo) $ (*spacing) <$> [1..]
  v1 = (div x spacing, edo - x)
  v2 = pairAdd v1 $ vv ec
  in if abs (dot (vv ec) v1) <= abs (dot (vv ec) v2)
     then                v1  else                v2
