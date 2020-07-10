{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, OverloadedStrings
, TupleSections #-}

module Montevideo.Monome.Math31 (
    edo, spacing    -- ^ Num a => a
  , vv, hv          -- ^ (X,Y)
  , et31ToFreq      -- ^ Pitch -> Float
  , xyToEt31        -- ^ (X,Y) -> Pitch
  , xyToEt31_st     -- ^ St EdoApp -> (X,Y) -> Pitch EdoApp
  , et31ToLowXY     -- ^ PitchClass -> (X,Y)
  , enharmonicToXYs -- ^ (X,Y) -> [(X,Y)]
  , pcToXys         -- ^ PitchClass -> (X,Y) -> [(X,Y)]
  ) where

import Montevideo.JI.Util (fromCents)
import Montevideo.Monome.Config
import Montevideo.Monome.Types.Initial
import Montevideo.Util


-- | `hv` and `vv` form The smallest, most orthogonal set of
-- basis vectors possible for the octave grid.
vv, hv :: (X,Y)
vv = (-1,spacing)
hv = let
  x = -- the first multiple of spacing greater than or equal to edo
    head $ filter (>= edo) $ (*spacing) <$> [1..]
  v1 = (div x spacing, edo - x)
  v2 = pairAdd v1 vv
  in if abs (dot vv v1) <= abs (dot vv v2)
     then           v1  else           v2

et31ToFreq :: Pitch EdoApp -> Float
et31ToFreq f =
  let two :: Float = realToFrac $ fromCents $
                     10 * (1200 + octaveStretchInCents)
  in two**(fi f / edo)

-- | on the PitchClass domain, xyToEt31 and et31ToLowXY are inverses:
-- xyToEt31 <$> et31ToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
xyToEt31 :: (X,Y) -> Pitch EdoApp
xyToEt31 (x,y) = spacing * x + (skip*y)

xyToEt31_st :: St EdoApp -> (X,Y) -> Pitch EdoApp
xyToEt31_st st xy =
  xyToEt31 $ pairAdd xy $ pairMul (-1) $ _etXyShift $ _stApp st

-- | The numerically lowest (closest to the top-left corner)
-- member of a pitch class, if the monome is not shifted (modulo octaves).
et31ToLowXY :: PitchClass EdoApp -> (X,Y)
et31ToLowXY i = (div j spacing, mod j spacing)
  where j = mod i edo

-- | A (maybe proper) superset of all keys that sound the same note
-- (modulo octave) visible on the monome.
--
-- TODO ? (speed) This computes a lot of out-of-range values.
-- (The higher the edo, the lesser this problem.)
enharmonicToXYs :: (X,Y) -> [(X,Y)]
enharmonicToXYs btn = let
  low = et31ToLowXY $ xyToEt31 btn
  ((v1,v2),(h1,h2)) = (vv,hv)
  wideGrid = [
    ( i*h1 + j*v1
    , i*h2 + j*v2 )
    | i <- [ 0 ..      div 15 h1 + 1] ,
      j <- [      -   (div 15 v2 + 1)
               .. 2 * (div 15 v2 + 1) ] ]
  -- `j` needs a wide range because `hv` might be diagonal.
  in map (pairAdd low) wideGrid

pcToXys :: (X,Y) -> PitchClass EdoApp -> [(X,Y)]
pcToXys shift pc =
  enharmonicToXYs $
  pairAdd (et31ToLowXY pc) shift
