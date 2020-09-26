-- | Math for equal divisions of the octave.

module Montevideo.Monome.EdoMath (
    edoToFreq   -- ^ EdoConfig -> EdoPitch -> Float
  , xyToEdo     -- ^ EdoConfig -> (X,Y) -> EdoPitch
  , xyToEdo_app -- ^ EdoApp    -> (X,Y) -> EdoPitch
  , pcToXys_st  -- ^ St EdoApp -> EdoPitchClass -> [(X,Y)]
  , pcToXys     -- ^ EdoConfig -> PitchClass -> (X,Y) -> [(X,Y)]
  , pcToLowXY   -- ^ EdoConfig -> PitchClass -> (X,Y)
  , vv, hv      -- ^ EdoConfig -> (X,Y)
  , modEdo      -- ^ Integral a => St EdoApp -> a -> a
  , modEdo_st   -- ^ Integral a => St EdoApp -> a -> a
  , pToPc       -- ^ EdoConfig -> EdoPitch -> EdoPitchClass
  , pToPc_st    -- ^ St EdoApp -> EdoPitch -> EdoPitchClass
  , xyToMonome  -- ^ EdoApp -> MonomeId -> MonomeId -> (X,Y)
                -- -> Either String (X,Y)
  ) where

import Control.Lens hiding (from,to)
import Data.Either.Combinators

import           Montevideo.JI.Util (fromCents)
import           Montevideo.Monome.Types.Most
import           Montevideo.Util


edoToFreq :: EdoConfig -> EdoPitch -> Float
edoToFreq ec f =
  let two :: Float = realToFrac $ fromCents $
                     10 * (1200 + ec ^. octaveStretchInCents)
  in two ** ( fi (_unEdoPitch f) /
              fi (ec ^. edo) )

xyToEdo_app :: EdoApp -> MonomeId -> (X,Y)
            -> Either String EdoPitch
xyToEdo_app app mi xy = do
  sh :: (X,Y) <-
    maybe (Left $ show mi ++ " not found in edoKeyboards.") Right
    $ app ^? edoKeyboards . at mi . _Just . kbdShift
  Right $
    xyToEdo (app ^. edoConfig) $
    pairAdd xy $ pairMul (-1) sh

pcToXys_st :: St EdoApp -> MonomeId -> EdoPitchClass
           -> Either String [(X,Y)]
pcToXys_st st mi = let
  sh :: Either String (X,Y) =
    maybe (Left $ show mi ++ " not found in edoKeyboards.") Right
    $ st ^? stApp . edoKeyboards . at mi . _Just . kbdShift
  in case sh of
       Left s -> const $ Left s -- This `const` prevents use of do notation.
       Right sh' ->
         Right . pcToXys (st ^. stApp . edoConfig) sh'


-- | `pcToXys ec shift pc` finds all buttons that are enharmonically
-- equal to a given PitchClass, taking into account how the pitches
-- have been shifted across the monome.
pcToXys :: EdoConfig -> (X,Y) -> EdoPitchClass -> [(X,Y)]
pcToXys ec shift pc = let
  onMonome :: (X,Y) -> Bool -- monome button coordinates are in [0,15]
  onMonome (x,y) = x >= 0  && y >= 0  &&
                   x <= 15 && y <= 15
  in filter onMonome $
     _enharmonicToXYs ec $
     pairAdd (pcToLowXY ec pc) shift

-- | A superset of all keys that sound the same note
-- (modulo octave) visible on the monome.
--
-- This computes a lot of out-of-range values.
-- (The higher the edo, the lesser this problem.)
-- I'm pretty sure that has no effect on user experience.

_enharmonicToXYs :: EdoConfig -> (X,Y) -> [(X,Y)]
_enharmonicToXYs ec btn = let

  low = pcToLowXY ec $ pToPc ec $ xyToEdo ec btn
  ((v1,v2),(h1,h2)) = (vv ec, hv ec)
  wideGrid = [
    ( i*h1 + j*v1
    , i*h2 + j*v2 )
    | i <- [ 0 ..      div 15 h1 + 1] ,
      j <- [ -- `j` needs a wide range because `hv` might be diagonal.
                  -   (div 15 v2 + 1)
               .. 2 * (div 15 v2 + 1) ] ]
  in map (pairAdd low) wideGrid

-- | `xyToEdo ec (x,y)` finds the pitch class that (x,y) corresponds to,
-- assuming the board has not been shifted.
--
-- In the PitchClass domain, xyToEdo and pcToLowXY are inverses:
-- xyToEdo <$> pcToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
xyToEdo :: EdoConfig -> (X,Y) -> EdoPitch
xyToEdo ec (x,y) = EdoPitch
                 $ _spacing ec * x
                 + _skip    ec * y

-- | The leftmost appearance of the input pitchclass on the monome.
--
-- PITFALL: Does not take into account shifting via _edoXyShift.
--
-- In the PitchClass domain, xyToEdo and pcToLowXY are roughly inverse:
-- xyToEdo <$> pcToLowXY <$> [0..31] == [0,1,2,3,4,5,6,7,8,9,10,11,12,
-- 13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,0]
-- (notice the 0 at the end).
--
-- This algorithm used to give the leftmost of the topmost instances,
-- but now it gives the lowermost of the leftmost instances of the note.
-- Hopefully it doesn't matter, since `_enharmonicToXYs`
-- generates lots of out-of-bounds instances on all four sides.
--
-- Without Thanos tunings, the algorithm was much simpler:
--   pcToLowXY ec pc = ( div j $ _spacing ec
--                      , mod j $ _spacing ec )
--     where j = mod pc $ _edo ec

pcToLowXY :: EdoConfig -> EdoPitchClass -> (X,Y)
pcToLowXY ec pc = let
  sk = _skip ec
  f :: X -> (X, Y)
  f x = let
    y = mod (_unEdoPitchClass pc - x * _spacing ec) $ _edo ec
    y' = div y sk
    in if mod y sk == 0 && y' >= 0 && y' <= 15
       then (x, y')
       else f $ x + 1
  in f 0

-- | PITFALL: These functions can only find `hv` and `vv` automatically
-- for non-Thanos tunings, i.e. tunings in which `_skip = 1`.
--
-- `hv` and `vv` form The smallest, most orthogonal set of
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
vv ec = case _gridVectors ec of
  Just gvs -> _gridVerticalVector gvs
  Nothing -> (-1, _spacing ec)
hv ec = case _gridVectors ec of
  Just gvs -> _gridHorizontalVector gvs
  Nothing -> let
    e = _edo ec
    s = _spacing ec
    x = -- horizontal span of an octave:
        -- the first multiple of spacing greater than or equal to edo
      head $ filter (>= e) $ (*s) <$> [1..]
    v1 = (div x s, e - x)
    v2 = pairAdd v1 $ vv ec
    in if abs (dot (vv ec) v1) <= abs (dot (vv ec) v2)
       then                v1  else                v2

modEdo :: Integral a => EdoConfig -> a -> a
modEdo ec = flip mod $ fromIntegral $
                _edo ec

modEdo_st :: Integral a => St EdoApp -> a -> a
modEdo_st = modEdo . _edoConfig . _stApp

pToPc :: EdoConfig -> EdoPitch -> EdoPitchClass
pToPc ec = EdoPitchClass . modEdo ec . _unEdoPitch

pToPc_st :: St EdoApp -> EdoPitch -> EdoPitchClass
pToPc_st st = EdoPitchClass . modEdo_st st . _unEdoPitch

-- | Translate an (X,Y) from one Keyboard to another.
-- If both have the same kbdShift, this leaves the (X,Y) unchanged.
xyToMonome :: EdoApp -> MonomeId -> MonomeId -> (X,Y)
           -> Either String (X,Y)
xyToMonome app from to xy =
  mapLeft ("xyToMonome: " ++) $ do
  fromShift :: (X,Y) <- maybe (Left $ show from ++ " not found.") Right $
                        app ^? edoKeyboards . at from . _Just . kbdShift
  toShift   :: (X,Y) <- maybe (Left $ show to ++ " not found.") Right $
                        app ^? edoKeyboards . at to . _Just . kbdShift
  Right $ xy & pairAdd toShift & flip pairSubtract fromShift
