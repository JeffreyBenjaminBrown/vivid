{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Montevideo.Monome.Window.Shift (
    handler
  , label
  , shiftWindow

  , shift
  , leftArrow, rightArrow, upArrow, downArrow, upOctave, downOctave -- ^ (X,Y)
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import qualified Data.Map as M

import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Types.Button
import           Montevideo.Monome.Types.Initial
import           Montevideo.Util
import qualified Montevideo.Monome.Window.Keyboard as Kbd


label :: WindowId
label = "shift window"

-- | = the arrows
rightArrow, downArrow, leftArrow, upOctave, upArrow, downOctave :: (X,Y)
rightArrow = (15,15)
downArrow =  (14,15)
leftArrow =  (13,15)
upOctave =   (15,14)
upArrow =    (14,14)
downOctave = (13,14)

-- | PITFALL: Remember (see Button.hs),
-- higher Y => lower (closer to you) on the monome.
-- | PITFALL: There are multiple ways to represent an octave shift.
-- Here I've chosen one arbitrarily.
shift :: EdoConfig -> (X,Y) -> (X,Y)
shift ec xy | xy == rightArrow = (-1, 0)
            | xy == downArrow  = ( 0,-1)
              -- origin at top-left => down means add to Y
            | xy == leftArrow  = ( 1, 0)
            | xy == upOctave   = pairMul (-1) (hv ec)
              -- lowering the origin raises the coordinate values of a given key, hence raising its pitch
            | xy == upArrow    = ( 0, 1)
            | xy == downOctave = hv ec
            | otherwise = error $ "shift: unexpected input: " ++ show xy

-- | = the window
shiftWindow :: Window EdoApp
shiftWindow = Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 13 15 x && numBetween 14 15 y
  , windowInit = stPending_Monome %~ flip (++)
    ( (label,) . (,True) <$>
      [ upArrow, downArrow, leftArrow, rightArrow ] )
  , windowRoutine = handler
}

handler :: St EdoApp -> ((X,Y), Switch) -> St EdoApp
handler    st0         (_,  False)      = st0
handler    st0         (xy, True )      = let
  ec = st0 ^. stApp . edoConfig
  st' :: St EdoApp = st0 & stApp . edoXyShift %~ pairAdd (shift ec xy)
  lit :: [PitchClass EdoApp] = M.keys $ st0 ^. stApp . edoLit
  msgs :: [LedMsg] =
    map (Kbd.label,) $
    (map (,False) $ concatMap (pcToXys_st st0) lit) ++
    (map (,True)  $ concatMap (pcToXys_st st') lit)
  in st' & stPending_Monome %~ flip (++) msgs
