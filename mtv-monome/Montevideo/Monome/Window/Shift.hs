{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Monome.Window.Shift (
    handler
  , label
  , shiftWindow

  , shift
  , leftArrow, rightArrow, upArrow, downArrow, upOctave, downOctave -- ^ (X,Y)
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M

import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Util.OSC
import           Montevideo.Monome.Types.Most
import           Montevideo.Util
import qualified Montevideo.Monome.Window.Keyboard as Kbd


label :: WindowId
label = ShiftWindow

-- | = the arrows
rightArrow, downArrow, leftArrow, upOctave, upArrow, downOctave :: (X,Y)
rightArrow = (2,1)
downArrow =  (1,1)
leftArrow =  (0,1)
upOctave =   (2,0)
upArrow =    (1,0)
downOctave = (0,0)

-- | PITFALL: Remember (see Button.hs),
-- higher Y => lower (closer to you) on the monome.
-- | PITFALL: There are multiple ways to represent an octave shift.
-- Here I've chosen one arbitrarily.
shift :: EdoConfig -> (X,Y) -> Either String (X,Y)
shift ec = mapLeft ("shift: " ++) . f where
  f xy
    | xy == rightArrow = Right $ (-1, 0)
    | xy == downArrow  = Right $ ( 0,-1)
      -- origin at top-left => down means add to Y
    | xy == leftArrow  = Right $ ( 1, 0)
    | xy == upOctave   = Right $ pairMul (-1) (hv ec)
      -- lowering the origin raises the coordinate values of a given key, hence raising its pitch
    | xy == upArrow    = Right $ ( 0, 1)
    | xy == downOctave = Right $ hv ec
    | otherwise = Left $ "shift: unexpected input: " ++ show xy

-- | = the window
shiftWindow :: Window EdoApp
shiftWindow = Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 0 2 x && numBetween 0 1 y
  , windowInitLeds = \_ mi ->
    ( (mi, label) ,) . (,True) <$>
    [ upArrow, downArrow, leftArrow, rightArrow ]
  , windowHandler = handler
}

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)

handler    st0          (_, (_,  False))      = Right st0

handler    st0          (mi, (xy, True))      =
  mapLeft ("Shift window handler: " ++) $ do
  let ec = st0 ^. stApp . edoConfig
  s <- shift ec xy
  let st' :: St EdoApp =
        st0 & stApp . edoKeyboards . at mi . _Just . kbdShift %~ pairAdd s
      lit :: [EdoPitchClass] = M.keys $ st0 ^. stApp . edoLit
  msgs :: [LedMsg] <- do
    x <- map (,False) . concat <$> mapM (pcToXys_st st0 mi) lit
    y <- map (,True)  . concat <$> mapM (pcToXys_st st' mi) lit
    Right $ map ( (mi, Kbd.label) ,) $ x ++ y
  Right $ st' & stPending_Monome %~
                flip (++) msgs
