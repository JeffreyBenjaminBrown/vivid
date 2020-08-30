{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications
#-}

module Montevideo.Monome.Window.ParamVal (
    handler
  , paramValWindow
  , label
  ) where

import           Control.Lens

import           Montevideo.Monome.Types.Most
import           Montevideo.Util


label :: WindowId
label = PulseWindow

paramValWindow :: Window EdoApp
paramValWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 0 15 x &&
                               y == 0
  , windowInitLeds = \_ _ -> []
  , windowHandler = handler }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (_, ((x,_),_)) =
  -- TODO : magic numbers; reify
  Right $ st & stPulse .~ linScale (0,15) (0,1) (fi x)
