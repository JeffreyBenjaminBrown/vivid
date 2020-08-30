{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications
#-}

module Montevideo.Monome.Window.ParamGroup (
    handler
  , pulseWindow
  , label
  ) where

import           Control.Lens
import qualified Data.Bimap as Bi

import           Montevideo.Monome.Types.Most
import           Montevideo.Util


label :: WindowId
label = ParamGroupWindow

pulseWindow :: Window EdoApp
pulseWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 0 2 x &&
                               numBetween 0 2 y
  , windowInitLeds = \_ _ -> []
  , windowHandler = handler }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (mi, press@(xy,sw)) =
  -- Find what group (x,y) corresponds to.
  -- Change the (supposed) _stParamGroup value to that.
  -- Darken the old button, light the new one.
  error "TODO"
