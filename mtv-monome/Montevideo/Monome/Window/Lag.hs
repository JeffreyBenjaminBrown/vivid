{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications
#-}

module Montevideo.Monome.Window.Lag (
    handler
  , lagWindow
  , label
  ) where

import           Control.Lens

import           Montevideo.Monome.Types.Most
import           Montevideo.Util


label :: WindowId
label = LagWindow

lagWindow :: Window EdoApp
lagWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 0 15 x &&
                               y == 0
  , windowInitLeds = \_ _ -> []
  , windowHandler = handler }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (_, ((x,_),_)) =
-- TODO : magic numbers; reify
  Right $ st & stLag .~ ( logScale (0,15)
                          ( 0.03, 10 )
                          $ fi x )
