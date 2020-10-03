-- | A window for changing what windows are on a monome.

module Montevideo.Monome.Window.ChordBank.Bank (
  chordBankWindow
  ) where

import           Prelude

import           Montevideo.Monome.Types.Most
import           Montevideo.Util


label :: WindowId
label = ChordBank

chordBankWindow :: Window app
chordBankWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) ->
      numBetween 0 7 x &&
      numBetween 0 7 y
  , windowInitLeds = \_ _ -> Right []
  , windowHandler = handler }

handler :: forall app.
           St app -> (MonomeId, ((X,Y), Switch))
        -> Either String (St app)
handler st (mid, (xy,sw)) = Right st
