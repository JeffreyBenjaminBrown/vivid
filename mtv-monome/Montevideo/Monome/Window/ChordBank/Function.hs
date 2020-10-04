module Montevideo.Monome.Window.ChordBank.Function (
  chordFunctionWindow
  ) where

import           Prelude hiding (pred)

import           Montevideo.Monome.Types.Most
import           Montevideo.Synth.Msg


buttonStore :: (X,Y)
buttonStore = (0,0)

label :: WindowId
label = ChordFunctionWindow

chordFunctionWindow :: Window app
chordFunctionWindow =  Window {
    windowLabel = label
  , windowContains = flip elem [buttonStore]
  , windowInitLeds = \_ _ -> Right []
  , windowHandler = handler }

handler :: forall app.
           St app -> (MonomeId, ((X,Y), Switch))
        -> Either String (St app)
handler st (mid, (xy,sw)) = Right st

