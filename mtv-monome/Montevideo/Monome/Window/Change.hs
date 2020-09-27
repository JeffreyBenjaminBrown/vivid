-- | A window for changing what windows are on a monome.

module Montevideo.Monome.Window.Change where

import           Prelude hiding (pred)
import           Control.Lens hiding (to)
import           Data.Either.Combinators
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)

import           Montevideo.Dispatch.Types.Many
import qualified Montevideo.Monome.Config.Mtv as Config
import qualified Montevideo.Monome.EdoMath as EM
import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Window.Common
import           Montevideo.Synth
import           Montevideo.Util
import           Montevideo.Monome.Window.Util


label :: WindowId
label = ChangeWindow

changewindow :: MonomeId                  -- ^ the affected monome
               -> [ ( (X,Y), Window app ) ] -- ^ the choices
               -> Window EdoApp
changewindow mi choices =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> x == 0 &&
                               numBetween 0 (length choices - 1) y
  , windowInitLeds = \_ _ -> Right []
  , windowHandler = handler }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (mi, press@(xy,    sw)) =
  Right $ st &
    _stPending_Monome .~ [
      ((mi, label), ((0,0), True))
      ,
      ((mi, label), ((0,0), False))
      ]
