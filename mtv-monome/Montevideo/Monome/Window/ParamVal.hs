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
import           Data.Map as M

import           Montevideo.Monome.Types
import           Montevideo.Synth
import           Montevideo.Util


label :: WindowId
label = PulseWindow

paramValWindow :: Window EdoApp
paramValWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 3 15 x &&
                               numBetween 0 3 y
  , windowInitLeds = \_ _ -> []
  , windowHandler = handler }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (_       , (_       , False )) = Right st
handler    st           (mi      , ((x,y), True  )) = do
  let app :: EdoApp = st ^. stApp
      pg :: ParamGroup = app ^. edoParamGroup
  case paramGroup_toParam pg y :: Either String ZotParam of
    Left _ -> Right st
    Right (zp :: ZotParam) -> do
      let (ns, nMin, nMax) :: (NumScale, Float, Float) =
            (M.!) (app ^. edoZotRanges) $ zp
      Right $ st
        & stZotDefaults %~ ( M.insert zp $
                             numScale ns (3,15) (nMin,nMax) $ fi x)
        & ( stPending_Monome %~ flip (++)
            (   ((mi, label), ((x ,y), True ))
            -- This is wasteful -- 13 LED messages where 2 would suffice --
            -- but whatever, nothing else happens during such a button press.
            : [ ((mi, label), ((x',y), False))
              | x' <- [3..15], x' /= x ] ) )
