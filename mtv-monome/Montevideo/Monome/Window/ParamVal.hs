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
import qualified Montevideo.Monome.Window.ParamGroup as PG
import           Montevideo.Synth
import           Montevideo.Util


label :: WindowId
label = ParamVal_Window

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
  let pg :: ParamGroup = st ^. stApp . edoParamGroup
  case paramGroup_toParam pg y :: Either String ZotParam of
    Left _ -> Right st
    Right (zp :: ZotParam) -> do

      let (ns, nMin, nMax) :: (NumScale, Float, Float) =
            (M.!) (st ^. stZotRanges) $ zp
          st1 = st
            & stZotDefaults %~ ( M.insert zp $
                                 numScale ns (3,15) (nMin,nMax) $ fi x)
            & ( stPending_Monome %~ flip (++)
                (   ((mi, label), ((x ,y), True ))
                : [ ((mi, label), ((x',y), False))
                  -- This is wwasteful: 13 messages where 2 would work.
                  -- But nothing else happens during such a button press.
                  | x' <- [3..15], x' /= x ] ) )
      Right $ st1
        & ( stPending_String %~ flip (++)
            ( PG.paramGroupReport st1
              (st1 ^. stApp . edoParamGroup)
              (Just zp)
            ) )
