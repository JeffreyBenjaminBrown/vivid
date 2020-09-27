{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications
#-}

module Montevideo.Monome.Window.ParamVal (
    handler
  , paramValWindow
  , label

  , paramToAllVoices -- ^ St EdoApp -> ZotParam -> Float -> [ScAction VoiceId]
  ) where

import           Control.Lens
import           Data.Map as M

import           Montevideo.Dispatch.Types.Many
import           Montevideo.Monome.Types
import qualified Montevideo.Monome.Window.ParamGroup as PG
import           Montevideo.Synth
import           Montevideo.Util


label :: WindowId
label = ParamVal_Window

paramValWindow :: Window EdoApp
paramValWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 0 12 x &&
                               numBetween 0 5 y
  , windowInitLeds = \_ _ -> Right []
  , windowHandler = handler }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (_       , (_       , False )) = Right st
handler    st           (mi      , ((x,y), True  )) = do
  let pg :: ParamGroup = st ^. stApp . edoParamGroup
  case paramGroup_toParam pg y :: Either String ZotParam of
    Left _ -> Right st
    Right (zp :: ZotParam) -> do

      let val :: Float = numScale ns
                         (0,12) (fr nMin, fr nMax) (fi x)
          (ns, nMin, nMax) :: (NumScale, Rational, Rational) =
            (M.!) (st ^. stZotRanges) $ zp

          st1 = st
            & stZotDefaults      %~ M.insert zp val
            & stPending_Vivid    %~ flip (++) (paramToAllVoices st zp val)
            & ( stPending_Monome %~ flip (++)
                (   ((mi, label), ((x ,y), True ))
                : [ ((mi, label), ((x',y), False)) -- TODO ? This is wasteful:
                  -- It sends 13 messages when 2 would suffice.
                  | x' <- [0..12], x' /= x ] ) )
      Right $ st1
        & ( stPending_String %~ flip (++)
            ( PG.paramGroupReport st1
              (st1 ^. stApp . edoParamGroup)
              (Just zp)
            ) )

paramToAllVoices :: St EdoApp -> ZotParam -> Float -> [ScAction VoiceId]
paramToAllVoices st zp f =
  [ ScAction_Send { _actionSynthDefEnum = Zot
                  , _actionSynthName = v :: VoiceId
                  , _actionScMsg = M.singleton (show zp) f }
  | v <- M.keys $ _stVoices st ]
