{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications
#-}

module Montevideo.Monome.Window.ParamGroup (
    handler
  , paramGroupWindow
  , label
  ) where

import           Control.Lens
import qualified Data.Bimap as Bi

import           Montevideo.Monome.Types
import           Montevideo.Util


label :: WindowId
label = ParamGroupWindow

paramGroupWindow :: Window EdoApp
paramGroupWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 0 2 x &&
                               numBetween 0 2 y
  , windowInitLeds = \_ _ -> []
  , windowHandler = handler }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (_,        (_,     False)) = Right st
handler    st           (mi,       (xy,    True)) = do
  pgNew :: ParamGroup <-
    maybe (Left $ show xy ++ " not in " ++ show label ++ ".") Right $
    Bi.lookupR xy paramGroupXys
  let pgOld :: ParamGroup = st ^. stApp . edoParamGroup
      xyOld :: (X,Y) = paramGroup_toXy pgOld
  Right $ st & stApp . edoParamGroup .~ pgNew
             & ( stPending_Monome %~ flip (++)
                 [ ((mi, label), (xyOld, False))
                 , ((mi, label), (xy,    True)) ] )
