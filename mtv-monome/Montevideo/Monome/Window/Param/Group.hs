{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications
#-}

module Montevideo.Monome.Window.Param.Group (
    handler
  , paramGroupWindow
  , label

  , paramReport      -- ^ St EdoApp ->                     ZotParam -> String
  , paramGroupReport -- ^ St EdoApp -> ParamGroup -> Maybe ZotParam -> [String]
  ) where

import           Control.Lens
import qualified Data.Map as M
import qualified Data.Bimap as Bi

import           Montevideo.Monome.Types
import           Montevideo.Synth
import           Montevideo.Util


label :: WindowId
label = ParamGroupWindow

paramGroupWindow :: Window EdoApp
paramGroupWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> ( numBetween 0 2 x &&
                                 numBetween 0 2 y )
                               || (x,y) == (0,3)
  , windowInitLeds = \_ _ -> Right []
  , windowHandler = \x y -> return $ handler x y }

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
             & ( stPending_String %~ flip (++)
                 (paramGroupReport st pgNew Nothing) )
             & ( stPending_Monome %~ flip (++)
                 [ ((mi, label), (xyOld, False))
                 , ((mi, label), (xy,    True)) ] )

paramReport :: St EdoApp -> ZotParam -> String
paramReport st p = let
  zDefault :: ZotParam -> String =
    maybe (show (zotDefaultValues M.! (show p)) ++ " (default)") show .
    flip M.lookup (st ^. stZotDefaults)
  zRange :: ZotParam -> String =
    maybe "paramGroupMessages: ERROR: missing ZotParam"     show .
    flip M.lookup (st ^. stZotRanges)
  in zRange p ++ " " ++
     zDefault p ++ " " ++
     show p

paramGroupReport :: St EdoApp -> ParamGroup -> Maybe ZotParam -> [String]
paramGroupReport st g mp = let
  zs :: [ZotParam] = paramGroup_params g
  zCaret :: ZotParam -> String
  zCaret p = case mp of
    Just p' -> if p == p' then " <-" else ""
    Nothing -> ""
  go :: ZotParam -> String
  go z = paramReport st z ++ " " ++
         zCaret z
  in "" : map go zs -- The empty string becomes a newline under putStrLn.
