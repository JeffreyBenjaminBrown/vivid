{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, ScopedTypeVariables
, TupleSections
, TypeApplications
#-}

module Montevideo.Monome.Window.Keyboard (
    handler
  , keyboardWindow
  , label
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Set (Set)

import qualified Montevideo.Monome.Config as Config
import           Montevideo.Monome.Math31
import           Montevideo.Monome.Types.Button
import           Montevideo.Monome.Types.Initial
import           Montevideo.Monome.Window.Common
import           Montevideo.Util


label :: WindowId
label = "keyboard window"

keyboardWindow :: Window EdoApp
keyboardWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInit = \st ->
      st & stPending_Monome %~
      flip (++) ( map ( (label,) . (,True) ) $
                  concatMap (pcToXys $ st ^. stApp . etXyShift) $
                  M.keys $ st ^. stApp . etLit )
  , windowRoutine = handler }

-- TODO ! duplicative of `JI.handler`
handler :: St EdoApp -> ((X,Y), Switch) -> St EdoApp
handler    st          press@ (xy,sw)   = let
  fingers' = st ^. stApp . etFingers
        & case sw of
            True  -> M.insert xy xy
            False -> M.delete xy
  pcNow :: (PitchClass EdoApp) =
    mod (xyToEt31_st st xy) Config.edo
    -- what the key represents currently
  pcThen :: Maybe (PitchClass EdoApp) =
    ledBecause_toPitchClass @ EdoApp
    (st ^. stApp . etLit) $ LedBecauseSwitch xy
    -- what the key represented when it was pressed,
    -- if it is now being released
  lit  :: LitPitches EdoApp = st ^. stApp . etLit
  lit' :: LitPitches EdoApp = updateStLit (xy,sw) pcNow pcThen lit
  oldKeys :: Set (PitchClass EdoApp) = S.fromList $ M.keys $ lit
  newKeys :: Set (PitchClass EdoApp) = S.fromList $ M.keys $ lit'
  toDark  :: [PitchClass EdoApp] = S.toList $ S.difference oldKeys newKeys
  toLight :: [PitchClass EdoApp] = S.toList $ S.difference newKeys oldKeys
  kbdMsgs :: [LedMsg] =
    map (label,) $
    ( map (,False) $
      concatMap (pcToXys $ st ^. stApp . etXyShift) toDark) ++
    ( map (,True)  $
      concatMap (pcToXys $ st ^. stApp . etXyShift) toLight)
  soundMsgs :: [SoundMsg EdoApp] = etKey_SoundMsg st press
  st1 :: St EdoApp = st
    & stApp . etFingers .~ fingers'
    & stApp . etLit     .~ lit'
    & stPending_Monome  %~ (++ kbdMsgs)
    & stPending_Vivid   %~ (++ soundMsgs)
  in foldr updateVoice st1 soundMsgs

updateStLit :: ((X,Y), Switch)
  -> PitchClass EdoApp         -- ^ what xy represents now
  -> Maybe (PitchClass EdoApp) -- ^ what xy represented when last pressed
  -> LitPitches EdoApp
  -> LitPitches EdoApp

-- | When a button is newly pressed,
-- it adds anoother LedBecause to the LitPitches.
updateStLit (xy,True) pcNow _ m =
  M.insert pcNow new m where
  new = case M.lookup pcNow m of
    Nothing ->      S.singleton $ LedBecauseSwitch xy
    Just reasons -> S.insert (LedBecauseSwitch xy) reasons

-- | When a key is released, it might be that we've used the arrows
-- since pressing it. If that's so, then the pitch it triggered is elsewhere,
-- and illuminated. This removes the pitch from the LitPitches,
-- so that the appropriate pitch class will be darkened.
updateStLit (xy,False) _ mpcThen m =
  case mpcThen of
    Nothing -> m
    Just pc ->
      -- todo ? (#safety) Check that that's really what's being deleted.
      let Just reasons = M.lookup pc m
      in case S.size reasons < 2 of -- size < 1 should not happen
        True -> M.delete pc m
        False -> M.insert pc (S.delete (LedBecauseSwitch xy) reasons) m
