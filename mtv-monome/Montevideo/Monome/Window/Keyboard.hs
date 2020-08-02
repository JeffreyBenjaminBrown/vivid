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
import           Data.Either.Combinators
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Set (Set)

import Montevideo.Dispatch.Types.Many
import Montevideo.Monome.EdoMath
import Montevideo.Monome.Util.Button
import Montevideo.Monome.Types.Most
import Montevideo.Monome.Window.Common
import Montevideo.Util


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
                  concatMap (pcToXys_st st) $
                  M.keys $ st ^. stApp . edoLit )
  , windowHandler = handler }

-- TODO ! duplicative of `JI.handler`
handler :: St EdoApp -> ((X,Y), Switch) -> Either String (St EdoApp)
handler    st          press@ (xy,sw)   =
  mapLeft ("Keyboard handler: " ++) $
  let
  app = st ^. stApp
  fingers' = app ^. edoFingers
        & case sw of
            True  -> M.insert xy xy
            False -> M.delete xy
  pcNow :: PitchClass EdoApp =
    mod (xyToEdo_app app xy) (app ^. edoConfig . edo)
    -- what the key represents currently
  pcThen :: Maybe (PitchClass EdoApp) =
    ledBecause_toPitchClass @ EdoApp
    (app ^. edoLit) $ LedBecauseSwitch xy
    -- what the key represented when it was pressed,
    -- if it is now being released

  lit  :: LitPitches EdoApp = app ^. edoLit
  lit' :: LitPitches EdoApp = updateStLit (xy,sw) pcNow pcThen lit
  oldKeys :: Set (PitchClass EdoApp) = S.fromList $ M.keys $ lit
  newKeys :: Set (PitchClass EdoApp) = S.fromList $ M.keys $ lit'
  toDark  :: [PitchClass EdoApp] = S.toList $ S.difference oldKeys newKeys
  toLight :: [PitchClass EdoApp] = S.toList $ S.difference newKeys oldKeys

  kbdMsgs :: [LedMsg] =
    map (label,) $
    ( map (,False) $
      concatMap (pcToXys_st st) toDark) ++
    ( map (,True)  $
      concatMap (pcToXys_st st) toLight)
  soundMsgs :: [ScAction VoiceId] = edoKey_ScAction app press
  st1 :: St EdoApp = st
    & stApp . edoFingers .~ fingers'
    & stApp . edoLit     .~ lit'
    & stPending_Monome  %~ (++ kbdMsgs)
    & stPending_Vivid   %~ (++ soundMsgs)
    & ( stVoices . at xy . _Just . voicePitch
        .~ xyToEdo_app app xy )
  in Right $ foldr updateVoiceParams st1 soundMsgs

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
