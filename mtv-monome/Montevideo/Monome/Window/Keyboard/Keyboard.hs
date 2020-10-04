{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications
#-}

module Montevideo.Monome.Window.Keyboard.Keyboard (
    handler
  , keyboardWindow
  , label
  , edoKey_ScAction -- ^ EdoApp -> ((X,Y), Switch) -> [ScAction VoiceId]
  ) where

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
import           Montevideo.Monome.Util
import           Montevideo.Monome.Window.Common
import           Montevideo.Monome.Window.Util
import           Montevideo.Synth
import           Montevideo.Util


label :: WindowId
label = KeyboardWindow

keyboardWindow :: Window EdoApp
keyboardWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInitLeds = \st mi ->
      fmap ( map ( ( (mi, label) ,)
                   . (, True) ) ) $
      fmap concat $
      mapM (EM.pcToXys_st st mi ) $
      M.keys $ st ^. stApp . edoLit
  , windowHandler = handler }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (mi, press@(xy,sw)) =
  mapLeft ("Keyboard handler: " ++) $ do
  let app = st ^. stApp
  kbd :: Keyboard <-
         maybe (Left $ show mi ++ " absent from _edoKeyboards.")
         Right $ M.lookup mi $ _edoKeyboards app
  vid :: VoiceId <-
    if sw then Right $ nextVoice st
    else maybe (Left $ show xy ++ " not fingered on Keyboard.")
         Right $ M.lookup xy $ _kbdFingers kbd

  pcNow :: EdoPitchClass <-
    EM.pToPc_st st <$> EM.xyToEdo_app app mi xy
      -- what the key represents currently
  let
    pcThen :: Maybe EdoPitchClass =
      ledBecause_toPitchClass @ EdoApp
      (app ^. edoLit) $ LedBecauseSwitch xy
      -- what the key represented when it was pressed,
      -- if it is now being released
    fingers' :: Map (X,Y) VoiceId =
      _kbdFingers kbd &
      if sw then M.insert xy vid else M.delete xy

    lit  :: LitPitches EdoApp = app ^. edoLit
    lit' :: LitPitches EdoApp = updateStLit (xy,sw) pcNow pcThen lit
    oldKeys :: Set EdoPitchClass = S.fromList $ M.keys $ lit
    newKeys :: Set EdoPitchClass = S.fromList $ M.keys $ lit'
    toDark  :: [EdoPitchClass] = S.toList $ S.difference oldKeys newKeys
    toLight :: [EdoPitchClass] = S.toList $ S.difference newKeys oldKeys

  whereDark  :: [LedMsg] <-
    EM.pcsToLedMsgs_st st label False toDark
  whereLight :: [LedMsg] <-
    EM.pcsToLedMsgs_st st label True toLight
  scas :: [ScAction VoiceId] <-
    edoKey_ScAction st mi vid press
  vp :: Pitch EdoApp <-
    EM.xyToEdo_app app mi xy

  let
    v :: Voice EdoApp = Voice
      { _voiceSynth  = Nothing
      , _voicePitch  = vp
      , _voiceParams = mempty -- changed later, by `updateVoiceParams`
      }
    st1 :: St EdoApp = st
      & stApp . edoKeyboards . at mi . _Just . kbdFingers .~ fingers'
      & stApp . edoLit     .~ lit'
      & stPending_Monome %~ (++ whereDark ++ whereLight)
      & stPending_Vivid  %~ (++ scas)
      & stVoices         %~ (if sw then M.insert vid v else id)
  Right $ foldr updateVoiceParams st1 scas

updateStLit :: ((X,Y), Switch)
  -> EdoPitchClass         -- ^ what xy represents now
  -> Maybe EdoPitchClass -- ^ what xy represented when last pressed
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

edoKey_ScAction :: St EdoApp -> MonomeId -> VoiceId -> ((X,Y), Switch)
                -> Either String [ScAction VoiceId]
edoKey_ScAction st mi vid (xy, sw) = do
  let app = _stApp st
      ec = app ^. edoConfig
  pitch <- EM.xyToEdo_app app mi xy
  Right $ if S.member vid $ app ^. edoSustaineded
          then [] -- The voice is sounding due to sustain; don't change it.
                  -- This branch is not possible on key presses (sw == True),
                  -- because each press creates a new voice,
                  -- but it is possible on key releases.

    else if sw -- sw <=> the key was pressed, rather than released
         then [ monome_scActionNew
                ec vid (_stZotDefaults st) pitch ]
         else [silenceMsg vid]
