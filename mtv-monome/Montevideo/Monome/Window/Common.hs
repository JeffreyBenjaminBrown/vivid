{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections
, AllowAmbiguousTypes
, ScopedTypeVariables #-}

module Montevideo.Monome.Window.Common (
    ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
  , silenceMsg              -- ^ (X,Y) -> SoundMsg
  , etKey_SoundMsg          -- ^ St -> ((X,Y), Switch) -> [SoundMsg]
  , updateVoiceParams       -- ^ SoundMsg -> St -> St
  , vid_to_pitch            -- ^ St -> VoiceId -> PitchClass
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S

import qualified Montevideo.Monome.Config as Config
import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Util.Button
import           Montevideo.Monome.Types.Most


-- | Given an `LedBecause` like `LedBecauseSwitch (x,y)`,
-- this will find the `PitchClass` that was lit for that reason.
--
-- TODO ? This is janky. For one thing, it doesn't make sense
-- if the `LedBecause` is `LedBecauseSustain`, because in that case
-- it should return multiple pitch classes.
--
-- TODO (#speed) Instead, keep a map from xy to pitchclass

ledBecause_toPitchClass :: forall app.
  LitPitches app -- Map (PitchClass app) (Set LedBecause)
  -> LedBecause
  -> Maybe (PitchClass app)
ledBecause_toPitchClass m lb =
  fst <$> mPair
  where
    mPair :: Maybe (PitchClass app, S.Set LedBecause) =
      listToMaybe
      $ filter (S.member lb . snd)
      $ M.toList m

silenceMsg :: (X,Y) -> SoundMsg app
silenceMsg xy = SoundMsg {
    _soundMsgVoiceId = xy
  , _soundMsgVal = 0
  , _soundMsgParam = "amp" }

-- TODO ! duplicative of `jiKey_SoundMsg`
etKey_SoundMsg :: EdoApp -> ((X,Y), Switch) -> [SoundMsg EdoApp]
etKey_SoundMsg app (xy, sw) = do
  let pitch = xyToEdo_app app xy
      ec = app ^. edoConfig
  if maybe False (S.member xy) $
     app ^. edoSustaineded
    then [] -- it's already sounding due to sustain

    else if sw -- sw <=> the key was pressed, rather than released
         then let msg = SoundMsg
                    { _soundMsgVoiceId = xy
                    , _soundMsgVal = error "replaced below"
                    , _soundMsgParam = error "replaced below"
                    }
              in [ msg & ( soundMsgVal .~
                           Config.freq * edoToFreq ec pitch )
                       & soundMsgParam .~ "freq"
                 , msg & soundMsgVal .~ Config.amp
                       & soundMsgParam .~ "amp" ]
         else [silenceMsg xy]

-- | `updateVoiceParams sdMsg st` finds the VoiceId in the sdMsg,
-- and updates the corresponding voice in the St to reflect the new
-- pitch and parameters.
updateVoiceParams :: SoundMsg app -> St app -> St app
updateVoiceParams sdMsg st =
  st &  stVoices    . at (_soundMsgVoiceId sdMsg) . _Just
     .  voiceParams . at (_soundMsgParam   sdMsg) . _Just
     .~                   _soundMsgVal     sdMsg

vid_to_pitch :: St EdoApp -> VoiceId ->  Either String (PitchClass EdoApp)
vid_to_pitch st v =
  mapLeft ("vid_to_pitch: " ++) $ maybe
  (Left "vid_to_pitch: voice not found")
  (Right . flip mod (st ^. stApp . edoConfig . edo) . _voicePitch)
  $ M.lookup v (_stVoices st)
