{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections
, AllowAmbiguousTypes
, ScopedTypeVariables #-}

module Montevideo.Monome.Window.Common (
    ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
  , silenceMsg              -- ^ (X,Y) -> SoundMsg
  , etKey_SoundMsg                -- ^ St -> ((X,Y), Switch) -> [SoundMsg]
  , updateVoice             -- ^ SoundMsg -> St -> St
  , vid_to_pitch            -- ^ St -> VoiceId -> PitchClass
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S

import qualified Montevideo.Monome.Config as Config
import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Types.Button
import           Montevideo.Monome.Types.Initial


-- Todo (#speed) Instead, keep a map from xy to pitchclass
ledBecause_toPitchClass :: forall app.
                           LitPitches app
                        -> LedBecause
                        -> Maybe (PitchClass app)
ledBecause_toPitchClass m ldr =
  fst <$> mPair
  where
    mPair = listToMaybe
            $ filter (S.member ldr . snd)
            $ M.toList m

silenceMsg :: (X,Y) -> SoundMsg app
silenceMsg xy = SoundMsg {
    _soundMsgVoiceId = xy
  , _soundMsgPitch = Nothing
  , _soundMsgVal = 0
  , _soundMsgParam = "amp" }

-- TODO ! duplicative of `jiKey_SoundMsg`
etKey_SoundMsg :: St EdoApp -> ((X,Y), Switch) -> [SoundMsg EdoApp]
etKey_SoundMsg st (xy, sw) = do
  let pitch = xyToEdo_st st xy
      ec = st ^. stApp . etConfig
  if maybe False (S.member xy) $
     st ^. stApp . etSustaineded
    then [] -- it's already sounding due to sustain
    else if sw -- sw <=> the key was pressed, rather than released
         then let msg = SoundMsg
                    { _soundMsgVoiceId = xy
                    , _soundMsgPitch = Just pitch
                    , _soundMsgVal = error "replaced below"
                    , _soundMsgParam = error "replaced below"
                    }
              in [ msg & ( soundMsgVal .~
                           Config.freq * edoToFreq ec pitch )
                       & soundMsgParam .~ "freq"
                 , msg & soundMsgVal .~ Config.amp
                       & soundMsgParam .~ "amp" ]
         else [silenceMsg xy]

updateVoice :: SoundMsg app -> St app -> St app
updateVoice sdMsg st = let
  vid   :: VoiceId = _soundMsgVoiceId sdMsg
  param :: Param   = _soundMsgParam   sdMsg
  f     :: Float   = _soundMsgVal     sdMsg
  in st & case _soundMsgPitch sdMsg of
            Nothing -> id
            Just p -> stVoices . at vid . _Just
                      %~ (voicePitch                     .~ p)
                      .  (voiceParams . at param . _Just .~ f)

vid_to_pitch :: St EdoApp -> VoiceId ->  PitchClass EdoApp
vid_to_pitch st v = maybe
  (error "vid_to_pitch: voice not found")
  (flip mod (st ^. stApp . etConfig . edo) . _voicePitch)
  $ M.lookup v (_stVoices st)
