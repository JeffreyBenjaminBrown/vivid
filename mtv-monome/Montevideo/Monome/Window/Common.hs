{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE AllowAmbiguousTypes
, ScopedTypeVariables #-}

module Montevideo.Monome.Window.Common (
    ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
  , silenceMsg              -- ^ (X,Y) -> ScAction VoiceId
  , updateVoiceParams       -- ^ ScAction VoiceId -> St -> St
  , vid_to_pitchClass       -- ^ St -> VoiceId -> PitchClass
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S

import           Montevideo.Dispatch.Types.Many
import           Montevideo.Monome.Util.Button
import           Montevideo.Monome.Types.Most
import           Montevideo.Synth


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

silenceMsg :: VoiceId -> ScAction VoiceId
silenceMsg xy = ScAction_Free
  { _actionSynthDefEnum = Moop
  , _actionSynthName = xy
  }

-- | `updateVoiceParams sdMsg st` finds the VoiceId in the sdMsg,
-- and updates the parameters of the corresponding voice in the St.
-- PITFALL: If the action is a `ScAction_Free`, this does nothing.
-- It could instead set the parameter map to mempty,
-- but I don't see the advantage.
updateVoiceParams :: ScAction VoiceId -> St app -> St app
updateVoiceParams sca =
  if has _ScAction_Free sca then id else
  let go :: (ParamName, Float) -> St app -> St app
      go (p, f) =
        (stVoices    . at (_actionSynthName sca) . _Just) .
        (voiceParams . at p                      . _Just) .~ f
  in foldr (.) id $ map go $
     M.toList $ _actionScMsg sca

vid_to_pitchClass :: St EdoApp -> VoiceId ->  Either String (PitchClass EdoApp)
vid_to_pitchClass st v =
  mapLeft ("vid_to_pitchClass: " ++) $ maybe
  (Left "vid_to_pitchClass: voice not found")
  (Right . flip mod (st ^. stApp . edoConfig . edo) . _voicePitch)
  $ M.lookup v (_stVoices st)
