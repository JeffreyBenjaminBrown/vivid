{-# LANGUAGE AllowAmbiguousTypes #-}

module Montevideo.Monome.Util (
    nextVoice     -- ^ M.Map VoiceId a -> VoiceId
  , monome_scActionNew -- ^ EdoConfig -> VoiceId -> M.Map ZotParam Float
                       -- -> EdoPitch -> ScAction VoiceId
  , ledBecause_toPitchClass -- ^ LitPitches -> LedBecause -> Maybe PitchClass
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

import           Montevideo.Dispatch.Types
import qualified Montevideo.Monome.Config.Mtv as Config
import qualified Montevideo.Monome.EdoMath as EM
import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Types
import           Montevideo.Synth


nextVoice :: St a -> VoiceId
nextVoice st =
  case M.lookupMax $ _stVoices st of
    Nothing             -> VoiceId 0
    Just (VoiceId i, _) -> VoiceId $ i+1

monome_scActionNew
  :: EdoConfig -> VoiceId -> M.Map ZotParam Float -> EdoPitch
  -> ScAction VoiceId
monome_scActionNew ec vi timbre pitch = ScAction_New
  { _actionSynthDefEnum = Zot
  , _actionSynthName = vi
  , _actionScParams =
      M.mapKeys show -- show :: ZotParam -> String
      $ M.union -- in fonclict, the first arg takes priority
      ( M.fromList [ (Zot_freq, Config.freq *
                                EM.edoToFreq ec pitch) ] )
      $ timbre }

-- | Given an `LedBecause` like `LedBecauseSwitch (x,y)`,
-- this will find the `PitchClass` that was lit for that reason.
--
-- TODO ? This is janky. For one thing, it doesn't make sense
-- if the `LedBecause` is `LedBecauseSustain`, because in that case
-- it should return multiple pitch classes.
--
-- TODO (#speed) Instead, keep a map from xy to pitchclass

ledBecause_toPitchClass :: forall app.
  LitPitches app
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
  { _actionSynthDefEnum = Zot
  , _actionSynthName = xy
  }

-- | `updateVoiceParams sdMsg st` finds the VoiceId in the sdMsg,
-- and updates the parameters of the corresponding voice in the St.
-- PITFALL: If the action is a `ScAction_Free`, this does nothing.
-- It could instead set the parameter map to mempty,
-- but I don't see the advantage in that.
updateVoiceParams :: ScAction VoiceId -> St app -> St app
updateVoiceParams sca =
  if has _ScAction_Free sca then id else
  let go :: (ParamName, Float) -> St app -> St app
      go (p, f) =
        (stVoices    . at (_actionSynthName sca) . _Just) .
        (voiceParams . at p                      . _Just) .~ f
  in foldr (.) id $ map go $
     M.toList $ _actionScParams sca

vid_to_pitchClass :: St EdoApp -> VoiceId
                  ->  Either String EdoPitchClass
vid_to_pitchClass st v =
  mapLeft ("vid_to_pitchClass: " ++) $ maybe
  (Left "vid_to_pitchClass: voice not found")
  (Right . pToPc_st st . _voicePitch)
  $ M.lookup v (_stVoices st)
