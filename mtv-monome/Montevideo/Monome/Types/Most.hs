{-# LANGUAGE TypeFamilies,
TemplateHaskell #-}

module Montevideo.Monome.Types.Most (
    module Montevideo.Monome.Types.EdoConfig
  , module Montevideo.Monome.Types.Monome
  , Param, WindowId, VoiceId(..)
  , LitPitches
  , LedMsg
  , Pitch, PitchClass
  , LedBecause(..)
  , Window(..)
  , Voice(..), voiceSynth, voicePitch, voiceParams
  , St(..), edoConfig, stApp, stWindowLayers, stToMonome, stVoices
    , stPending_Monome, stPending_Vivid
  , EdoApp(..), edoXyShift, edoFingers, edoLit, edoSustaineded
  , JiApp(..), jiGenerator, jiShifts, jiFingers
  ) where

import           Control.Lens
import           Data.Map
import           Data.Set
import           Vivid hiding (Param)

import Montevideo.Dispatch.Types.Many
import Montevideo.Monome.Types.EdoConfig
import Montevideo.Monome.Types.Monome
import Montevideo.Synth


type Param = String
type WindowId = String
newtype VoiceId = VoiceId Int
  deriving (Show, Eq, Ord)

-- | In the Equal Tempered app, Pitch is isomorphic to the integers, and
-- PitchClass is isomorphic to the integers modulo the edo (e.g. 31).
-- That is, in 31-edo, PitchClass 0 is identical to PitchClass 31,
-- whereas Pitch 31 is an octave above Pitch 0.
-- Pitch classes are (so far) only relevant in the visual context:
-- if a key is lit up on the keyboard, so are all enharmonic notes,
-- in all octaves.
--
-- In the Just Tempered app, Pitch is isomorphic to the rationals,
-- and pitch class is isomorphic to the rationals modulo octaves
-- (powers of 2) -- so, for instance, 1 == 2 == 4 and (3/2) == 3 == 6.
type family Pitch app
type family PitchClass app

type instance Pitch      EdoApp = Int
type instance PitchClass EdoApp = Int
type instance Pitch      JiApp = Rational
type instance PitchClass JiApp = Rational

type LitPitches app = Map (PitchClass app) (Set LedBecause)
  -- ^ For each pitch class that is lit,
  -- we need to know why -- e.g. if it's being sustained,
  -- then we should not darken it when the finger on it is lifted,
  -- and if it's an anchor, we should never darken it.
  -- The Set is a Set because an LED could be on for multiple reasons.

type LedMsg = (WindowId, ((X,Y), Led))

-- | The reason a (pitch class of) LED(s) in the keyboard window is lit.
data LedBecause =
    LedBecauseSwitch (X,Y)
  | LedBecauseSustain
  | LedBecauseAnchor -- ^ Some "visual anchor" pitches are always on.
  deriving (Show, Eq, Ord)

data Window app = Window {
    windowLabel :: WindowId -- ^ PITFALL: Must be unique across windows,
    -- or the Eq instance fails.
  , windowContains :: (X,Y) -> Bool
    -- ^ PITFALL: A monome will respond to out-of-bounds (x,y) values.
    -- Every Window therefore needs a nontrivial windowContains field,
    -- even the background Window.
  , windowInit :: St app -> St app
  , windowHandler :: -- ^ Acts on messages from the monome.
      St app
      -> ((X,Y), Switch) -- ^ the incoming button press|release
      -> Either String (St app)
  }

-- | The app allows the monome to control "voices" in SuperCollider.
-- The `Synth` of a `Voice` is fixed, but the other values can change.
data Voice app = Voice {
    _voiceSynth  :: Maybe (Synth MoopParams) -- ^ This field is Nothing
    -- until SuperCollider has allocated a synth.
  , _voicePitch  :: Pitch app -- ^ Used (so far) only for sustain,
    -- by way of vid_to_pitchClass.
    -- One could instead find pitch by looking up "freq" in `_voiceParams,
    -- but that would be subject to floating point-induced mismatches.
  , _voiceParams :: Map String Float }

-- | So far, viable options for `app` in `St app` are `EdoApp` or `JiApp`.
data St app = St {
    _stApp :: app
  , _stWindowLayers :: [Window  app] -- ^ PITFALL: Order matters.
      -- Key presses are handled by the first window containing them.
      -- Windows listed earlier are thus "above" later ones.
  , _stToMonome :: Socket -- ^ PITFALL: It's tempting to remove this from St.
    -- That's feasible now, but I'll want it here when using multiple monomes.
  , _stVoices :: Map VoiceId (Voice app)

  -- | The purpose of `_stPending_Monome` and `_stPending_Vivid`
  -- is to isolate side-effects to a small portion of the code. Elsewhere,
  -- scattered functions can simply change an `St` instead of doing IO.
  , _stPending_Monome :: [LedMsg]
  , _stPending_Vivid :: [ScAction VoiceId]
  }

data EdoApp = EdoApp
  { _edoConfig :: EdoConfig
  , _edoXyShift :: (X,Y) -- ^ this is relative -- a vector, not a point
  , _edoFingers :: Map (X,Y) VoiceId
    -- ^ Where fingers are, what each is sounding,
    -- and what each is lighting up.
  , _edoLit :: LitPitches EdoApp
  , _edoSustaineded :: Maybe (Set VoiceId)
    -- ^ PITFALL: In spirit, the thing sustained is a Pitch,
    -- but it's represented as a voice,
    -- identified by the key that originally launched it.
  } deriving (Show, Eq)

-- | This is a just-intoned alternative to the EDO app.
-- It doesn't send LED messages.
-- I don't use it any more;
-- I used it just long enough to convince myself that EDO is the way for me.
--
-- If I remember right, the jiGenerator and the jiShifts are equivalent;
-- they might be better named "horizontal intervals" and "vertical intervals".
data JiApp = JiApp
  { _jiGenerator :: [Rational]
  , _jiShifts :: [Rational]
  , _jiFingers :: Map (X,Y) VoiceId -- ^ just like in the EdoApp
  }
  deriving (Show, Eq)

makeLenses ''Voice
makeLenses ''St
makeLenses ''EdoApp
makeLenses ''JiApp
