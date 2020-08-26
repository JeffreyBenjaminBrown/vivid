{-# LANGUAGE TypeFamilies,
TemplateHaskell #-}

module Montevideo.Monome.Types.Most (
    module Montevideo.Monome.Types.EdoConfig
  , module Montevideo.Monome.Types.Monome
  , Param, MonomeId(..), WindowId(..), VoiceId(..)
  , EdoPitch(..), EdoPitchClass(..)
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

data MonomeId = Monome_256 | Monome_128
  deriving (Show, Eq, Ord)

data WindowId = KeyboardWindow
              | ShiftWindow
              | SustainWindow
  deriving (Show, Eq, Ord)

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

newtype EdoPitch = EdoPitch {_unEdoPitch :: Int}
  deriving (Eq, Show, Ord)
newtype EdoPitchClass = EdoPitchClass {_unEdoPitchClass :: Int}
  deriving (Eq, Show, Ord)

type instance Pitch      EdoApp = EdoPitch
type instance PitchClass EdoApp = EdoPitchClass
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
  , windowInitLeds :: St app -> [LedMsg]
    -- ^ Some windows begin with some LEDs lit.
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
  , _stToMonome :: Map MonomeId Socket
  , _stVoices :: Map VoiceId (Voice app) -- ^ The set of all voices
    -- currently in existence; a reflection of the state of SC.
    --
    -- TODO : Updates to this are too scattered.
    -- Some come from Monome.Main.doScAction,
    -- some from Monome.(JI | Window.Keyboard).handler,
    -- and some from Monome.Window.Common.updateVoiceParams.

  -- | The purpose of `_stPending_Monome` and `_stPending_Vivid`
  -- is to isolate side-effects to a small portion of the code. Elsewhere,
  -- scattered functions can simply change an `St` instead of doing IO.
  , _stPending_Monome :: [(MonomeId, LedMsg)]
  , _stPending_Vivid :: [ScAction VoiceId]
  }

data EdoApp = EdoApp
  { _edoConfig :: EdoConfig
  , _edoXyShift :: (X,Y) -- ^ this is relative -- a vector, not a point
  , _edoFingers :: Map (X,Y) VoiceId
    -- ^ Where fingers are, what each is sounding,
    -- and what each is lighting up.
  , _edoLit :: LitPitches EdoApp
  , _edoSustaineded :: Set VoiceId
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
