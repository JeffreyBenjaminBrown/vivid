{-# LANGUAGE TypeFamilies,
FlexibleContexts,
UndecidableInstances,
TemplateHaskell #-}

module Montevideo.Monome.Types.Initial (
    HostName, Socket
  , Param, WindowId, VoiceId
  , LitPitches
  , LedMsg
  , Pitch, PitchClass
  , SoundMsg(..), soundMsgVoiceId, soundMsgPitch, soundMsgVal, soundMsgParam
  , X, Y, Switch, Led
  , LedBecause(..)
  , Window(..)
  , Voice(..), voiceSynth, voicePitch, voiceParams
  , St(..), stApp, stWindowLayers, stToMonome, stVoices
    , stPending_Monome, stPending_Vivid
  , EdoApp(..), etXyShift, etFingers, etLit, etSustaineded
  , JiApp(..), jiGenerator, jiShifts, jiFingers
  ) where

import           Control.Lens
import           Data.Map
import           Data.Set
import qualified Network.Socket as NS
import           Vivid hiding (Param)

import Montevideo.Synth.Boop_Monome


type HostName = NS.HostName
type Socket = NS.Socket

-- | X and Y are coordinates on the monome.
-- PITFALL: X rises from left to right, but Y rises from top to bottom.
-- Thus (0,1) is just under the top-left corner.
-- PITFALL: The monome will respond to out-of-bounds (x,y) values.
-- I don't use that feature.
type X = Int
type Y = Int

type Switch = Bool -- | Whether a monome button is pressed.
type Led    = Bool -- | Whether a monome LED is lit.

type Param = String
type WindowId = String
type VoiceId = (X,Y)

-- | In the Equal Tempered app, Pitch is isomorphic to the integers, and
-- PitchClass is isomorphic to the integers modulo 31.
-- That is, PitchClass 0 is identical to PitchClass 31,
-- whereas Pitch 31 is an octave above Pitch 0.
-- Pitch classes are (so far) only relevant in the visual context:
-- if a key is lit up on the keyboard, so are all enharmonic notes,
-- in all octaves.
--
-- In the Just Tempered app, Pitch is isomorphic to the rationals,
-- and pitch class is isomorphic to the rationals modulo powers of 2 --
-- i.e. the rationals in the half-open interval [1,2).
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

type LedMsg     = (WindowId, ((X,Y), Led))
data SoundMsg app = SoundMsg {
    _soundMsgVoiceId :: VoiceId
  , _soundMsgPitch   :: Maybe (Pitch app)
    -- ^ messages like "off" don't need one
  , _soundMsgVal     :: Float
  , _soundMsgParam   :: Param }

instance Show (Pitch app) => Show (SoundMsg app) where
  show sm = "SoundMsg {_soundMsgVoiceId = " ++ show (_soundMsgVoiceId sm)
                 ++ ", _soundMsgPitch = "   ++ show (_soundMsgPitch sm)
                 ++ ", _soundMsgVal = "     ++ show (_soundMsgVal sm)
                 ++ ", _soundMsgParam = "   ++ show (_soundMsgParam sm) ++ "}"

instance Eq (Pitch app) => Eq (SoundMsg app) where
  a == b =
    _soundMsgVoiceId a == _soundMsgVoiceId b &&
    _soundMsgPitch a   == _soundMsgPitch b   &&
    _soundMsgVal a     == _soundMsgVal b     &&
    _soundMsgParam a   == _soundMsgParam b
instance (Eq (Pitch app), Ord (Pitch app))
         => Ord (SoundMsg app) where
  a <= b =
    if      not $ _soundMsgVoiceId a == _soundMsgVoiceId b
    then          _soundMsgVoiceId a <= _soundMsgVoiceId b
    else if not $ _soundMsgPitch a   == _soundMsgPitch b
    then          _soundMsgPitch a   <= _soundMsgPitch b
    else if not $ _soundMsgVal a     == _soundMsgVal b
    then          _soundMsgVal a     <= _soundMsgVal b
    else          _soundMsgParam a   <= _soundMsgParam b

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
  , windowRoutine :: -- ^ Acts on messages from the monome.
      St app
      -> ((X,Y), Switch) -- ^ the incoming button press|release
      -> St app
  }

instance Eq (Window app) where
  (==) a b = windowLabel a == windowLabel b

instance Show (Window app) where
  show = windowLabel

data Voice app = Voice {
    _voiceSynth  :: Synth BoopParams
  , _voicePitch  :: Pitch app
  , _voiceParams :: Map String Float }

instance Show (Pitch app) => Show (Voice app) where
  show v = "Voice "
    ++ "{ _voiceSynth = "  ++ show (_voiceSynth v)
    ++ "{ _voicePitch = "  ++ show (_voicePitch v)
    ++ "{ _voiceParams = " ++ show (_voiceParams v)

instance Eq (Pitch app) => Eq (Voice app) where
  a == b = and
    [ _voiceSynth a  == _voiceSynth b
    , _voicePitch a  == _voicePitch b
    , _voiceParams a == _voiceParams b ]

data St app = St {
    _stApp :: app
  , _stWindowLayers :: [Window  app] -- ^ PITFALL: Order matters.
      -- Key presses are handled by the first window containing them.
      -- Windows listed earlier are thus "above" later ones.
  , _stToMonome :: Socket -- ^ PITFALL: It's tempting to remove this from St.
    -- That's feasible now, ll want it here when using multiple monomes.
  , _stVoices :: Map VoiceId (Voice app)
    -- ^ TODO ? This is expensive, precluding the use of big synths.
    -- Maybe I could make them dynamically without much speed penalty.
    -- Tom of Vivid thinks so.

  -- | The purpose of `_stPending_Monome` and `_stPending_Vivid`
  -- is to isolate side-effects to a small portion of the code. Elsewhere,
  -- scattered functions can simply change an `St` instead of doing IO.
  , _stPending_Monome :: [LedMsg]
  , _stPending_Vivid :: [SoundMsg app]
  }

instance (Show app, Show (Pitch app)) => Show (St app) where
  show app = "St "
    ++ "{ _stApp app = "            ++ show (_stApp app)
    ++ ", _stWindowLayers app = "   ++ show (_stWindowLayers app)
    ++ ", _stToMonome app = "       ++ show (_stToMonome app)
    ++ ", _stVoices app = "         ++ show (_stVoices app)
    ++ ", _stPending_Monome app = " ++ show (_stPending_Monome app)
    ++ ", _stPending_Vivid app = "  ++ show (_stPending_Vivid app)

instance (Eq app, Eq (Pitch app)) => Eq (St app) where
  a == b = and
    [ _stApp a           == _stApp b
    , _stWindowLayers a  == _stWindowLayers b
    , _stToMonome a      == _stToMonome b
    , _stVoices a        == _stVoices b
    , _stPending_Monome a == _stPending_Monome b
    , _stPending_Vivid a  == _stPending_Vivid b ]

data EdoApp = EdoApp
  { _etXyShift :: (X,Y) -- ^ this is relative -- a vector, not a point
  , _etFingers :: Map (X,Y) VoiceId
    -- ^ Where fingers are, what each is sounding,
    -- and what each is lighting up.
  , _etLit :: LitPitches EdoApp
  , _etSustaineded :: Maybe (Set VoiceId)
    -- ^ PITFALL: In spirit, the thing sustained is a Pitch,
    -- but it's represented as a voice,
    -- identified by the key that originally launched it.
  } deriving (Show, Eq)

data JiApp = JiApp
  { _jiGenerator :: [Rational]
  , _jiShifts :: [Rational]
  , _jiFingers :: Map (X,Y) VoiceId }
  deriving (Show, Eq)

makeLenses ''SoundMsg
makeLenses ''Voice
makeLenses ''St
makeLenses ''EdoApp
makeLenses ''JiApp
