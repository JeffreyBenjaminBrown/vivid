{-# LANGUAGE DeriveFunctor
, TemplateHaskell
, KindSignatures
, GADTs
#-}

module Montevideo.Dispatch.Types.Many (
    SynthName, MuseqName
  , NamedWith
  , Event(..), evArc, evLabel, evData
  , Ev
  , Museq(..), dur, sup, vec
  , SynthRegister(..), boops, samplers, samples, sqfms, vaps, zots
  , Note(..), noteSd, noteScParams
  , Dispatch(..)
  , Recording(..), recordingStart, recordingEnd, recordingData
  , Observation(..), observationTime, observationData
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Data.Map (Map)
import Data.Vector (Vector)
import Vivid

import Montevideo.Dispatch.Types.Time
import Montevideo.Synth
import Montevideo.Synth.Msg
import Montevideo.Synth.Samples


-- | = Names

type SynthName = String
type MuseqName = String


-- | == Instructions

-- | = (synth) Messages and (synth) `ScAction`s

type NamedWith name a = (name, a)

-- | = an `Event` happens in time, and might have a name

-- | dn `Event time label a` indicates that `a` should happen to
-- the voice called `label` at time `time`.
data Event time label a =
  Event { _evLabel :: label
        , _evArc :: (time,time) -- ^ start time, end time
        , _evData :: a } -- ^ the thing that happens
  deriving (Show, Eq, Ord, Functor)
makeLenses ''Event

type Ev = Event RTime


-- | = a `Museq` is a sequence of `Event`s

-- | All times in a `Museq` are relative to the `Dispatch`'s tempo period.
data Museq label a = Museq {
    _dur :: RDuration -- ^ the play duration of the sequence (usually a loop)
  , _sup :: RDuration -- ^ supremum of possible start `RTime`s in `_vec`.
    -- The events in `_vec` can start at any time in the half-open
    -- interval [0,_sup).
    -- If this is greater than `dur`, the `Museq`will rotate
    -- through different sections of the `vec` each time it plays.
    -- If less than `dur`, the `Museq` will play the `vec` more than
    -- once (in general not a whole number of times) each time it plays.
  , _vec :: Vector (Ev label a) }
  deriving (Show, Eq)
makeLenses ''Museq


-- | The global state

data SynthRegister =
  -- | You might not have any `Zot`s, for instance, but you still need a
  -- `_zots` field, since you might use a `Zot` synth later.
  -- I believe this per-synth boilerplate is unavoidable,
  -- because Vivid types each synth by the arguments it accepts.
  SynthRegister
  { _boops    :: Map SynthName (Synth BoopParams)
  , _vaps     :: Map SynthName (Synth VapParams)
  , _samplers :: Map SynthName (Synth SamplerParams)
  , _samples  :: Map Sample BufferId -- ^ the samplers will use these
  , _sqfms    :: Map SynthName (Synth SqfmParams)
  , _zots     :: Map SynthName (Synth ZotParams)
  } deriving (Show, Eq, Ord)
makeLenses ''SynthRegister

data Note = Note
  { _noteSd :: SynthDefEnum
  , _noteScParams :: ScParams }
  deriving (Show, Eq)
makeLenses ''Note

data Dispatch = Dispatch {
    mMuseqs      :: MVar (Map MuseqName (Museq String Note))
  , mReg         :: MVar SynthRegister
  , mTime0       :: MVar Time -- ^ a past moment of reference
  , mTempoPeriod :: MVar Duration
  }

data Observation a = Observation
  { _observationTime :: Time
  , _observationData :: a }
makeLenses ''Observation

-- | `Recording`s are for transforming into `Museq`s.
data Recording (a :: * -> *) label = Recording
  { _recordingStart :: Time
  , _recordingEnd :: Maybe Time
  , _recordingData :: [ Observation (a label) ] }
makeLenses ''Recording
