{-# LANGUAGE DeriveFunctor
, TemplateHaskell
, GADTs
#-}

module Montevideo.Dispatch.Types.Many (
    SynthName, ParamName, MuseqName
  , Msg, Msg'(..)
  , NamedWith
  , Action(..)
  , Event(..), evArc, evLabel, evData
  , Ev
  , Museq(..), dur, sup, vec
  , SynthRegister(..), boops, samplers, samples, sqfms, vaps, zots
  , Note(..), noteSd, noteMsg
  , Dispatch(..)
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Data.Map (Map)
import Data.Vector (Vector)
import Vivid

import Montevideo.Dispatch.Types.Time
import Montevideo.Synth
import Montevideo.Synth.Samples


-- | = Names

type ParamName = String
type SynthName = String
type MuseqName = String


-- | == Instructions

-- | = (synth) Messages and (synth) Actions

-- | A message type that knows nothing about Vivid's type-fussiness.
type Msg = Map ParamName Float

type NamedWith name a = (name, a)

-- | A `Msg'`, unlike a `Msg`, is typed for a particular kind of synth,
-- and to send it anywhere else is a type error.
-- (This innovation is Vivid's, not my own --
-- in fact I circumvent it with the `Msg` type.)
data Msg' sdArgs where
  Msg' :: forall params sdArgs.
          ( Vivid.VarList params
          , Vivid.Subset (Vivid.InnerVars params) sdArgs)
       => params -> Msg' sdArgs

-- | The `SynthDefEnum` gives the kind of synth.
-- (The options are at Montevideo/Synth/*.hs.)
-- The `SynthName` gives the name of the particular instance of that kind.
data Action
  = New  SynthDefEnum SynthName -- ^ create it
  | Free SynthDefEnum SynthName -- ^ destroy it
  | Send SynthDefEnum SynthName Msg
  deriving (Show, Eq, Ord)


-- | = an `Event` happens in time, and might have a name

data Event time label a =
  Event { _evLabel :: label -- ^ Sometimes you need to name your events,
        -- so that they can interact. For instance, in a slide guitar melody,
        -- if one note transitions into the next, they would share a name.
        , _evArc :: (time,time) -- ^ start time, end time
        , _evData :: a } -- ^ the thing that happens
  deriving (Show, Eq, Ord, Functor)
makeLenses ''Event

type Ev = Event RTime


-- | = a `Museq` is a sequence of `Event`s

data Museq label a = Museq {
    _dur :: RDuration -- ^ the play duration of the sequence (usually a loop)
  , _sup :: RDuration -- ^ supremum of the possible RTime values in `_vec`.
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
  , _noteMsg :: Msg } deriving (Show, Eq)
makeLenses ''Note

data Dispatch = Dispatch {
    mMuseqs      :: MVar (Map MuseqName (Museq String Note))
  , mReg         :: MVar SynthRegister
  , mTime0       :: MVar Time -- ^ a past moment of reference
  , mTempoPeriod :: MVar Duration
  }
