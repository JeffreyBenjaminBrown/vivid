{-# LANGUAGE DeriveFunctor
, TemplateHaskell
, GADTs
#-}

module Montevideo.Dispatch.Types.Many (
    SynthName, ParamName, MuseqName
  , ScParams, ScParams'(..)
  , NamedWith
  , ScAction(..), _ScAction_New, _ScAction_Send, _ScAction_Free
  , Event(..), evArc, evLabel, evData
  , Ev
  , Museq(..), dur, sup, vec
  , SynthRegister(..), boops, samplers, samples, sqfms, vaps, zots
  , Note(..), noteSd, noteScParams
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

-- | = (synth) Messages and (synth) ScActions

-- | A message for SuperCollider unaware of Vivid's type shenanigans..
type ScParams = Map ParamName Float

type NamedWith name a = (name, a)

-- | A `ScParams'`, unlike a `Msg`, is typed for a particular kind of synth,
-- and to send it anywhere else is a type error.
-- (This innovation is Vivid's, not my own --
-- in fact I circumvent it with the `Msg` type.)
data ScParams' sdArgs where
  ScParams' :: forall params sdArgs.
          ( Vivid.VarList params
          , Vivid.Subset (Vivid.InnerVars params) sdArgs)
       => params -> ScParams' sdArgs

-- | A SuperCollider action: create a voice, destroy (free)  a voice,
-- or change a voice's parameters.
data ScAction labelType
  = ScAction_New  -- ^ create it
    { _actionSynthDefEnum :: SynthDefEnum -- ^ The kind of synth.
    , _actionSynthName    :: labelType -- ^ Which instance of the synth.
    , _actionScParams     :: ScParams -- ^ Can be the empty map.
      -- In fact, in mtv-lang (Montevideo.Dispatch), it always is,
      -- because voices are created in advance of being used.
    }
  | ScAction_Free -- ^ destroy it
    { _actionSynthDefEnum :: SynthDefEnum -- ^ The kind of synth.
    , _actionSynthName    :: labelType } -- ^ Which instance of the synth.
  | ScAction_Send
    { _actionSynthDefEnum :: SynthDefEnum -- ^ The kind of synth.
    , _actionSynthName    :: labelType -- ^ Which instance of the synth.
    , _actionScParams     :: ScParams } -- ^ This should not be the empty map.
  deriving (Show, Eq, Ord)
makePrisms ''ScAction

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
  , _noteScParams :: ScParams }
  deriving (Show, Eq)
makeLenses ''Note

data Dispatch = Dispatch {
    mMuseqs      :: MVar (Map MuseqName (Museq String Note))
  , mReg         :: MVar SynthRegister
  , mTime0       :: MVar Time -- ^ a past moment of reference
  , mTempoPeriod :: MVar Duration
  }
