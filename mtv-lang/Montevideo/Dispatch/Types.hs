-- | A few types are also defined in Jbb.Synths

{-# LANGUAGE DataKinds
, DeriveFunctor
, ExtendedDefaultRules
, ScopedTypeVariables
, TupleSections
, TemplateHaskell
, GADTs
#-}

module Montevideo.Dispatch.Types (
    SynthName, ParamName, MuseqName
  , Time, Duration, RTime(..), RDuration
  , Start, End, RStart, REnd
  , unTimestamp
  , Msg, Msg'(..)
  , NamedWith, mNamed, anon
  , Action(..), actionToSynth
  , Event(..), showEvs, mkEv, mkEv0
  , evArc   , evLabel   , evData   , evStart   , evEnd
  , eventRTimeToEventTime
  , Ev
  , Museq(..), dur, sup, vec
  , emptyMuseq
  , SynthRegister(..), boops, samplers, samples, sqfms, vaps, zots
  , emptySynthRegister
  , Note(..), noteSd, noteMsg
  , Dispatch(..), newDispatch
  ) where

import Control.Concurrent.MVar
import Control.Lens (makeLenses, over, _1, _2, Lens')
import qualified Data.Map as M
import qualified Data.Vector as V
import Vivid

import Montevideo.Synth
import Montevideo.Synth.Samples
import Montevideo.Util


-- | = Names

type ParamName = String
type SynthName = String
type MuseqName = String


-- | = Time

type Time      = Rational -- ^ a moment in time
type Start     = Time
type End       = Time
type Duration  = Rational -- ^ a length of time

-- | TODO ? Maybe `RTime` means "relative time" --
-- relative to something, e.g. the global cycle duration.
-- Or maybe (I hope not) it means "wrapped time"?
newtype RTime  = RTime Rational deriving (Ord,Eq)
type RStart    = RTime
type REnd      = RTime
type RDuration = RTime

instance Show RTime where
  show (RTime t) = show t

instance Num RTime where
  (+) (RTime t) (RTime s) = RTime (t + s)
  (-) (RTime t) (RTime s) = RTime (t - s)
  (*) (RTime t) (RTime s) = RTime (t * s)
  negate (RTime t) = RTime (negate t)
  abs (RTime t) = RTime (abs t)
  signum (RTime t) = RTime (signum t)
  fromInteger int = RTime (fromInteger int)

instance Fractional RTime where
  (/) (RTime t) (RTime s) = RTime (t / s)
  recip (RTime t) = RTime (recip t)
  fromRational rat = RTime rat

instance Real RTime where
  toRational (RTime t) = t

instance RealFrac RTime where
  properFraction (RTime r0) = let (i,r) = properFraction r0
                              in (i,RTime r)
  truncate (RTime r) = truncate r
  round (RTime r) = round r
  ceiling (RTime r) = ceiling r
  floor (RTime r) = floor r

unTimestamp :: Vivid.Timestamp -> Time
unTimestamp (Timestamp x) = toRational x


-- | == Instructions

-- | = (synth) Messages and (synth) Actions

-- | A message type that knows nothing about Vivid's type-fussiness.
type Msg = M.Map ParamName Float

type NamedWith name a = (name, a)

mNamed :: n -> a -> NamedWith (Maybe n) a
mNamed n = (Just n , )

anon :: a -> NamedWith (Maybe n) a
anon = (Nothing , )

-- | A `Msg'`, unlike a `Msg`, is typed for a particular kind of synth,
-- and to send it anywhere else is a type error.
-- (This innovation is Vivid's, not my own --
-- in fact I actively circumvent it, with the `Msg` type.)
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
  deriving (Show,Eq,Ord)

-- | From an action, extract the synth it is for.
actionToSynth :: Action -> (SynthDefEnum, SynthName)
actionToSynth (New  s n  ) = (s,n)
actionToSynth (Free s n  ) = (s,n)
actionToSynth (Send s n _) = (s,n)


-- | = an `Event` happens in time, and might have a name

data Event time label a =
  Event { _evLabel :: label
        , _evArc :: (time,time) -- ^ start time, end time
        , _evData :: a}
  deriving (Show, Eq, Ord, Functor)
makeLenses ''Event

type Ev = Event RTime

eventRTimeToEventTime :: Event RTime l a -> Event Time l a
eventRTimeToEventTime ev =
  let (s,t) = _evArc ev
  in ev { _evArc = (tr s, tr t) }

evStart, evEnd :: Lens' (Event t l a) t
evStart = evArc . _1
evEnd   = evArc . _2

showEvs :: (Foldable t, Show a, Show label)
        => t (Ev label a) -> String
showEvs = foldl (\acc ev -> acc ++ show ev) "\n"

mkEv :: l -> Rational -> Rational -> a -> Ev l a
mkEv l s e a = Event l (fr s, fr e) a

-- ^ make a duration-0 event
mkEv0 :: l -> Rational -> a -> Ev l a
mkEv0 l t a = Event l (fr t, fr t) a


-- | = a `Museq` is a sequence of `Event`s

data Museq label a = Museq {
    _dur :: RDuration -- ^ the play duration of the sequence (usually a loop)
  , _sup :: RDuration -- ^ supremum of the possible RTime values in `_vec`.
    -- If this is greater than `dur`, the `Museq`will rotate
    -- through different sections of the `vec` each time it plays.
    -- If less than `dur`, the `Museq` will play the `vec` more than
    -- once (in general not a whole number of times) each time it plays.
  , _vec :: V.Vector (Ev label a) }
  deriving (Show, Eq)
makeLenses ''Museq

instance Functor (Museq label) where
  fmap = over vec . V.map . over evData

emptyMuseq :: Museq label a
emptyMuseq = Museq { _dur = 1, _sup = 1, _vec = V.empty }


-- | The global state

data SynthRegister =
  -- You might not have any zots, for instance, but you still need a
  -- `_zots` field, since you might use the Zot synth later.
  -- I believe this per-synth boilerplate is unavoidable.
  -- That's because Vivid types each synth by the arguments it accepts.
  SynthRegister
  { _boops    :: M.Map SynthName (Synth BoopParams)
  , _vaps     :: M.Map SynthName (Synth VapParams)
  , _samplers :: M.Map SynthName (Synth SamplerParams)
  , _samples  :: M.Map Sample BufferId -- ^ the samplers will use these
  , _sqfms    :: M.Map SynthName (Synth SqfmParams)
  , _zots     :: M.Map SynthName (Synth ZotParams)
  } deriving (Show, Eq, Ord)
makeLenses ''SynthRegister

emptySynthRegister :: SynthRegister
emptySynthRegister = SynthRegister
  { _boops = mempty
  , _vaps = mempty
  , _samplers = mempty
  , _samples = mempty
  , _sqfms = mempty
  , _zots = mempty }

data Note = Note
  { _noteSd :: SynthDefEnum
  , _noteMsg :: Msg } deriving (Show, Eq)
makeLenses ''Note

data Dispatch = Dispatch {
    mMuseqs      :: MVar (M.Map MuseqName (Museq String Note))
  , mReg         :: MVar SynthRegister
  , mTime0       :: MVar Time -- ^ a past moment of reference
  , mTempoPeriod :: MVar Duration
  }

newDispatch :: IO Dispatch
newDispatch = do
  a <- newMVar M.empty
  b <- newMVar emptySynthRegister
  c <- newEmptyMVar
  d <- newMVar 1
  return Dispatch { mMuseqs      = a
                  , mReg         = b
                  , mTime0       = c
                  , mTempoPeriod = d }
