-- | A few types are also defined in Jbb.Synths

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , TupleSections
           , TemplateHaskell
           , GADTs
#-}

module Vivid.Dispatch.Types (
    SynthName, ParamName, MuseqName
  , FileSubPath, Description, Nickname, Filename
  , Time, Duration, RTime(..), RDuration, unTimestamp
  , Msg, Msg'(..)
  , NamedWith, mNamed, anon
  , Action(..), actionToSynth
  , Event(..), showEvs, mkEv, mkEv0
  , evArc   , evLabel   , evData   , evStart   , evEnd
  , eventRTimeToEventTime
  , Ev
  , Museq(..), dur, sup, vec
  , emptyMuseq
  , SynthRegister(..), boops, samplers, samples, sqfms, vaps
  , emptySynthRegister
  , Note(..), noteSd, noteMsg
  , Dispatch(..), newDispatch
  ) where

import Control.Concurrent.MVar
import Control.Lens (makeLenses, over, _1, _2, Lens')
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Synths
import Util


-- | == The easy types

-- | = Kinds of name

type ParamName = String
type SynthName = String
type MuseqName = String

type FileSubPath = String -- ^ `FilePath` is already defined in Base
type Description = String
type Nickname = String
type Filename = String -- ^ without a path


-- | = Time. Some durations are relative to something,
-- e.g. the global cycle duration.

type Time = Rational
type Duration = Rational
newtype RTime = RTime Rational deriving (Ord,Eq)
type RDuration = RTime

instance Show RTime where show (RTime t) = show t
instance Num RTime where (+) (RTime t) (RTime s) = RTime (t + s)
                         (-) (RTime t) (RTime s) = RTime (t - s)
                         (*) (RTime t) (RTime s) = RTime (t * s)
                         negate (RTime t) = RTime (negate t)
                         abs (RTime t) = RTime (abs t)
                         signum (RTime t) = RTime (signum t)
                         fromInteger int = RTime (fromInteger int)
instance Fractional RTime where (/) (RTime t) (RTime s) = RTime (t / s)
                                recip (RTime t) = RTime (recip t)
                                fromRational rat = RTime rat
instance Real RTime where toRational (RTime t) = t
instance RealFrac RTime where
  properFraction (RTime r0) = let (i,r) = properFraction r0
                              in (i,RTime r)
  truncate (RTime r) = truncate r
  round (RTime r) = round r
  ceiling (RTime r) = ceiling r
  floor (RTime r) = floor r

unTimestamp :: Timestamp -> Time
unTimestamp (Timestamp x) = toRational x


-- | = Instructions

type Msg = M.Map ParamName Float

type NamedWith t a = (t, a)

mNamed :: t -> a -> NamedWith (Maybe t) a
mNamed n = (Just n , )

anon :: a -> NamedWith (Maybe t) a
anon = (Nothing , )

data Msg' sdArgs where
  Msg' :: forall params sdArgs.
          ( VarList params
          , Subset (InnerVars params) sdArgs)
       => params -> Msg' sdArgs

-- | The `SynthDefEnum` gives the kind of synth.
-- The `SynthName` gives the name of the particular instance of that kind.
data Action = New  SynthDefEnum SynthName
            | Free SynthDefEnum SynthName
            | Send SynthDefEnum SynthName Msg
  deriving (Show,Eq,Ord)

actionToSynth :: Action -> (SynthDefEnum, SynthName)
actionToSynth (New  s n  ) = (s,n)
actionToSynth (Free s n  ) = (s,n)
actionToSynth (Send s n _) = (s,n)


data Event time label a = Event { _evLabel :: label
                                , _evArc :: (time,time)
                                , _evData :: a} deriving (Show, Eq, Ord)
makeLenses ''Event

type Ev = Event RTime

eventRTimeToEventTime :: Event RTime l a -> Event Time l a
eventRTimeToEventTime ev = let (s,t) = _evArc ev
                           in ev { _evArc = (tr s, tr t) }

evStart, evEnd :: Lens' (Event t l a) t
evStart = evArc . _1
evEnd   = evArc . _2

showEvs :: (Foldable t, Show a, Show label)
        => t (Ev label a) -> String
showEvs = foldl (\acc ev -> acc ++ show ev) "\n"

mkEv :: l -> Rational -> Rational -> a -> Ev l a
mkEv l s e a = Event l (fr s, fr e) a

mkEv0 :: l -> Rational -> a -> Ev l a -- ^ duration-0 events
mkEv0 l t a = Event l (fr t, fr t) a

data Museq label a = Museq {
  _dur :: RDuration -- ^ the play duration of the loop
  , _sup :: RDuration -- ^ the supremum of the possible RTime values
    -- in `_vec`. If this is greater than `dur`, the `Museq`will rotate
    -- through different sections of the `vec` each time it plays.
    -- If less than `dur`, the `Museq` will play the entire `vec` more than
    -- once each time it plays.
  , _vec :: V.Vector (Ev label a) }
  deriving (Show,Eq)
makeLenses ''Museq

instance Functor (Museq label) where
  fmap = over vec . V.map . over evData

emptyMuseq :: Museq label a
emptyMuseq = Museq { _dur = 1, _sup = 1, _vec = V.empty }


-- | The global state

data SynthRegister = -- per-synth boilerplate
  SynthRegister
  { _boops    :: M.Map SynthName (Synth BoopParams)
  , _vaps     :: M.Map SynthName (Synth VapParams)
  , _samplers :: M.Map SynthName (Synth SamplerParams)
  , _samples  :: M.Map String BufferId -- ^ the samplers will use these
  , _sqfms    :: M.Map SynthName (Synth SqfmParams)
  } deriving (Show, Eq, Ord)
makeLenses ''SynthRegister

emptySynthRegister :: SynthRegister
emptySynthRegister = SynthRegister
  M.empty M.empty M.empty M.empty M.empty

data Note = Note { _noteSd :: SynthDefEnum
                 , _noteMsg :: Msg } deriving (Show, Eq)
makeLenses ''Note

data Dispatch = Dispatch {
    mMuseqs      :: MVar (M.Map MuseqName (Museq String Note))
  , mReg         :: MVar SynthRegister
  , mTime0       :: MVar Time
  , mTempoPeriod :: MVar Duration
  }

-- | "new" because it's not really empty, except for `time0`
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
