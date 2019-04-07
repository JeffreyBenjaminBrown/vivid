-- | A few types are also defined in Jbb.Synths

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , TupleSections
           , TemplateHaskell
           , GADTs
#-}

module Vivid.Jbb.Dispatch.Types (
  SynthName, ParamName, MuseqName
  , Time, Duration, RTime(..), RDuration, unTimestamp
  , Msg, Msg'(..)
  , NamedWith, mNamed, anon
  , Action(..), actionToSynth
  , Ev     , showEvs
  , Event(..), showEvs', ev, ev0
  , evArc   , evLabel   , evData   , evStart   , evEnd
  , eventRTimeToEventTime
  , Ev'
  , Museq(..) , dur , sup , vec
  , emptyMuseq
  , Museq'(..), dur', sup', vec'
  , emptyMuseq'
  , SynthRegister(..), boops, vaps, sqfms
  , emptySynthRegister
  , Note
  , Note'(..), noteSd, noteMsg
  , Dispatch(..),  newDispatch
  , Dispatch'(..), newDispatch'
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens (makeLenses, over, _1, _2, Lens', view)
import qualified Data.Map as M
import Data.Ratio
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Util


-- | == The easy types

-- | = Kinds of name

type ParamName = String
type SynthName = String
type MuseqName = String


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
  properFraction (RTime r) = let (i,r) = properFraction r in (i,RTime r)
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


type Ev a = ( (RTime,RTime), a) -- ^ start and end time, I guess

showEvs :: (Foldable t, Show a) => t (Ev a) -> String
showEvs evs = concatMap (\(t,a) -> "\n" ++ show t ++ ": " ++ show a) evs

data Event time label a = Event { _evLabel :: label
                                , _evArc :: (time,time)
                                , _evData :: a} deriving (Show, Eq, Ord)
makeLenses ''Event

type Ev' = Event RTime

eventRTimeToEventTime :: Event RTime l a -> Event Time l a
eventRTimeToEventTime ev = let (s,t) = _evArc ev
                           in ev { _evArc = (tr s, tr t) }

evStart, evEnd :: Lens' (Event t l a) t
evStart = evArc . _1
evEnd = evArc . _2

showEvs' :: (Foldable t, Show a, Show label)
        => t (Ev' label a) -> String
showEvs' = foldl (\acc ev -> acc ++ show ev) "\n"

ev :: l -> Rational -> Rational -> a -> Ev' l a
ev l s e a = Event l (fr s, fr e) a

ev0 :: l -> Rational -> a -> Ev' l a -- ^ duration-0 events
ev0 l t a = Event l (fr t, fr t) a

data Museq a = Museq {
    _dur :: RDuration -- ^ the play duration of the loop
  , _sup :: RDuration -- ^ the supremum of the possible RTime values
    -- in `_vec`. If this is greater than `dur`, the `Museq`will rotate
    -- through different sections of the `vec` each time it plays.
    -- If less than `dur`, the `Museq` will play the entire `vec` more than
    -- once each time it plays.
  , _vec :: V.Vector (Ev a) }
  deriving (Show,Eq)
makeLenses ''Museq

instance Functor Museq where
  fmap = over vec . V.map . over _2

emptyMuseq :: Museq a
emptyMuseq = Museq { _dur = 1, _sup = 1, _vec = V.empty }

data Museq' label a = Museq' {
  _dur' :: RDuration -- ^ the play duration of the loop
  , _sup' :: RDuration -- ^ the supremum of the possible RTime values
    -- in `_vec`. If this is greater than `dur`, the `Museq`will rotate
    -- through different sections of the `vec` each time it plays.
    -- If less than `dur`, the `Museq` will play the entire `vec` more than
    -- once each time it plays.
  , _vec' :: V.Vector (Ev' label a) }
  deriving (Show,Eq)
makeLenses ''Museq'

instance Functor (Museq' label) where
  fmap = over vec' . V.map . over evData

emptyMuseq' :: Museq' label a
emptyMuseq' = Museq' { _dur' = 1, _sup' = 1, _vec' = V.empty }


-- | The global state

data SynthRegister = -- per-synth boilerplate
  SynthRegister{  _boops :: M.Map SynthName (Synth BoopParams)
                , _vaps  :: M.Map SynthName (Synth VapParams)
                , _sqfms :: M.Map SynthName (Synth SqfmParams)
                } deriving (Show, Eq, Ord)
makeLenses ''SynthRegister

emptySynthRegister :: SynthRegister
emptySynthRegister = SynthRegister M.empty M.empty M.empty

type Note  = NamedWith String (SynthDefEnum, Msg)

data Note' = Note' { _noteSd :: SynthDefEnum
                   , _noteMsg :: Msg } deriving (Show, Eq)
makeLenses ''Note'

data Dispatch = Dispatch {
    mMuseqs :: MVar (M.Map MuseqName (Museq Note))
  , mReg :: MVar SynthRegister
  , mTime0 :: MVar Time
  , mTempoPeriod :: MVar Duration
  }

-- | PITFALL: the `mTime0` field begins empty.
newDispatch :: IO Dispatch
newDispatch = do
  mTimeMuseqs  <- newMVar M.empty
  mReg         <- newMVar emptySynthRegister
  mTime0       <- newEmptyMVar
  mTempoPeriod <- newMVar 1
  return Dispatch { mMuseqs      = mTimeMuseqs
                  , mReg         = mReg
                  , mTime0       = mTime0
                  , mTempoPeriod = mTempoPeriod }

data Dispatch' = Dispatch' {
    mMuseqs'      :: MVar (M.Map MuseqName (Museq' String Note'))
  , mReg'         :: MVar SynthRegister
  , mTime0'       :: MVar Time
  , mTempoPeriod' :: MVar Duration
  }

-- | "new" because it's not really empty, except for `time0`
newDispatch' :: IO Dispatch'
newDispatch' = do
  mTimeMuseqs  <- newMVar M.empty
  mReg         <- newMVar emptySynthRegister
  mTime0       <- newEmptyMVar
  mTempoPeriod <- newMVar 1
  return Dispatch' { mMuseqs'      = mTimeMuseqs
                   , mReg'         = mReg
                   , mTime0'       = mTime0
                   , mTempoPeriod' = mTempoPeriod }
