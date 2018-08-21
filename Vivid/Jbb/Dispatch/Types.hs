-- | A few types are also defined in Jbb.Synths

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , TemplateHaskell
           , GADTs
#-}

module Vivid.Jbb.Dispatch.Types (
  SynthName, ParamName, MuseqName
  , Time, Duration, RTime, RDuration, RelDuration, unTimestamp
  , Msg, Msg'(..)
  , Action(..), actionSynth
  , Museq(..), dur, sup, vec
  , emptyMuseq, museq
  , Museq'(..), dur', sup', vec'
  , emptyMuseq', museq'
  , SynthRegister(..), boops, vaps, sqfms
  , emptySynthRegister
  , Dispatch(..), newDispatch
  , Dispatch'(..), newDispatch'
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens (makeLenses, over, _2)
import Data.Map as M
import Data.Ratio
import qualified Data.Vector as V

import Vivid.Jbb.Dispatch.HasStart
import Vivid
import Vivid.Jbb.Synths


-- | == The easy types

-- | = Kinds of name

type ParamName = String
type SynthName = String
type MuseqName = String


-- | = Time

type Time = Double
type Duration = Double

type RTime = Rational
type RDuration = Rational

type RelDuration = Rational
  -- ^ Some durations are expressed relative to the global cycle duration.
  -- (An "R" prefix here would be redundant.)

unTimestamp :: Timestamp -> Double
unTimestamp (Timestamp x) = x


-- | = Instructions

type Msg = (ParamName, Float)

data Msg' sdArgs where
  Msg' :: forall params sdArgs.
          ( VarList params
          , Subset (InnerVars params) sdArgs)
       => params -> Msg' sdArgs

data Action = New  SynthDefEnum SynthName
            | Free SynthDefEnum SynthName
            | Send SynthDefEnum SynthName Msg
  deriving (Show,Eq,Ord)

actionSynth :: Action -> (SynthDefEnum, SynthName)
actionSynth (New  s n  ) = (s,n)
actionSynth (Free s n  ) = (s,n)
actionSynth (Send s n _) = (s,n)

data Museq a = Museq {
  _dur :: RelDuration -- ^ the play duration of the loop
  , _sup :: RelDuration -- ^ the supremum of the possible RTime values
    -- in `_vec`. If this is greater than `dur`, the `Museq`will rotate
    -- through different sections of the `vec` each time it plays.
    -- If less than `dur`, the `Museq` will play the entire `vec` more than
    -- once each time it plays.
  , _vec :: V.Vector (RTime, a) }
  deriving (Show,Eq)

makeLenses ''Museq

instance Functor Museq where
  fmap = over vec . V.map . over _2

emptyMuseq :: Museq a
emptyMuseq = Museq { _dur = 1, _sup = 1, _vec = V.empty }

museq :: RelDuration -> [(RTime,a)] -> Museq a
museq d tas = Museq {_dur = d, _sup = d, _vec = V.fromList tas}

data Museq' a = Museq' {
  _dur' :: RelDuration -- ^ the play duration of the loop
  , _sup' :: RelDuration -- ^ the supremum of the possible RTime values
    -- in `_vec`. If this is greater than `dur`, the `Museq`will rotate
    -- through different sections of the `vec` each time it plays.
    -- If less than `dur`, the `Museq` will play the entire `vec` more than
    -- once each time it plays.
  , _vec' :: V.Vector ((RTime,RTime), a) }
  deriving (Show,Eq)

makeLenses ''Museq'

instance Functor Museq' where
  fmap = over vec' . V.map . over _2

emptyMuseq' :: Museq' a
emptyMuseq' = Museq' { _dur' = 1, _sup' = 1, _vec' = V.empty }

museq' :: RelDuration -> [((RTime,RTime),a)] -> Museq' a
museq' d tas = Museq' {_dur' = d, _sup' = d, _vec' = V.fromList tas}


-- | The global state

data SynthRegister = -- per-synth boilerplate
  SynthRegister{ _boops :: M.Map SynthName (Synth BoopParams)
                , _vaps  :: M.Map SynthName (Synth VapParams)
                , _sqfms :: M.Map SynthName (Synth SqfmParams)
                } deriving (Show, Eq, Ord)

makeLenses ''SynthRegister

emptySynthRegister :: SynthRegister
emptySynthRegister = SynthRegister M.empty M.empty M.empty


data Dispatch = Dispatch {
  mMuseqs :: MVar (M.Map MuseqName (Museq Action))
  , mReg :: MVar SynthRegister
  , mTime0 :: MVar Time
  , mTempoPeriod :: MVar Duration
  }

-- | "new" because it's not really empty, except for `time0`
newDispatch :: IO Dispatch
newDispatch = do
  mTimeMuseqs <- newMVar M.empty
  reg <- newMVar emptySynthRegister
  mTime0 <- newEmptyMVar
  mTempoPeriod <- newMVar 1
  return Dispatch
    { mMuseqs = mTimeMuseqs,  mReg    = reg
    , mTime0      = mTime0     ,  mTempoPeriod = mTempoPeriod }

data Dispatch' = Dispatch' {
  mMuseqs' :: MVar (M.Map MuseqName (Museq' Action))
  , mReg' :: MVar SynthRegister
  , mTime0' :: MVar Time
  , mTempoPeriod' :: MVar Duration
  }

-- | "new" because it's not really empty, except for `time0`
newDispatch' :: IO Dispatch'
newDispatch' = do
  mTimeMuseqs <- newMVar M.empty
  reg <- newMVar emptySynthRegister
  mTime0 <- newEmptyMVar
  mTempoPeriod <- newMVar 1
  return Dispatch'
    { mMuseqs' = mTimeMuseqs,  mReg'    = reg
    , mTime0'  = mTime0     ,  mTempoPeriod' = mTempoPeriod }
