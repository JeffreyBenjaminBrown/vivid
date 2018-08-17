-- | A few types are also defined in Jbb.Synths

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , TemplateHaskell
           , GADTs #-}

module Vivid.Jbb.Distrib.Types (
  SynthName
  , ParamName
  , MuseqName
  , Time, Duration
  , RTime, RDuration, RelDuration
  , unTimestamp
  , Msg
  , Action(..)
  , actionSynth
  , Museq(..), dur, sup, vec
  , emptyMuseq
  , unitMuseq
  , museq
  , SynthRegister(..)
  , emptySynthRegister
  , showSynthRegister
  , Distrib(..)
  , newDistrib
  , Msg'(..)
  , Action'(..)
  ) where

import Control.Concurrent.MVar
import Control.Lens (makeLenses, over, _2)
import Data.Map as M
import Data.Ratio
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Synths


-- | == The easy types

-- | = Kinds of name

type SynthName = String
type ParamName = String
type MuseqName = String


-- | = Kinds of time

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

type Msg = (ParamName,Float)

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
  -- in the `Museq`'s `vec`. If this is greater than `dur`, the `Museq`
  -- will rotate through different sections of the `vec` each time it plays.
  -- If less than `dur`, the `Museq` will play the entire `vec` more than
  -- once each time it plays.
  , _vec :: V.Vector (RTime, a) }
  deriving (Show,Eq)

makeLenses ''Museq

instance Functor Museq where
  fmap = over vec . V.map . over _2

emptyMuseq :: Museq a
emptyMuseq = Museq { _dur = 1, _sup = 1, _vec = V.empty }

unitMuseq :: Museq a -> Museq ()
unitMuseq = fmap $ const ()

museq :: RelDuration -> [(RTime,a)] -> Museq a
museq d tas = Museq {_dur = d, _sup = d, _vec = V.fromList tas}


-- | The global state

data SynthRegister = -- per-synth boilerplate
  SynthRegister { boops :: MVar (M.Map SynthName (Synth BoopParams))
                , vaps  :: MVar (M.Map SynthName (Synth VapParams))
                , sqfms :: MVar (M.Map SynthName (Synth SqfmParams))
                -- , zots :: MVar (M.Map SynthName (Synth ZotParams))
                }

emptySynthRegister :: IO SynthRegister
emptySynthRegister = do x <- newMVar M.empty
                        y <- newMVar M.empty
                        z <- newMVar M.empty
--                        w <- newMVar M.empty
                        return $ SynthRegister x y z -- w

-- | todo : this blocks if any MVar is empty
showSynthRegister :: SynthRegister -> IO String
showSynthRegister reg = do bs <- show <$> (readMVar $ boops reg)
                           vs <- show <$> (readMVar $ vaps reg )
                           ss <- show <$> (readMVar $ sqfms reg)
                           return $ bs ++ "\n" ++ vs ++ "\n" ++ ss

data Distrib = Distrib {
  mTimeMuseqs :: MVar (M.Map MuseqName (Time, Museq Action))
    -- ^ Each `Time` here is the next time that Museq is scheduled to run.
    -- Rarely, briefly, those `Time` values will be in the past.
  , reg :: SynthRegister
  , mTime0 :: MVar Time
  , mPeriod :: MVar Duration -- ^ Period is the inverse of tempo.
  }

-- | "new" because it's not really empty, except for `time0`
newDistrib :: IO Distrib
newDistrib = do
  mTimeMuseqs <- newMVar M.empty
  reg <- emptySynthRegister
  mTime0 <- newEmptyMVar
  mPeriod <- newMVar 1
  return Distrib { mTimeMuseqs = mTimeMuseqs,  reg     = reg
                 , mTime0  = mTime0         ,  mPeriod = mPeriod }


-- | == The GADTs. Hopefully quarantined away from the live coding.

data Msg' sdArgs where
  Msg' :: forall params sdArgs.
         (VarList params
         , Subset (InnerVars params) sdArgs)
      => params -> Msg' sdArgs

data Action' where
  New'  :: MVar (M.Map SynthName (Synth sdArgs))
       -> SynthDef sdArgs
       -> SynthName -> Action'
  Free' :: MVar (M.Map SynthName (Synth sdArgs))
       -> SynthName -> Action'
  Send' :: MVar (M.Map SynthName (Synth sdArgs))
       -> SynthName
       -> Msg' sdArgs -> Action'
