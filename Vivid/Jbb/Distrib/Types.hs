-- | A few types are also defined in Jbb.Synths

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , TemplateHaskell
           , GADTs #-}

module Vivid.Jbb.Distrib.Types where

import Control.Concurrent.MVar
import Control.Lens (makeLenses)
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

actionSynthInfo :: Action -> (SynthDefEnum, SynthName)
actionSynthInfo (New  s n  ) = (s,n)
actionSynthInfo (Free s n  ) = (s,n)
actionSynthInfo (Send s n _) = (s,n)

data Museq = Museq { _dur :: RelDuration
                   , _vec :: V.Vector (RTime, Action) }
  deriving (Show,Eq)

makeLenses ''Museq

emptyMuseq :: Museq
emptyMuseq = Museq { _dur = 1, _vec = V.empty }


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
  mTimeMuseqs :: MVar (M.Map MuseqName (Time, Museq))
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
