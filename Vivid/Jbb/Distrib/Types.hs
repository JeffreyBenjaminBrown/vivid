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
import Data.Vector

import Vivid
import Vivid.Jbb.Synths


-- | == The easy types

type SynthString = String
type ParamString = String


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

type Msg = (ParamString,Float)

data Action = New  SynthDefEnum SynthString
            | Free SynthDefEnum SynthString
            | Send SynthDefEnum SynthString Msg
  deriving (Show,Eq,Ord)

data Museq = Museq { _dur :: RelDuration
                   , _vec :: Vector (RTime, Action) }
  deriving (Show,Eq)

makeLenses ''Museq


-- | The global state

data SynthRegister = -- per-synth boilerplate
  SynthRegister { boops :: MVar (M.Map SynthString (Synth BoopParams))
                , vaps  :: MVar (M.Map SynthString (Synth VapParams))
                , sqfms :: MVar (M.Map SynthString (Synth SqfmParams))
                -- , zots :: MVar (M.Map SynthString (Synth ZotParams))
                }

emptySynthRegister :: IO SynthRegister
emptySynthRegister = do x <- newMVar M.empty
                        y <- newMVar M.empty
                        z <- newMVar M.empty
--                        w <- newMVar M.empty
                        return $ SynthRegister x y z -- w

data Distrib = Distrib {
  mMuseqs :: MVar (M.Map String (Time, Museq))
    -- ^ Each `Time` here is the next time that Museq is scheduled to run.
    -- Rarely, briefly, those `Time` values will be in the past.
  , reg :: SynthRegister
  , mTime0 :: MVar Time
  , mPeriod :: MVar Duration
  }


-- | == The GADTs. Hopefully quarantined away from the live coding.

data Msg' sdArgs where
  Msg' :: forall params sdArgs.
         (VarList params
         , Subset (InnerVars params) sdArgs)
      => params -> Msg' sdArgs

data Action' where
  New'  :: MVar (M.Map SynthString (Synth sdArgs))
       -> SynthDef sdArgs
       -> SynthString -> Action'
  Free' :: MVar (M.Map SynthString (Synth sdArgs))
       -> SynthString -> Action'
  Send' :: MVar (M.Map SynthString (Synth sdArgs))
       -> SynthString
       -> Msg' sdArgs -> Action'
