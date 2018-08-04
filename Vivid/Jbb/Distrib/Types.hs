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
-- | A few types are also defined in Jbb.Synths

import Vivid.Jbb.Synths


-- | = The easy types

type SynthString = String
type ParamString = String
type Duration = Rational
type Time = Rational

unTimestamp :: Timestamp -> Double
unTimestamp (Timestamp x) = x

type Msg = (ParamString,Float)

data Action = New  SynthDefEnum SynthString
            | Free SynthDefEnum SynthString
            | Send SynthDefEnum SynthString Msg
  deriving (Show,Eq,Ord)

data Museq = Museq { _dur :: Duration
                   , _vec :: Vector (Time, Action) }
  deriving (Show,Eq)

makeLenses ''Museq

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


-- | = The GADTs. Hopefully quarantined away from the live coding.

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
