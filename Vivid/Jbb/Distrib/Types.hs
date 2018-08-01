{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Distrib.Types where

import Control.Concurrent.MVar
import Data.Map as M

import Vivid
import Vivid.Jbb.Synths


type SynthString = String
type ParamString = String

type Msg = (ParamString,Float)

data Msg' sdArgs where
  Msg' :: forall params sdArgs.
         (VarList params
         , Subset (InnerVars params) sdArgs)
      => params -> Msg' sdArgs

set' :: VividAction m => Synth params -> Msg' params -> m ()
set' synth (Msg' m) = set synth m

data SynthRegister = -- per-synth boilerplate
  SynthRegister { boops :: MVar (M.Map SynthString (Synth BoopParams))
                , vaps  :: MVar (M.Map SynthString (Synth VapParams))
                , sqfms :: MVar (M.Map SynthString (Synth SqfmParams))
                , zots :: MVar (M.Map SynthString (Synth ZotParams))
                }

emptySynthRegister :: IO SynthRegister
emptySynthRegister = do x <- newMVar M.empty
                        y <- newMVar M.empty
                        z <- newMVar M.empty
                        w <- newMVar M.empty
                        return $ SynthRegister x y z w  

data Action = Wait Float
            | New  SynthDefEnum SynthString        
            | Free SynthDefEnum SynthString        
            | Send SynthDefEnum SynthString Msg 

data Action' where
  Wait' :: Float -> Action'
  New'  :: MVar (M.Map SynthString (Synth sdArgs))
       -> SynthDef sdArgs
       -> SynthString -> Action'
  Free' :: MVar (M.Map SynthString (Synth sdArgs))
       -> SynthString -> Action'
  Send' :: MVar (M.Map SynthString (Synth sdArgs))
       -> SynthString
       -> Msg' sdArgs -> Action'
