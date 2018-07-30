{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Distrib.Types where

import Control.Concurrent.MVar
import Data.Map as M

import Vivid
import Vivid.Jbb.Synths


type SynthName = String

type Msg = (String,Float)

data Msg' sdArgs where
  Msg' :: forall params sdArgs.
         (VarList params
         , Subset (InnerVars params) sdArgs)
      => params -> Msg' sdArgs

set' :: VividAction m => Synth params -> Msg' params -> m ()
set' synth (Msg' m) = set synth m

data SynthRegister = -- per-synth boilerplate
  SynthRegister { boops :: MVar (M.Map SynthName (Synth BoopParams))
                , vaps  :: MVar (M.Map SynthName (Synth VapParams))
                , sqfms :: MVar (M.Map SynthName (Synth SqfmParams))
                , zots :: MVar (M.Map SynthName (Synth ZotParams))
                }

emptySynthRegister :: IO SynthRegister
emptySynthRegister = do x <- newMVar M.empty
                        y <- newMVar M.empty
                        z <- newMVar M.empty
                        w <- newMVar M.empty
                        return $ SynthRegister x y z w  

data Action = Wait Float
            | New  SynthDefName SynthName        
            | Free SynthDefName SynthName        
            | Send SynthDefName SynthName Msg 

data Action' where
  Wait' :: Float -> Action'
  New'  :: MVar (M.Map SynthName (Synth sdArgs))
       -> SynthDef sdArgs
       -> SynthName -> Action'
  Free' :: MVar (M.Map SynthName (Synth sdArgs))
       -> SynthName -> Action'
  Send' :: MVar (M.Map SynthName (Synth sdArgs))
       -> SynthName
       -> Msg' sdArgs -> Action'
