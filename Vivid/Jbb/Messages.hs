{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.Messages where

import Data.List as L
import Data.Map as M

import Vivid
import Vivid.Jbb.Synths


type SynthModel = String

-- | A more recent attempt

data Msg = MsgParamMsg ParamMsg | MsgFree
data ParamMsg = ParamMsg { param :: String
                         , value :: Float }

-- | An earlier attempt, which doesn't allow for targeting a specific
-- instance of a synth

data Msg' =
  Msg' { target :: SynthModel -- one synth model can have many instances
      , param' :: String
      , value' :: Float }

data SynthModelMgr =
  SynthModelMgr { free :: [Synth MyParams]
                , busy :: [Synth MyParams] }

type SynthMgr = M.Map SynthModel SynthModelMgr

_sendMsg :: SynthModelMgr -> Msg' -> IO SynthModelMgr
_sendMsg sm@(SynthModelMgr free busy) (Msg' _ param value) =
  case free of [] -> do putStrLn "can't"
                        return sm
               f:fs -> do set' param value f
                          return $ SynthModelMgr fs $ f:busy
