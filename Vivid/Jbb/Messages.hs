{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.Messages where

import Data.List as L
import Data.Map as M

import Vivid
import Vivid.Jbb.Synths


type SynthModel = String

data Msg =
  Msg { target :: SynthModel -- one synth model can have many instances
      , param :: String
      , value :: Float }

data SynthModelMgr =
  SynthModelMgr { free :: [Node MyParams]
                , busy :: [Node MyParams] }

type SynthMgr = M.Map SynthModel SynthModelMgr

-- >>> TODO
-- sendMsg :: SynthMgr -> Msg -> IO SynthMgr
-- sendMsg mgr msg@(Msg target _ _) =
  -- look up the target in mgr
  -- send msg to that target, using _sendMsg

_sendMsg :: SynthModelMgr -> Msg -> IO SynthModelMgr
_sendMsg sm@(SynthModelMgr free busy) (Msg _ param value) =
  case free of [] -> do putStrLn "can't"
                        return sm
               f:fs -> do set' param value f
                          return $ SynthModelMgr fs $ f:busy
