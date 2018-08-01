{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Distrib.Act (
  act'
  ) where

import Data.Map as M
import Control.Concurrent.MVar

import Vivid
import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Synths


act' :: Action' -> IO ()
  -- todo ? make this a VividAction rather than an IO
    -- problem: you can't read an MVar from a VividAction
act' (Wait' k) = wait k
act' (New' mSynthMap synthDef name) = do
  synthMap <- readMVar mSynthMap
  synthMap' <- newAction' synthDef name synthMap
  swapMVar mSynthMap synthMap'
  return ()
act' (Free' mSynthMap name) = do
  synthMap <- readMVar mSynthMap
  synthMap' <- freeAction' name synthMap
  swapMVar mSynthMap synthMap'
  return ()
act' (Send' mSynthMap name msg) = do
  synthMap <- readMVar mSynthMap
  sendAction' name msg synthMap

newAction' :: VividAction m
          => SynthDef sdArgs
          -> SynthString
          -> M.Map SynthString (Synth sdArgs)
          -> m (M.Map SynthString (Synth sdArgs))
newAction' synthDef name synthMap =
  case M.lookup name $ synthMap of
    Just _ -> error $ "The name " ++ name ++ " is already in use."
    Nothing -> do s <- synth synthDef ()
                  return $ M.insert name s synthMap

freeAction' :: VividAction m
           => SynthString
           -> M.Map SynthString (Synth sdArgs)
           -> m (M.Map SynthString (Synth sdArgs))
freeAction' name synthMap =
  case M.lookup name $ synthMap of
    Nothing -> error $ "The name " ++ name ++ " is already unused."
    Just s -> do free s
                 return $ M.delete name synthMap

sendAction' :: forall m sdArgs. VividAction m
           => SynthString
           -> Msg' sdArgs
           -> M.Map SynthString (Synth sdArgs)
           -> m ()
sendAction' name msg synthMap =
  case M.lookup name synthMap of
    Nothing -> error $ "The name " ++ name ++ " is not in use."
    Just synth -> set' synth msg
