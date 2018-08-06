{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Distrib.Act (
  act, act'
  ) where

import Data.Map as M
import Control.Concurrent.MVar

import Vivid
import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Distrib.Msg
import Vivid.Jbb.Synths


-- | How to act on an Action:
-- Turn it into an Action', then act' on it.

act :: SynthRegister -> Action -> IO ()
act r a = act' $ toAction' r a

toAction' :: SynthRegister -> Action -> Action'
-- per-synth boilerplate follows
toAction' reg (New Boop synth) = New' (boops reg) boop synth
toAction' reg (Free Boop synth) = Free' (boops reg) synth
toAction' reg (Send Boop synth msg) = Send' (boops reg) synth (boopMsg msg)

toAction' reg (New Vap synth) = New' (vaps reg) vap synth
toAction' reg (Free Vap synth) = Free' (vaps reg) synth
toAction' reg (Send Vap synth msg) = Send' (vaps reg) synth (vapMsg msg)

toAction' reg (New Sqfm synth) = New' (sqfms reg) sqfm synth
toAction' reg (Free Sqfm synth) = Free' (sqfms reg) synth
toAction' reg (Send Sqfm synth msg) = Send' (sqfms reg) synth (sqfmMsg msg)


-- | How to act' on an Action'

act' :: Action' -> IO ()
  -- todo ? make this a VividAction rather than an IO
    -- problem: you can't read an MVar from a VividAction
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

newAction' :: SynthDef sdArgs
           -> SynthString
           -> M.Map SynthString (Synth sdArgs)
           -> IO (M.Map SynthString (Synth sdArgs))
newAction' synthDef name synthMap =
  case M.lookup name $ synthMap of
    Just _ -> do now <- getTime
                 appendFile "errors.txt" $ show now
                   ++ " The name " ++ name ++ " is already in use.\n"
                 return synthMap
    Nothing -> do s <- synth synthDef ()
                  return $ M.insert name s synthMap

freeAction' :: SynthString
            -> M.Map SynthString (Synth sdArgs)
            -> IO (M.Map SynthString (Synth sdArgs))
freeAction' name synthMap =
  case M.lookup name $ synthMap of
    Nothing -> do now <- getTime
                  appendFile "errors.txt" $ show now
                    ++ " The name " ++ name ++ " is already unused.\n"
                  return synthMap
    Just s -> do free s
                 return $ M.delete name synthMap

sendAction' :: forall m sdArgs.
               SynthString
            -> Msg' sdArgs
            -> M.Map SynthString (Synth sdArgs)
            -> IO ()
sendAction' name msg synthMap =
  case M.lookup name synthMap of
    Nothing -> do now <- getTime
                  appendFile "errors.txt" $ show now
                    ++ " The name " ++ name ++ " is not in use.\n"
    Just synth -> set' synth msg
