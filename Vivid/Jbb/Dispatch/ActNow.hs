{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Dispatch.ActNow (
  museqSynths
  , museqsDiff
  , act
  , act'
  , writeTimeAndError
  ) where

import Control.Concurrent.MVar
import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Dispatch.Types
import Vivid.Jbb.Dispatch.Msg
import Vivid.Jbb.Synths
import Vivid.Jbb.Util (unique)


-- | Given a Museq, find the synths it uses.
museqSynths :: Museq Action -> [(SynthDefEnum, SynthName)]
museqSynths = map (actionSynth . snd) . V.toList . _vec

-- | Given an old set of Museqs and a new one, figure out
-- which synths need to be created, and which destroyed.
museqsDiff :: M.Map MuseqName (Museq Action)
           -> M.Map MuseqName (Museq Action)
           -> ([(SynthDefEnum, SynthName)],
               [(SynthDefEnum, SynthName)])
museqsDiff old new = (toFree,toCreate) where
  oldMuseqs = M.elems old :: [Museq Action]
  newMuseqs = M.elems new :: [Museq Action]
  oldSynths = unique $ concatMap museqSynths oldMuseqs
  newSynths = unique $ concatMap museqSynths newMuseqs
  toCreate = newSynths \\ oldSynths
  toFree = oldSynths \\ newSynths

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
           -> SynthName
           -> M.Map SynthName (Synth sdArgs)
           -> IO (M.Map SynthName (Synth sdArgs))
newAction' synthDef name synthMap =
  case M.lookup name synthMap of
    Just _ -> do writeTimeAndError
                   $ "The name " ++ name ++ " is already in use.\n"
                 return synthMap
    Nothing -> do s <- synth synthDef ()
                  return $ M.insert name s synthMap

freeAction' :: SynthName
            -> M.Map SynthName (Synth sdArgs)
            -> IO (M.Map SynthName (Synth sdArgs))
freeAction' name synthMap =
  case M.lookup name synthMap of
    Nothing -> do writeTimeAndError
                    $ "The name " ++ name ++ " is already unused.\n"
                  return synthMap
    Just s -> do free s
                 return $ M.delete name synthMap

sendAction' :: forall m sdArgs.
               SynthName
            -> Msg' sdArgs
            -> M.Map SynthName (Synth sdArgs)
            -> IO ()
sendAction' name msg synthMap =
  case M.lookup name synthMap of
    Nothing -> writeTimeAndError
      $ " The name " ++ name ++ " is not in use.\n"
    Just synth -> set' synth msg

writeTimeAndError :: String -> IO ()
writeTimeAndError msg = do now <- getTime
                           appendFile "errors.txt"
                             $ show now ++ ": " ++ msg
