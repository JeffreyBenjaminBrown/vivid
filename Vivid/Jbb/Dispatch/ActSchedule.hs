{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs
#-}

module Vivid.Jbb.Dispatch.ActSchedule where

import Control.Concurrent.MVar
import Data.List ((\\),partition)
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Dispatch.Config (frameDuration)
import Vivid.Jbb.Dispatch.Types
import Vivid.Jbb.Dispatch.Msg
import Vivid.Jbb.Synths
import Vivid.Jbb.Util (unique)

import Vivid.Jbb.Dispatch.ActNow


type SynthMap sdArgs = M.Map SynthName (Synth sdArgs)


-- act'At :: forall sdArgs. Action' -> IO (SynthMap sdArgs -> SynthMap sdArgs)
-- act'At (New' mvSMap (sd :: SynthDef sdArgs) name) = do
--   sMap <- readMVar mvSMap
--   newAction'At sd name sMap

-- | If you'll need some synths in the future, might as well make them now.
-- Therefore this does no scheduling
newAction'At :: forall sdArgs.
                SynthDef sdArgs
             -> SynthName
             -> SynthMap sdArgs
             -> IO (SynthMap sdArgs -> SynthMap sdArgs)
newAction'At sd name synthMap = case M.lookup name synthMap of
  Just _ -> do writeTimeAndError $ "A synth named "
                 ++ show name ++ " already exists."
               return id
  Nothing -> do s <- synth sd ()
                return $ M.insert name s

-- | This version creates a bunch of synths (of the
-- same type) at once, because I suspect that will be faster.
newAction'sAt :: forall sdArgs. -- TODO : use
              SynthDef sdArgs
           -> [SynthName]
           -> SynthMap sdArgs
           -> IO (SynthMap sdArgs)
newAction'sAt sd names synthMap = do
  let (found, notFound) =
        partition (isJust . flip M.lookup synthMap) names
  forM_ found (\name -> writeTimeAndError $ "A synth named "
                ++ show name ++ " already exists.")
  synths <- forM notFound $ const $ synth sd ()
  let nameSynths = zip notFound synths :: [(SynthName, Synth sdArgs)]
  return $ foldl (\m (name,s) -> M.insert name s m)
           synthMap nameSynths

-- | Rather than return (now) a modified SynthMap, this returns
-- a function to apply later, when it's safe to delete the Synth.
freeAction'At :: Elem "amp" sdArgs
              => SynthName
              -> SynthMap sdArgs
              -> Time
              -> IO ( SynthMap sdArgs -> SynthMap sdArgs )
freeAction'At name synthMap when =
  case M.lookup name synthMap of
    Nothing -> do writeTimeAndError
                    $ "The name " ++ name ++ " is already unused.\n"
                  return id
    Just s -> do doScheduledAt (Timestamp when)
                   $ set s (0::I"amp")
                 doScheduledAt (Timestamp $ when + frameDuration / 2)
                   $ free s
                 return $ M.delete name

sendAction'At :: forall m sdArgs.
                 SynthName
              -> Msg' sdArgs
              -> SynthMap sdArgs
              -> Time
              -> IO ()
sendAction'At name msg synthMap when =
  case M.lookup name synthMap of
    Nothing -> writeTimeAndError
      $ " The name " ++ name ++ " is not in use.\n"
    Just synth -> doScheduledAt (Timestamp when)
      $ set' synth msg
