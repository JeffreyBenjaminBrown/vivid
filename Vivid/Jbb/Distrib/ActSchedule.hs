{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

module Vivid.Jbb.Distrib.ActSchedule where

import Control.Concurrent.MVar
import Data.List ((\\),partition)
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Distrib.Msg
import Vivid.Jbb.Synths
import Vivid.Jbb.Util (unique)

import Vivid.Jbb.Distrib.ActNow


-- | If you'll need some synths in the future, might as well make them now.
-- Therefore this does no scheduling. It creates a bunch of synths (of the
-- same type) at once, because I suspect that will be faster.
newActions :: forall sdArgs.
              SynthDef sdArgs
           -> [SynthName]
           -> M.Map SynthName (Synth sdArgs)
           -> IO (M.Map SynthName (Synth sdArgs))
newActions sd names synthMap = do
  let (found, notFound) =
        partition (isJust . flip M.lookup synthMap) names
  forM_ found (\name -> writeTimeAndError $ "A synth named "
                ++ show name ++ " already exists.")
  synths <- forM notFound $ const $ synth sd ()
  let nameSynths = zip notFound synths :: [(SynthName, Synth sdArgs)]
  return $ foldl (\m (name,s) -> M.insert name s m)
           synthMap nameSynths

freeAction'At :: SynthName
              -> M.Map SynthName (Synth sdArgs)
              -> Time
              -> IO (M.Map SynthName (Synth sdArgs))
freeAction'At name synthMap when =
  case M.lookup name synthMap of
    Nothing -> do writeTimeAndError
                    $ "The name " ++ name ++ " is already unused.\n"
                  return synthMap
    Just s -> do doScheduledAt (Timestamp when) $ free s
                 return $ M.delete name synthMap

sendAction'At :: forall m sdArgs.
                 SynthName
              -> Msg' sdArgs
              -> M.Map SynthName (Synth sdArgs)
              -> Time
              -> IO ()
sendAction'At name msg synthMap when =
  case M.lookup name synthMap of
    Nothing -> writeTimeAndError
      $ " The name " ++ name ++ " is not in use.\n"
    Just synth -> doScheduledAt (Timestamp when)
      $ set' synth msg
