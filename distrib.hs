-- Yet to implement:
--   transient: create a new synth, give it a Msg, let it persist
--     for a while, then free it
--     Give it a random name.
--     Keep that random name in the synth registry, to be completely sure
--     there's never a conflict.
-- For that I will need:
--   the random string function
--   if I'm being absurdly safe, the tempNames field of SynthRegistry

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

import System.Random
import Data.Map as M
import Data.Set as S
import Control.Concurrent.MVar

import Vivid
import Vivid.Jbb.Synths


type SynthName = String

randomString = do
   gen <- newStdGen
   return $ Prelude.take 20 $ randomRs
                              ('!','~') --widest possible on normal keyboard
                              gen


-- | = SynthRegister

data SynthRegister =
  SynthRegister { boops :: M.Map SynthName (Synth BoopParams)
                , sqfms :: M.Map SynthName (Synth SqfmParams)
                , tempNames :: S.Set String }

emptySynthRegister :: SynthRegister
emptySynthRegister = SynthRegister M.empty M.empty S.empty


-- | = Action, and how to act on one

data Action where
  Wait :: Float -> Action
  New  :: MVar (M.Map SynthName (Synth sdArgs))
       -> SynthDef sdArgs
       -> SynthName -> Action
  Free :: MVar (M.Map SynthName (Synth sdArgs))
       -> SynthName -> Action
  Send :: MVar (M.Map SynthName (Synth sdArgs))
       -> SynthName
       -> Msg sdArgs -> Action

act :: Action -> IO ()
  -- todo ? make this a VividAction rather than an IO
    -- problem: you can't read an MVar from a VividAction
act (Wait k) = wait k
-- The rest of this definition of act has to be duplicated for each synthdef.
act (New mSynthMap synthDef name) = do
  synthMap <- readMVar mSynthMap
  synthMap' <- newAction synthDef name synthMap
  swapMVar mSynthMap synthMap'
  return ()
act (Free mSynthMap name) = do
  synthMap <- readMVar mSynthMap
  synthMap' <- freeAction name synthMap
  swapMVar mSynthMap synthMap'
  return ()
act (Send mSynthMap name msg) = do
  synthMap <- readMVar mSynthMap
  sendAction name msg synthMap


-- | = Helper functions, which reduced the above per-synthdef boilerplate.

newAction :: VividAction m
          => SynthDef sdArgs
          -> SynthName
          -> M.Map SynthName (Synth sdArgs)
          -> m (M.Map SynthName (Synth sdArgs))
newAction synthDef name synthMap = 
  case M.lookup name $ synthMap of
    Just _ -> error $ "The name " ++ name ++ " is already in use."
    Nothing -> do s <- synth synthDef ()
                  return $ M.insert name s synthMap

freeAction :: VividAction m
           => SynthName
           -> M.Map SynthName (Synth sdArgs)
           -> m (M.Map SynthName (Synth sdArgs))
freeAction name synthMap = 
  case M.lookup name $ synthMap of
    Nothing -> error $ "The name " ++ name ++ " is already unused."
    Just s -> do free s
                 return $ M.delete name synthMap

sendAction :: forall m sdArgs. VividAction m
           => SynthName
           -> Msg sdArgs
           -> M.Map SynthName (Synth sdArgs)
           -> m ()
sendAction name msg synthMap =
  case M.lookup name synthMap of
    Nothing -> error $ "The name " ++ name ++ " is not in use."
    Just synth -> set' synth msg
