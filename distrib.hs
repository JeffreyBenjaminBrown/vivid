-- Goal: distribute messages across synths
-- Pieces
--   keep a Map String Synth for each active SynthDef
--   kinds of actions
--     transient: create a new synth, give it a Msg, let it persist
--       for a while, then free it
--     to: give a Msg to a named synth
--     new: create a new synth, give it a name
--     free: free a named synth
--     wait: wait for a number of seconds

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

import Data.Map as M
import Data.Set as S
import Vivid
import Vivid.Jbb.Synths


type SynthName = String

-- | = SynthRegister

data SynthRegister =
  SynthRegister { boops :: M.Map SynthName (Synth BoopParams)
                , sqfms :: M.Map SynthName (Synth SqfmParams)
                , tempNames :: S.Set String }

emptySynthRegister :: SynthRegister
emptySynthRegister = SynthRegister M.empty M.empty S.empty


-- | = Action, and how to act on one

data Action sdArgs where
  Wait :: Float                                   -> Action sdARgs
  Free :: SynthDefName -> SynthName               -> Action sdARgs
  New  :: SynthDefName -> SynthName               -> Action sdARgs
  Send :: SynthDefName -> SynthName -> Msg sdARgs -> Action sdARgs

act :: VividAction m => SynthRegister -> Action sdArgs -> m SynthRegister
act synths (Wait k) = do wait k
                         return synths
act synths (New Boop name) = do 
  newBoops <- newAction boop name $ boops synths
  return $ synths {boops = newBoops}
act synths (Free Boop name) = do 
  newBoops <- freeAction name $ boops synths
  return $ synths {boops = newBoops}


-- | = Helper functions to reduce the boilerplate above

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
