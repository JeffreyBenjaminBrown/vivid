-- | = Act on a ScParams
--
-- `Dispatch.replaceAll` is what uses these.
-- `New`s are sent immediately; schedule time is disregarded.
-- `Send`s are scheduled for the requested time.
-- `Free`s are scheduled so that:
--   At the requested time, the synth is silenced.
--   Half a frame later, the synth is deleted.

{-# LANGUAGE DataKinds #-}

module Montevideo.Dispatch.Msg.Act (
    dispatchConsumeScAction
    -- ^ SynthRegister -> Time     -> ScAction -> IO (SynthRegister -> SynthRegister)
  , dispatchConsumeScAction_New
    -- ^ SynthRegister             -> ScAction -> IO (SynthRegister -> SynthRegister)
  , dispatchConsumeScAction_Free
    -- ^ SynthRegister -> Rational -> ScAction -> IO (SynthRegister -> SynthRegister)
  , dispatchConsumeScAction_Send
    -- ^ SynthRegister -> Time     -> ScAction -> IO ()
  ) where

import           Control.Lens hiding (set,set')
import qualified Data.Map as M
import qualified Vivid as V

import Montevideo.Dispatch.Config
import Montevideo.Synth.Msg
import Montevideo.Dispatch.Types
import Montevideo.Synth
import Montevideo.Util


-- | Unused, so far --
-- Dispatch.Dispatch uses dispatchConsumeScAction_New, dispatchConsumeScAction_Free, dispatchConsumeScAction_Send, but not this.
dispatchConsumeScAction :: SynthRegister -> Time -> ScAction SynthName
                        -> IO (SynthRegister -> SynthRegister)
dispatchConsumeScAction reg t a@(ScAction_Send _ _ _) =
  dispatchConsumeScAction_Send reg t a >> return id
dispatchConsumeScAction reg t a@(ScAction_Free _ _)   =
  dispatchConsumeScAction_Free reg t a
dispatchConsumeScAction reg _ a@(ScAction_New _ _ _)  =
  dispatchConsumeScAction_New  reg   a

-- | PITFALL: Sets no parameters.
-- In this sense it does not consume everything that might be present.
-- That's because sequences in mtv-lang create voices well before they sound,
-- and therefore the `_actionScParams` field of the input
-- `ScAction` is always the empty map when they are first created.
--
-- `dispatchConsumeScAction_New reg (ScAction_New Boop name)`,
-- if it finds `name`, logs an error and returns the identity.
-- Otherwise, it creates the synth (immediately -- no scheduling),
--   and returns how to insert it into the synth register.
--   The reg has a separate field for each different kind of synth.
-- In the special case of a sampler, it also looks up the buffer.
--   If found, it uses the buffer as an argument to the sampler synth created.

dispatchConsumeScAction_New ::
  SynthRegister -> ScAction SynthName ->
  IO (SynthRegister -> SynthRegister)

dispatchConsumeScAction_New reg (ScAction_New Boop name _) =
  case M.lookup name $ _boops reg of
    Nothing -> do s <- V.synth boop ()
                  return $ boops %~ M.insert name s
    _ -> do writeTimeAndError $
              "There is already a Boop named " ++ name ++ "\n"
            return id

dispatchConsumeScAction_New reg (ScAction_New (Sampler sample) name _) =
  case M.lookup name $ _samplers reg of
    Nothing -> do
      case M.lookup sample $ reg ^. samples of
        Nothing -> do
          writeTimeAndError $ "dispatchConsumeScAction_New: Sample "
            ++ show sample ++ " not found."
          return id
        Just buf -> do
          s <- V.synth sampler (V.b2i buf :: V.I "buffer")
          return $ samplers %~ M.insert name s
    _ -> do writeTimeAndError $
              "There is already a Sampler named " ++ name ++ "\n"
            return id

dispatchConsumeScAction_New reg (ScAction_New Sqfm name _) =
  case M.lookup name $ _sqfms reg of
    Nothing -> do s <- V.synth sqfm ()
                  return $ sqfms %~ M.insert name s
    _ -> do writeTimeAndError $
              "There is already a Sqfm named " ++ name ++ "\n"
            return id

dispatchConsumeScAction_New reg (ScAction_New Vap name _) =
  case M.lookup name $ _vaps reg of
    Nothing -> do s <- V.synth vap ()
                  return $ vaps %~ M.insert name s
    _ -> do writeTimeAndError $
              "There is already a Vap named " ++ name ++ "\n"
            return id

dispatchConsumeScAction_New reg (ScAction_New Zot name _) =
  case M.lookup name $ _zots reg of
    Nothing -> do s <- V.synth zot ()
                  return $ zots %~ M.insert name s
    _ -> do writeTimeAndError $
              "There is already a Zot named " ++ name ++ "\n"
            return id

dispatchConsumeScAction_New _ (ScAction_Send _ _ _) =
  error $ "dispatchConsumeScAction_New received a ScAction_Send."
dispatchConsumeScAction_New _ (ScAction_Free _ _)   =
  error $ "dispatchConsumeScAction_New received a ScAction_Free."


-- | `dispatchConsumeScAction_Free reg when (ScAction_Free Boop name)`,
-- if it doesn't find `name`, logs an error and returns `id`.
-- If it does, it
--   schedules an amp=0 message for its "when" argument,
--   schedules a free message for shortly thereafter,
--   and returns a way to delete the synth.

dispatchConsumeScAction_Free
  :: SynthRegister
  -> Time
  -> ScAction SynthName
  -> IO (SynthRegister -> SynthRegister)

dispatchConsumeScAction_Free reg when (ScAction_Free Boop name) =
  case M.lookup name $ _boops reg of
  Nothing -> do
    writeTimeAndError $ "There is no Boop named " ++ name ++ " to free.\n"
    return id
  Just s -> do
    V.doScheduledAt (timestamp when)
      $ set' s $ ScParams' (0 :: V.I "amp")
    V.doScheduledAt (timestamp $ when + frameDuration / 2)
      $ V.free s
    return $ boops %~ M.delete name

dispatchConsumeScAction_Free reg when (ScAction_Free (Sampler _) name) =
  case M.lookup name $ _samplers reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sampler named " ++ name ++ " to free.\n"
    return id
  Just s -> do
    V.doScheduledAt (timestamp when)
      $ set' s $ ScParams' (0 :: V.I "amp")
    V.doScheduledAt (timestamp $ when + frameDuration / 2)
      $ V.free s
    return $ samplers %~ M.delete name

dispatchConsumeScAction_Free reg when (ScAction_Free Sqfm name) =
  case M.lookup name $ _sqfms reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sqfm named " ++ name ++ " to free.\n"
    return id
  Just s -> do
    V.doScheduledAt (timestamp when)
      $ set' s $ ScParams' (0 :: V.I "amp")
    V.doScheduledAt (timestamp $ when + frameDuration / 2)
      $ V.free s
    return $ sqfms %~ M.delete name

dispatchConsumeScAction_Free reg when (ScAction_Free Vap name) =
  case M.lookup name $ _vaps reg of
  Nothing -> do
    writeTimeAndError $ "There is no Vap named " ++ name ++ " to free.\n"
    return id
  Just s -> do
    V.doScheduledAt (timestamp when)
      $ set' s $ ScParams' (0 :: V.I "amp")
    V.doScheduledAt ( timestamp $ when + frameDuration / 2)
      $ V.free s
    return $ vaps %~ M.delete name

dispatchConsumeScAction_Free reg when (ScAction_Free Zot name) =
  case M.lookup name $ _zots reg of
  Nothing -> do
    writeTimeAndError $ "There is no Zot named " ++ name ++ " to free.\n"
    return id
  Just s -> do
    V.doScheduledAt (timestamp when)
      $ set' s $ ScParams' (0 :: V.I "amp")
    V.doScheduledAt (timestamp $ when + frameDuration / 2)
      $ V.free s
    return $ zots %~ M.delete name

dispatchConsumeScAction_Free _ _ (ScAction_Send _ _ _) =
  error "dispatchConsumeScAction_Free received a ScAction_Send."
dispatchConsumeScAction_Free _ _ (ScAction_New _ _ _) =
  error "dispatchConsumeScAction_Free received a ScAction_New."


-- | `dispatchConsumeScAction_Send reg when (ScAction_Send _ name msg)`,
-- if it doesn't find `name` in `reg`, logs an error.
-- Otherwise, it schedules an action to send everythingg in `msg` at `when`.
-- In the special case of a sampler receiving a "trigger=1" message,
--   it schedules a "trigger=0" message for shortly thereafter.
dispatchConsumeScAction_Send ::
  SynthRegister -> Time -> ScAction SynthName -> IO ()
dispatchConsumeScAction_Send reg when (ScAction_Send Boop name msg) =
  case M.lookup name $ _boops reg of
    Nothing ->
      writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth BoopParams) ->
      V.doScheduledAt (timestamp when)
      $ mapM_ (set' s) $ boopScParams msg

dispatchConsumeScAction_Send reg when (ScAction_Send (Sampler _) name msg) =
  case M.lookup name $ _samplers reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth SamplerParams) -> do
      V.doScheduledAt (timestamp when)
        $ mapM_ (set' s) $ samplerScParams msg
      case M.lookup "trigger" msg of
        Nothing -> return ()
        Just x -> if x <= 0 -- If it's an off message,
          then return ()    -- then do nothing.
          else -- Otherwise schedule an off message for the very near future.
          -- (The lag can be shorter than the sample without truncating it.)
          V.doScheduledAt (timestamp $ when + retriggerLag)
          $ set' s $ ScParams' (V.toI (-1 :: Int) :: V.I "trigger")

dispatchConsumeScAction_Send reg when (ScAction_Send Sqfm name msg) =
  case M.lookup name $ _sqfms reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth SqfmParams) ->
      V.doScheduledAt (timestamp when)
      $ mapM_ (set' s) $ sqfmScParams msg

dispatchConsumeScAction_Send reg when (ScAction_Send Vap name msg) =
  case M.lookup name $ _vaps reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth VapParams) ->
      V.doScheduledAt (timestamp when)
      $ mapM_ (set' s) $ vapScParams msg

dispatchConsumeScAction_Send reg when (ScAction_Send Zot name msg) =
  case M.lookup name $ _zots reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth ZotParams) ->
      V.doScheduledAt (timestamp when)
      $ mapM_ (set' s) $ zotScParams msg

dispatchConsumeScAction_Send _ _ (ScAction_Free _ _) =
  error "dispatchConsumeScAction_Send received a ScAction_Free."
dispatchConsumeScAction_Send _ _ (ScAction_New _ _ _)  =
  error "dispatchConsumeScAction_Send received a ScAction_New."
