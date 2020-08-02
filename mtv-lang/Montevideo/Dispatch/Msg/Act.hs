-- | = Act on a ScMsg
--
-- `Dispatch.replaceAll` is what uses these.
-- `New`s are sent immediately; schedule time is disregarded.
-- `Send`s are scheduled for the requested time.
-- `Free`s are scheduled so that:
--   At the requested time, the synth is silenced.
--   Half a frame later, the synth is deleted.

module Montevideo.Dispatch.Msg.Act (
    set'    -- ^ VividAction m => Synth params -> ScMsg' params -> m ()
  , dispatchConsumeScAction
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
import Montevideo.Dispatch.Msg.Mk
import Montevideo.Dispatch.Types
import Montevideo.Synth
import Montevideo.Util


set' :: V.VividAction m => V.Synth params -> ScMsg' params -> m ()
set' _synth (ScMsg' m) = V.set _synth m


-- | Unused, so far --
-- Dispatch.Dispatch uses dispatchConsumeScAction_New, dispatchConsumeScAction_Free, dispatchConsumeScAction_Send, but not this.
dispatchConsumeScAction :: SynthRegister -> Time -> ScAction SynthName
                        -> IO (SynthRegister -> SynthRegister)
dispatchConsumeScAction reg t a@(ScAction_Send _ _ _) =
  dispatchConsumeScAction_Send reg t a >> return id
dispatchConsumeScAction reg t a@(ScAction_Free _ _)   =
  dispatchConsumeScAction_Free reg t a
dispatchConsumeScAction reg _ a@(ScAction_New _ _)    =
  dispatchConsumeScAction_New  reg   a


-- | `dispatchConsumeScAction_New reg (ScAction_New Boop name)`,
-- if it finds `name`, logs an error and returns the identity.
-- Otherwise, it creates the synth (immediately -- no scheduling),
--   and returns how to insert it into synth reg.
--   The reg has a separate field for each different kind of synth.
-- In the special case of a sampler, it also looks up the buffer.
--   If found, it uses the buffer as an argument to the sampler synth created.
dispatchConsumeScAction_New ::
  SynthRegister -> ScAction SynthName ->
  IO (SynthRegister -> SynthRegister)
dispatchConsumeScAction_New reg (ScAction_New Boop name) =
  case M.lookup name $ _boops reg of
    Nothing -> do s <- V.synth boop ()
                  return $ boops %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Boop named " ++ name
            return id

dispatchConsumeScAction_New reg (ScAction_New (Sampler sample) name) =
  case M.lookup name $ _samplers reg of
    Nothing -> do
      case M.lookup sample $ reg ^. samples of
        Nothing -> do writeTimeAndError $
                        "dispatchConsumeScAction_New: Sample " ++ show sample ++ " not found."
                      return id
        Just buf -> do
          s <- V.synth sampler (V.b2i buf :: V.I "buffer")
          return $ samplers %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sampler named " ++ name
            return id

dispatchConsumeScAction_New reg (ScAction_New Sqfm name) =
  case M.lookup name $ _sqfms reg of
    Nothing -> do s <- V.synth sqfm ()
                  return $ sqfms %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sqfm named " ++ name
            return id

dispatchConsumeScAction_New reg (ScAction_New Vap name) =
  case M.lookup name $ _vaps reg of
    Nothing -> do s <- V.synth vap ()
                  return $ vaps %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Vap named " ++ name
            return id

dispatchConsumeScAction_New reg (ScAction_New Zot name) =
  case M.lookup name $ _zots reg of
    Nothing -> do s <- V.synth zot ()
                  return $ zots %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Zot named " ++ name
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
  -> Rational
  -> ScAction SynthName
  -> IO (SynthRegister -> SynthRegister)
dispatchConsumeScAction_Free reg when (ScAction_Free Boop name) =
  case M.lookup name $ _boops reg of
  Nothing -> do
    writeTimeAndError $ "There is no Boop named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ ScMsg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ boops %~ M.delete name

dispatchConsumeScAction_Free reg when (ScAction_Free (Sampler _) name) =
  case M.lookup name $ _samplers reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sampler named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ ScMsg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ samplers %~ M.delete name

dispatchConsumeScAction_Free reg when (ScAction_Free Sqfm name) =
  case M.lookup name $ _sqfms reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sqfm named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ ScMsg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ sqfms %~ M.delete name

dispatchConsumeScAction_Free reg when (ScAction_Free Vap name) =
  case M.lookup name $ _vaps reg of
  Nothing -> do
    writeTimeAndError $ "There is no Vap named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ ScMsg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ vaps %~ M.delete name

dispatchConsumeScAction_Free reg when (ScAction_Free Zot name) =
  case M.lookup name $ _zots reg of
  Nothing -> do
    writeTimeAndError $ "There is no Zot named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ ScMsg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ zots %~ M.delete name

dispatchConsumeScAction_Free _ _ (ScAction_Send _ _ _) =
  error "dispatchConsumeScAction_Free received a ScAction_Send."
dispatchConsumeScAction_Free _ _ (ScAction_New _ _) =
  error "dispatchConsumeScAction_Free received a ScAction_New."


-- | `dispatchConsumeScAction_Send reg when (ScAction_Send _ name msg)`,
-- if it doesn't find `name` in `reg`, logs an error.
-- Otherwise, it schedules an action to send everythingg in `msg` at `when`.
-- In the special case of a sampler receiving an "on" message,
--   it schedules an "off" message for shortly thereafter.
dispatchConsumeScAction_Send ::
  SynthRegister -> Time -> ScAction SynthName -> IO ()
dispatchConsumeScAction_Send reg when (ScAction_Send Boop name msg) =
  case M.lookup name $ _boops reg of
    Nothing ->
      writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth BoopParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ boopScMsg msg

dispatchConsumeScAction_Send reg when (ScAction_Send (Sampler _) name msg) =
  case M.lookup name $ _samplers reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth SamplerParams) -> do
      V.doScheduledAt (V.Timestamp $ fromRational when)
        $ mapM_ (set' s) $ samplerScMsg msg
      case M.lookup "trigger" msg of
        Nothing -> return ()
        Just x -> if x <= 0 -- If it's an off message,
          then return ()    -- then do nothing.
          else -- Otherwise schedule an off message for the very near future.
          -- (The lag can be shorter than the sample without truncating it.)
          V.doScheduledAt (V.Timestamp $ fromRational $ when + retriggerLag)
          $ set' s $ ScMsg' (V.toI (-1 :: Int) :: V.I "trigger")

dispatchConsumeScAction_Send reg when (ScAction_Send Sqfm name msg) =
  case M.lookup name $ _sqfms reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth SqfmParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ sqfmScMsg msg

dispatchConsumeScAction_Send reg when (ScAction_Send Vap name msg) =
  case M.lookup name $ _vaps reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth VapParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ vapScMsg msg

dispatchConsumeScAction_Send reg when (ScAction_Send Zot name msg) =
  case M.lookup name $ _zots reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth ZotParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ zotScMsg msg

dispatchConsumeScAction_Send _ _ (ScAction_Free _ _) =
  error "dispatchConsumeScAction_Send received a ScAction_Free."
dispatchConsumeScAction_Send _ _ (ScAction_New _ _)  =
  error "dispatchConsumeScAction_Send received a ScAction_New."
