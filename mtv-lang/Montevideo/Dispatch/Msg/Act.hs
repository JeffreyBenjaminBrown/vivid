-- | = Act on a Msg
--
-- `Dispatch.replaceAll` is what uses these.
-- `New`s are sent immediately; schedule time is disregarded.
-- `Send`s are scheduled for the requested time.
-- `Free`s are scheduled so that:
--   At the requested time, the synth is silenced.
--   Half a frame later, the synth is deleted.

module Montevideo.Dispatch.Msg.Act (
    set'    -- ^ VividAction m => Synth params -> Msg' params -> m ()
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


set' :: V.VividAction m => V.Synth params -> Msg' params -> m ()
set' _synth (Msg' m) = V.set _synth m


-- | Unused, so far --
-- Dispatch.Dispatch uses dispatchConsumeScAction_New, dispatchConsumeScAction_Free, dispatchConsumeScAction_Send, but not this.
dispatchConsumeScAction :: SynthRegister -> Time -> ScAction
                       -> IO (SynthRegister -> SynthRegister)
dispatchConsumeScAction reg t a@(Send _ _ _) =
  dispatchConsumeScAction_Send reg t a >> return id
dispatchConsumeScAction reg t a@(Free _ _)   =
  dispatchConsumeScAction_Free reg t a
dispatchConsumeScAction reg _ a@(New _ _)    =
  dispatchConsumeScAction_New  reg   a


-- | `dispatchConsumeScAction_New reg (New Boop name)`,
-- if it finds `name`, logs an error and returns the identity.
-- Otherwise, it creates the synth (immediately -- no scheduling),
--   and returns how to insert it into synth reg.
--   The reg has a separate field for each different kind of synth.
-- In the special case of a sampler, it also looks up the buffer.
--   If found, it uses the buffer as an argument to the sampler synth created.
dispatchConsumeScAction_New
  :: SynthRegister -> ScAction -> IO (SynthRegister -> SynthRegister)
dispatchConsumeScAction_New reg (New Boop name) =
  case M.lookup name $ _boops reg of
    Nothing -> do s <- V.synth boop ()
                  return $ boops %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Boop named " ++ name
            return id

dispatchConsumeScAction_New reg (New (Sampler sample) name) =
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

dispatchConsumeScAction_New reg (New Sqfm name) =
  case M.lookup name $ _sqfms reg of
    Nothing -> do s <- V.synth sqfm ()
                  return $ sqfms %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sqfm named " ++ name
            return id

dispatchConsumeScAction_New reg (New Vap name) =
  case M.lookup name $ _vaps reg of
    Nothing -> do s <- V.synth vap ()
                  return $ vaps %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Vap named " ++ name
            return id

dispatchConsumeScAction_New reg (New Zot name) =
  case M.lookup name $ _zots reg of
    Nothing -> do s <- V.synth zot ()
                  return $ zots %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Zot named " ++ name
            return id

dispatchConsumeScAction_New _ (Send _ _ _) =
  error $ "dispatchConsumeScAction_New received a Send."
dispatchConsumeScAction_New _ (Free _ _)   =
  error $ "dispatchConsumeScAction_New received a Free."


-- | `dispatchConsumeScAction_Free reg when (Free Boop name)`,
-- if it doesn't find `name`, logs an error and returns `id`.
-- If it does, it
--   schedules an amp=0 message for its "when" argument,
--   schedules a free message for shortly thereafter,
--   and returns a way to delete the synth.
dispatchConsumeScAction_Free :: SynthRegister
        -> Rational
        -> ScAction
        -> IO (SynthRegister -> SynthRegister)
dispatchConsumeScAction_Free reg when (Free Boop name) =
  case M.lookup name $ _boops reg of
  Nothing -> do
    writeTimeAndError $ "There is no Boop named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ boops %~ M.delete name

dispatchConsumeScAction_Free reg when (Free (Sampler _) name) =
  case M.lookup name $ _samplers reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sampler named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ samplers %~ M.delete name

dispatchConsumeScAction_Free reg when (Free Sqfm name) =
  case M.lookup name $ _sqfms reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sqfm named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ sqfms %~ M.delete name

dispatchConsumeScAction_Free reg when (Free Vap name) =
  case M.lookup name $ _vaps reg of
  Nothing -> do
    writeTimeAndError $ "There is no Vap named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ vaps %~ M.delete name

dispatchConsumeScAction_Free reg when (Free Zot name) =
  case M.lookup name $ _zots reg of
  Nothing -> do
    writeTimeAndError $ "There is no Zot named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ zots %~ M.delete name

dispatchConsumeScAction_Free _ _ (Send _ _ _) = error "dispatchConsumeScAction_Free received a Send."
dispatchConsumeScAction_Free _ _ (New _ _)    = error "dispatchConsumeScAction_Free received a New."


-- | `dispatchConsumeScAction_Send reg when (Send _ name msg)`,
-- if it doesn't find `name` in `reg`, logs an error.
-- Otherwise, it schedules an action to send everythingg in `msg` at `when`.
-- In the special case of a sampler receiving an "on" message,
--   it schedules an "off" message for shortly thereafter.
dispatchConsumeScAction_Send :: SynthRegister -> Time -> ScAction -> IO ()
dispatchConsumeScAction_Send reg when (Send Boop name msg) =
  case M.lookup name $ _boops reg of
    Nothing ->
      writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth BoopParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ boopMsg msg

dispatchConsumeScAction_Send reg when (Send (Sampler _) name msg) =
  case M.lookup name $ _samplers reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth SamplerParams) -> do
      V.doScheduledAt (V.Timestamp $ fromRational when)
        $ mapM_ (set' s) $ samplerMsg msg
      case M.lookup "trigger" msg of
        Nothing -> return ()
        Just x -> if x <= 0 -- If it's an off message,
          then return ()    -- then do nothing.
          else -- Otherwise schedule an off message for the very near future.
          -- (The lag can be shorter than the sample without truncating it.)
          V.doScheduledAt (V.Timestamp $ fromRational $ when + retriggerLag)
          $ set' s $ Msg' (V.toI (-1 :: Int) :: V.I "trigger")

dispatchConsumeScAction_Send reg when (Send Sqfm name msg) =
  case M.lookup name $ _sqfms reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth SqfmParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ sqfmMsg msg

dispatchConsumeScAction_Send reg when (Send Vap name msg) =
  case M.lookup name $ _vaps reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth VapParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ vapMsg msg

dispatchConsumeScAction_Send reg when (Send Zot name msg) =
  case M.lookup name $ _zots reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth ZotParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ zotMsg msg

dispatchConsumeScAction_Send _ _ (Free _ _) =
  error "dispatchConsumeScAction_Send received a Free."
dispatchConsumeScAction_Send _ _ (New _ _)  =
  error "dispatchConsumeScAction_Send received a New."
