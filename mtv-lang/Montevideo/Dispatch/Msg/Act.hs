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
  , dispatchConsumeAction
    -- ^ SynthRegister -> Time     -> Action -> IO (SynthRegister -> SynthRegister)
  , dispatchConsumeAction_New
    -- ^ SynthRegister             -> Action -> IO (SynthRegister -> SynthRegister)
  , dispatchConsumeAction_Free
    -- ^ SynthRegister -> Rational -> Action -> IO (SynthRegister -> SynthRegister)
  , dispatchConsumeAction_Send
    -- ^ SynthRegister -> Time     -> Action -> IO ()
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
-- Dispatch.Dispatch uses dispatchConsumeAction_New, dispatchConsumeAction_Free, dispatchConsumeAction_Send, but not this.
dispatchConsumeAction :: SynthRegister -> Time -> Action
                       -> IO (SynthRegister -> SynthRegister)
dispatchConsumeAction reg t a@(Send _ _ _) =
  dispatchConsumeAction_Send reg t a >> return id
dispatchConsumeAction reg t a@(Free _ _)   =
  dispatchConsumeAction_Free reg t a
dispatchConsumeAction reg _ a@(New _ _)    =
  dispatchConsumeAction_New  reg   a


-- | `dispatchConsumeAction_New reg (New Boop name)`,
-- if it finds `name`, logs an error and returns the identity.
-- Otherwise, it creates the synth (immediately -- no scheduling),
--   and returns how to insert it into synth reg.
--   The reg has a separate field for each different kind of synth.
-- In the special case of a sampler, it also looks up the buffer.
--   If found, it uses the buffer as an argument to the sampler synth created.
dispatchConsumeAction_New
  :: SynthRegister -> Action -> IO (SynthRegister -> SynthRegister)
dispatchConsumeAction_New reg (New Boop name) =
  case M.lookup name $ _boops reg of
    Nothing -> do s <- V.synth boop ()
                  return $ boops %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Boop named " ++ name
            return id

dispatchConsumeAction_New reg (New (Sampler sample) name) =
  case M.lookup name $ _samplers reg of
    Nothing -> do
      case M.lookup sample $ reg ^. samples of
        Nothing -> do writeTimeAndError $
                        "dispatchConsumeAction_New: Sample " ++ show sample ++ " not found."
                      return id
        Just buf -> do
          s <- V.synth sampler (V.b2i buf :: V.I "buffer")
          return $ samplers %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sampler named " ++ name
            return id

dispatchConsumeAction_New reg (New Sqfm name) =
  case M.lookup name $ _sqfms reg of
    Nothing -> do s <- V.synth sqfm ()
                  return $ sqfms %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sqfm named " ++ name
            return id

dispatchConsumeAction_New reg (New Vap name) =
  case M.lookup name $ _vaps reg of
    Nothing -> do s <- V.synth vap ()
                  return $ vaps %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Vap named " ++ name
            return id

dispatchConsumeAction_New reg (New Zot name) =
  case M.lookup name $ _zots reg of
    Nothing -> do s <- V.synth zot ()
                  return $ zots %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Zot named " ++ name
            return id

dispatchConsumeAction_New _ (Send _ _ _) =
  error $ "dispatchConsumeAction_New received a Send."
dispatchConsumeAction_New _ (Free _ _)   =
  error $ "dispatchConsumeAction_New received a Free."


-- | `dispatchConsumeAction_Free reg when (Free Boop name)`,
-- if it doesn't find `name`, logs an error and returns `id`.
-- If it does, it
--   schedules an amp=0 message for its "when" argument,
--   schedules a free message for shortly thereafter,
--   and returns a way to delete the synth.
dispatchConsumeAction_Free :: SynthRegister
        -> Rational
        -> Action
        -> IO (SynthRegister -> SynthRegister)
dispatchConsumeAction_Free reg when (Free Boop name) =
  case M.lookup name $ _boops reg of
  Nothing -> do
    writeTimeAndError $ "There is no Boop named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ boops %~ M.delete name

dispatchConsumeAction_Free reg when (Free (Sampler _) name) =
  case M.lookup name $ _samplers reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sampler named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ samplers %~ M.delete name

dispatchConsumeAction_Free reg when (Free Sqfm name) =
  case M.lookup name $ _sqfms reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sqfm named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ sqfms %~ M.delete name

dispatchConsumeAction_Free reg when (Free Vap name) =
  case M.lookup name $ _vaps reg of
  Nothing -> do
    writeTimeAndError $ "There is no Vap named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ vaps %~ M.delete name

dispatchConsumeAction_Free reg when (Free Zot name) =
  case M.lookup name $ _zots reg of
  Nothing -> do
    writeTimeAndError $ "There is no Zot named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ zots %~ M.delete name

dispatchConsumeAction_Free _ _ (Send _ _ _) = error "dispatchConsumeAction_Free received a Send."
dispatchConsumeAction_Free _ _ (New _ _)    = error "dispatchConsumeAction_Free received a New."


-- | `dispatchConsumeAction_Send reg when (Send _ name msg)`,
-- if it doesn't find `name` in `reg`, logs an error.
-- Otherwise, it schedules an action to send everythingg in `msg` at `when`.
-- In the special case of a sampler receiving an "on" message,
--   it schedules an "off" message for shortly thereafter.
dispatchConsumeAction_Send :: SynthRegister -> Time -> Action -> IO ()
dispatchConsumeAction_Send reg when (Send Boop name msg) =
  case M.lookup name $ _boops reg of
    Nothing ->
      writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth BoopParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ boopMsg msg

dispatchConsumeAction_Send reg when (Send (Sampler _) name msg) =
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

dispatchConsumeAction_Send reg when (Send Sqfm name msg) =
  case M.lookup name $ _sqfms reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth SqfmParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ sqfmMsg msg

dispatchConsumeAction_Send reg when (Send Vap name msg) =
  case M.lookup name $ _vaps reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth VapParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ vapMsg msg

dispatchConsumeAction_Send reg when (Send Zot name msg) =
  case M.lookup name $ _zots reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth ZotParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ zotMsg msg

dispatchConsumeAction_Send _ _ (Free _ _) =
  error "dispatchConsumeAction_Send received a Free."
dispatchConsumeAction_Send _ _ (New _ _)  =
  error "dispatchConsumeAction_Send received a New."
