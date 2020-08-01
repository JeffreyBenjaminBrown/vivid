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
  , act     -- ^ SynthRegister -> Time -> Action -> IO (SynthRegister -> SynthRegister)
  , actNew  -- ^ SynthRegister -> Action -> IO (SynthRegister -> SynthRegister)
  , actFree -- ^ SynthRegister -> Rational -> Action -> IO (SynthRegister -> SynthRegister)
  , actSend -- ^ SynthRegister -> Time -> Action -> IO ()
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
-- Dispatch.Dispatch uses actNew, actFree, actSend, but not this.
act :: SynthRegister -> Time -> Action
    -> IO (SynthRegister -> SynthRegister)
act reg t a@(Send _ _ _) = actSend reg t a >> return id
act reg t a@(Free _ _)   = actFree reg t a
act reg _ a@(New _ _)    = actNew  reg   a


-- | `actNew reg (New Boop name)`,
-- if it finds `name`, logs an error and returns the identity.
-- Otherwise, it creates the synth (immediately -- no scheduling),
--   and returns how to insert it into synth reg.
--   The reg has a separate field for each different kind of synth.
-- In the special case of a sampler, it also looks up the buffer.
--   If found, it uses the buffer as an argument to the sampler synth created.
actNew :: SynthRegister -> Action -> IO (SynthRegister -> SynthRegister)
actNew reg (New Boop name) =
  case M.lookup name $ _boops reg of
    Nothing -> do s <- V.synth boop ()
                  return $ boops %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Boop named " ++ name
            return id

actNew reg (New (Sampler sample) name) =
  case M.lookup name $ _samplers reg of
    Nothing -> do
      case M.lookup sample $ reg ^. samples of
        Nothing -> do writeTimeAndError $
                        "actNew: Sample " ++ show sample ++ " not found."
                      return id
        Just buf -> do
          s <- V.synth sampler (V.b2i buf :: V.I "buffer")
          return $ samplers %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sampler named " ++ name
            return id

actNew reg (New Sqfm name) =
  case M.lookup name $ _sqfms reg of
    Nothing -> do s <- V.synth sqfm ()
                  return $ sqfms %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Sqfm named " ++ name
            return id

actNew reg (New Vap name) =
  case M.lookup name $ _vaps reg of
    Nothing -> do s <- V.synth vap ()
                  return $ vaps %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Vap named " ++ name
            return id

actNew reg (New Zot name) =
  case M.lookup name $ _zots reg of
    Nothing -> do s <- V.synth zot ()
                  return $ zots %~ M.insert name s
    _ -> do writeTimeAndError $ "There is already a Zot named " ++ name
            return id

actNew _ (Send _ _ _) = error $ "actNew received a Send."
actNew _ (Free _ _)   = error $ "actNew received a Free."


-- | `actFree reg when (Free Boop name)`,
-- if it doesn't find `name`, logs an error and returns `id`.
-- If it does, it
--   schedules an amp=0 message for its "when" argument,
--   schedules a free message for shortly thereafter,
--   and returns a way to delete the synth.
actFree :: SynthRegister
        -> Rational
        -> Action
        -> IO (SynthRegister -> SynthRegister)
actFree reg when (Free Boop name) =
  case M.lookup name $ _boops reg of
  Nothing -> do
    writeTimeAndError $ "There is no Boop named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ boops %~ M.delete name

actFree reg when (Free (Sampler _) name) =
  case M.lookup name $ _samplers reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sampler named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ samplers %~ M.delete name

actFree reg when (Free Sqfm name) =
  case M.lookup name $ _sqfms reg of
  Nothing -> do
    writeTimeAndError $ "There is no Sqfm named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ sqfms %~ M.delete name

actFree reg when (Free Vap name) =
  case M.lookup name $ _vaps reg of
  Nothing -> do
    writeTimeAndError $ "There is no Vap named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ vaps %~ M.delete name

actFree reg when (Free Zot name) =
  case M.lookup name $ _zots reg of
  Nothing -> do
    writeTimeAndError $ "There is no Zot named " ++ name ++ "to free."
    return id
  Just s -> do
    V.doScheduledAt (V.Timestamp $ fr when) $ set' s $ Msg' (0 :: V.I "amp")
    V.doScheduledAt (V.Timestamp $ fr $ when + frameDuration / 2) $ V.free s
    return $ zots %~ M.delete name

actFree _ _ (Send _ _ _) = error "actFree received a Send."
actFree _ _ (New _ _)    = error "actFree received a New."


-- | `actSend reg when (Send _ name msg)`,
-- if it doesn't find `name` in `reg`, logs an error.
-- Otherwise, it schedules an action to send everythingg in `msg` at `when`.
-- In the special case of a sampler receiving an "on" message,
--   it schedules an "off" message for shortly thereafter.
actSend :: SynthRegister -> Time -> Action -> IO ()
actSend reg when (Send Boop name msg) =
  case M.lookup name $ _boops reg of
    Nothing ->
      writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth BoopParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ boopMsg msg

actSend reg when (Send (Sampler _) name msg) =
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

actSend reg when (Send Sqfm name msg) =
  case M.lookup name $ _sqfms reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth SqfmParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ sqfmMsg msg

actSend reg when (Send Vap name msg) =
  case M.lookup name $ _vaps reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth VapParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ vapMsg msg

actSend reg when (Send Zot name msg) =
  case M.lookup name $ _zots reg of
    Nothing -> writeTimeAndError $ " The name " ++ name ++ " is not in use.\n"
    Just (s :: V.Synth ZotParams) ->
      V.doScheduledAt (V.Timestamp $ fromRational when)
      $ mapM_ (set' s) $ zotMsg msg

actSend _ _ (Free _ _) = error "actSend received a Free."
actSend _ _ (New _ _)  = error "actSend received a New."
