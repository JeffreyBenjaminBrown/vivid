{-# LANGUAGE ScopedTypeVariables,
ViewPatterns
#-}

module Montevideo.Monome.Window.Sustain (
  -- ** Data
    label
  , buttons
  , button_sustainLess
  , button_sustainMore
  , button_sustainOff
  , sustainWindow

  -- ** Functions
  , handler

  -- * `sustainOff`, `sustainMore`, and `sustainLess`
  , sustainOff                     -- ^ St EdoApp -> St EdoApp
  , sustainMore                    -- ^ St EdoApp -> St EdoApp
  , sustainLess                    -- ^ St EdoApp -> St EdoApp

  -- * utilities
  , sustainedVoices_inPitchClasses -- ^ St EdoApp -> [PitchClass EdoApp]
                                   -- -> Either String [VoiceId]
  , sustained_minus_fingered       -- ^ St EdoApp -> Set VoiceId
  , insertOneSustainReason -- ^ PitchClass -> LitPitches -> LitPitches
  , deleteOneSustainReason -- ^ PitchClass -> LitPitches -> LitPitches
  , buttonMsgs -- ^ Bool -> [(WindowId, ((X,Y), Bool))]
  ) where

import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Montevideo.Dispatch.Types.Many
import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Window.Common
import qualified Montevideo.Monome.Window.Keyboard as Kbd


-- ** Data

label :: WindowId
label = "sustain window"

buttons :: [(X,Y)]
buttons = [button_sustainLess, button_sustainMore, button_sustainOff]

-- | Press this to add more notes to what's being sustained
-- (whether or not any are currently sustained).
button_sustainMore :: (X,Y)
button_sustainMore = (0,15)

-- | Toggle this to enable erasure: Press a tone (in any octave)
-- to erase it from the sustained chord.
button_sustainLess :: (X,Y)
button_sustainLess = (1,15)

-- | Press this to turn off sustain.
-- This is also the button that lights to indicate sustain is on.
button_sustainOff :: (X,Y)
button_sustainOff = (0,14)

sustainWindow :: Window EdoApp
sustainWindow = Window {
    windowLabel = label
  , windowContains = flip elem buttons
    -- TODO : Better would be if, when nothing is sustained,
    -- this window only occupied the very coerner-most button,
    -- allowing the other two buttons to be part of the Keyboard window.
  , windowInit = id
  , windowHandler = handler
}


-- ** Functions

-- | How sustain works:
-- Releasing a button in the sustain window has no effect.
-- Pressing `button_sustainOff` releases all sustained voices.
-- Pressing `button_sustainMore` adds whatever is being fingered to
-- the set of voices being sustained, be they empty or not to begin with.
--
-- Most of the work of `handler` is passed off to `sustainOff`, `sustainMore`
-- and `sustainLess`. The only exception is the generation of messages
-- for SC and the LEDs, and updates to `_stVoices`.
-- `handler` creates those messages itself. `Main.handleSwitch` calls
-- `handler` to create those messages, and uses them to update `_stVoices`.
handler :: St EdoApp
        -> ( (X,Y) -- ^ ignored, since the sustain window has only one button
           , Switch)
        -> Either String (St EdoApp)

handler st (_ , False) = Right st

handler st ((==) button_sustainMore -> True,  True)  =
  mapLeft ("Window.Sustain.handler (sustainMore): " ++) $ do
  st1 <- sustainMore st
  Right $ if null $ st1 ^. stApp . edoSustaineded
          then st
          else st1 & stPending_Monome %~ flip (++) (buttonMsgs True)

handler st ((==) button_sustainLess -> True,  True)  =
  mapLeft ("Window.Sustain.handler (sustainLess): " ++) $ do
  ( st1 :: St EdoApp, toSilence :: [VoiceId] ) <-
    sustainLess st
  let scas :: [ScAction VoiceId] =
        -- If nothing is fingered, this is empty,
        -- and `handler` returns `st` unchanged.
        map silenceMsg toSilence
      st2 = st1 & stPending_Vivid  %~ flip (++) scas

   -- TODO This call to updateVoiceParams seems uneeded.
  Right $ foldr updateVoiceParams st2 scas

handler st ((==) button_sustainOff -> True,  True)  =
   mapLeft ("Window.Sustain.handler (sustainOff): " ++) $ do
   st1 <- sustainOff st
   toDark <- pitchClassesToDarken_uponSustainOff st st1
 
   let
     kbdMsgs :: [LedMsg] =
       map ( (Kbd.label,) . (,False) ) $
       concatMap (pcToXys_st st) $ toDark
     scas :: [ScAction VoiceId] =
       map silenceMsg $ S.toList $ sustained_minus_fingered st
     st2 = st1 & ( stPending_Monome %~ flip (++)
                   (buttonMsgs False ++ kbdMsgs) )
               & stPending_Vivid  %~ flip (++) scas

   -- TODO This call to updateVoiceParams seems uneeded.
   Right $ foldr updateVoiceParams st2 scas

handler _ b =
  error $ "Sustain.handler: Impossible button input: " ++ show b


-- * `sustainOff`, `sustainLess` and `sustainMore`
--
-- These functions change the value of `_edoSustaineded` and `_edoLit`.
-- The `handler` is in charge of creating LED and SC messages,
-- and thereby (indirectly) modifying `_stVoices`.

sustainOff :: St EdoApp -> Either String (St EdoApp)
sustainOff st =
  mapLeft ("sustainOff: " ++) $ let
  app = st ^. stApp
  in if null $ app ^. edoSustaineded
     then Right st -- nothing is sustained, so nothing to do
     else do

  susPcs :: [PitchClass EdoApp] <-
    mapM (vid_to_pitchClass st) $
    S.toList $ app ^. edoSustaineded
  Right $ st &   stApp . edoSustaineded .~ mempty
             & ( stApp . edoLit         .~
                 foldr deleteOneSustainReason (app ^. edoLit) susPcs )

sustainMore :: St EdoApp -> Either String (St EdoApp)
sustainMore st =
  mapLeft ("sustainMore: " ++) $ do
  let app = st ^. stApp
      fs :: [VoiceId] = M.elems $ app ^. edoFingers
  fPcs :: [PitchClass EdoApp] <-
    mapM (vid_to_pitchClass st) fs
  let lit' = foldr insertOneSustainReason (app ^. edoLit) fPcs
  Right $ st & stApp . edoSustaineded %~ S.union (S.fromList fs)
             & stApp . edoLit         .~ lit'

-- | `sustainLess st` returns `(st', pcs)`,
-- where `pcs` are the pitch classes that were being fingered in `st`.
--
-- todo ? You could argue it would be more convenient
-- if "sustainLess" was, rather than a momentary action,
-- a new state for the keyboard. You'd push a button to enter "delete mode",
-- then press keys to delete, then push the button again to exit that state.
-- This would make it easier to use with a single hand.
-- However, it would be slower to use, and harder to write.

sustainLess :: St EdoApp -> Either String ( St EdoApp
                                          , [VoiceId] )
sustainLess st =
  mapLeft ("sustainLess: " ++) $ do
  let app :: EdoApp = st ^. stApp
      fs :: [VoiceId] = M.elems $ app ^. edoFingers
  fPcs :: [PitchClass EdoApp] <-
           mapM (vid_to_pitchClass st) fs
  toSilence :: [VoiceId] <-
    sustainedVoices_inPitchClasses st fPcs
  Right $ ( -- If `fs` is empty, this is just `(st, [])`
            st & ( stApp . edoSustaineded %~
                   flip S.difference (S.fromList toSilence) )
               & ( stApp . edoLit         .~
                   foldr deleteOneSustainReason (app ^. edoLit) fPcs )
          , toSilence )


-- * utilities

-- | `sustainedVoices_inPitchClasses st pcs` returns every sustained voice
-- in `st` that is equal modulo the edo to something in `pcs`.
sustainedVoices_inPitchClasses
  :: St EdoApp -> [PitchClass EdoApp] -> Either String [VoiceId]
sustainedVoices_inPitchClasses st pcs =
  let susVs :: [VoiceId] = S.toList $ st ^. stApp . edoSustaineded
      isMatch :: VoiceId -> Either String (VoiceId, Bool)
      isMatch vid = do
        pc <- vid_to_pitchClass st vid
        Right ( vid
              , -- The call to `modEdo` below is unnecessary *if* the caller
                -- only sends `PitchClass`es, but it could send `Pitch`es.
                -- TODO ? Enforce, by using newtypes instead of aliases.
                elem pc $ S.map (modEdo st) $ S.fromList pcs )
  in map fst . filter snd <$> mapM isMatch susVs

pitchClassesToDarken_uponSustainOff ::
  St EdoApp -> St EdoApp -> Either String [PitchClass EdoApp]
  -- TODO ? speed: This calls `sustained_minus_fingered`.
  -- Would it be faster to pass the result of `sustained_minus_fingered`
  -- as a precomputed argument? (I'm guessing the compiler fogures it out.)

pitchClassesToDarken_uponSustainOff oldSt newSt = let
  -- `pitchClassesToDarken_uponSustainOff` is nearly equal to `sustained_minus_fingered`,
  -- but it excludes visual anchors as well as fingered notes.
    mustStayLit :: PitchClass EdoApp -> Either String Bool
    mustStayLit pc = case M.lookup pc $ newSt ^. stApp . edoLit of
      Nothing -> Right False
      Just s -> if null s
        then Left "Null value in LitPitches."
        else Right True

  in mapLeft ("pitchClassesToDarken_uponSustainOff: " ++) $ do
  voicesToSilence_pcs :: [PitchClass EdoApp] <-
    mapM (vid_to_pitchClass oldSt) $ S.toList $
    sustained_minus_fingered oldSt
  msls <- mapM mustStayLit voicesToSilence_pcs
  Right $ map snd $ filter (not . fst) $
    zip msls voicesToSilence_pcs

sustained_minus_fingered :: St EdoApp -> Set VoiceId
sustained_minus_fingered st = let
  sustained :: Set VoiceId =
    st ^. stApp . edoSustaineded
  fingered :: Set VoiceId =
    S.fromList $ M.elems $ st ^. stApp . edoFingers
  in S.difference sustained fingered


-- | `insertOneSustainReason pc` and `deleteOneSustainReason pc`
-- insert or delete, respectively, sustain as a reason for `pc` to be lit.
--
-- Why that's needed:
-- When sustain is toggled, the reasons for having LEDs on change.
-- If it is turned on, some LEDs are now lit for two reasons:
-- fingers and sustain. If it is turned off,
-- sustain is no longer a reason to light up any LEDs.
insertOneSustainReason, deleteOneSustainReason
  :: PitchClass EdoApp -> LitPitches EdoApp -> LitPitches EdoApp

insertOneSustainReason pc m =
  let reasons :: Set LedBecause =
        maybe S.empty id $ M.lookup pc m
  in M.insert pc (S.insert LedBecauseSustain reasons) m

deleteOneSustainReason pc m =
  case M.lookup pc m of
    Nothing -> m -- TODO ? Should this throw an error? It shouldn't happen.
    Just reasons -> let
      reasons' = S.delete LedBecauseSustain reasons
      in if null reasons'
         then M.delete pc m
         else M.insert pc reasons' m

buttonMsgs :: Bool -> [(WindowId, ((X,Y), Bool))]
buttonMsgs light =
  [ ( label, (button, light) )
  | button <- buttons ]
