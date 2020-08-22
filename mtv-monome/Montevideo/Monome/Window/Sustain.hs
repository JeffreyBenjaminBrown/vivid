{-# LANGUAGE ScopedTypeVariables,
ViewPatterns
#-}

module Montevideo.Monome.Window.Sustain (
    label
  , buttons
  , button_sustainLess
  , button_sustainMore
  , button_sustainOff
  , sustainWindow

  , handler
  , sustainedVoices_inPitchClasses -- ^ St EdoApp -> [PitchClass EdoApp]
                                   -- -> Either String [VoiceId]
  , voicesToSilence_uponSustainOff -- ^ St EdoApp -> Set VoiceId
  , sustainOff                     -- ^ St EdoApp -> St EdoApp
  , sustainMore                    -- ^ St EdoApp -> St EdoApp
  , sustainLess                    -- ^ St EdoApp -> St EdoApp
  , insertOneSustainReason -- ^ PitchClass -> LitPitches -> LitPitches
  , deleteOneSustainReason -- ^ PitchClass -> LitPitches -> LitPitches
  , buttonMsgs -- ^ Bool -> [(WindowId, ((X,Y), Bool))]
  ) where

import           Control.Lens
import           Data.Either.Combinators
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Montevideo.Dispatch.Types.Many
import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Window.Common
import qualified Montevideo.Monome.Window.Keyboard as Kbd


-- * Data

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


-- * Functions

-- | How sustain works:
-- Releasing a button in the sustain window has no effect.
-- Pressing `button_sustainOff` releases all sustained voices.
-- Pressing `button_sustainMore` adds whatever is being fingered to
-- the set of voices being sustained, be they empty or not to begin with.
--
-- Most of the work of this handler is passed off to `sustainOff`
-- and `sustainMore`. The only exception is that those two inner functions
-- do not deal with the generation or processing of `LedMsg`s or `ScMsg`s.
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
  ( st1 :: St EdoApp, pcs :: [PitchClass EdoApp] ) <-
    sustainLess st
  scas :: [ScAction VoiceId] <-
    -- If nothing is fingered, this is empty,
    -- and `handler` returns `st` unchanged.
    map silenceMsg <$> sustainedVoices_inPitchClasses st pcs
  let st2 = st1 & stPending_Vivid  %~ flip (++) scas
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
       map silenceMsg $ S.toList $ voicesToSilence_uponSustainOff st
     st2 = st1 & ( stPending_Monome %~ flip (++)
                   (buttonMsgs False ++ kbdMsgs) )
               & stPending_Vivid  %~ flip (++) scas
   Right $ foldr updateVoiceParams st2 scas

handler _ b =
  error $ "Sustain.handler: Impossible button input: " ++ show b

-- | `sustainedVoices_inPitchClasses st pcs` returns every sustained voice
-- in `st` that is equal modulo the edo to something in `pcs`.
sustainedVoices_inPitchClasses
  :: St EdoApp -> [PitchClass EdoApp] -> Either String [VoiceId]
sustainedVoices_inPitchClasses st pcs =
  case S.toList <$> st ^. stApp . edoSustaineded of
    Nothing -> Right []
    Just (susVs :: [VoiceId]) -> do

      let m :: Pitch EdoApp -> PitchClass EdoApp
          m = flip mod $ st ^. stApp . edoConfig . edo
          vp :: VoiceId -> Either String (Pitch EdoApp)
          vp vid = do
            v :: Voice EdoApp <- let
              err = Left $ "Voice " ++ show vid ++ " not found."
              in maybe err Right $ M.lookup vid $ _stVoices st
            return $ m $ _voicePitch v
          isMatch :: VoiceId -> Either String (VoiceId, Bool)
          isMatch vid = do
            pc <- vp vid
            Right ( vid
                  , -- `map m` below is unnecessary *if* the caller only
                    -- sends `PitchClass`es, but it could send `Pitch`es.
                    -- TODO ? Enforce, by using newtypes instead of aliases.
                    elem pc $ S.map m $ S.fromList $ pcs )
      map fst . filter snd <$> mapM isMatch susVs

pitchClassesToDarken_uponSustainOff ::
  St EdoApp -> St EdoApp -> Either String [PitchClass EdoApp]
  -- TODO ? speed: This calls `voicesToSilence_uponSustainOff`.
  -- Would it be faster to pass the result of `voicesToSilence_uponSustainOff`
  -- as a precomputed argument? (I'm guessing the compiler fogures it out.)

pitchClassesToDarken_uponSustainOff oldSt newSt = let
  -- `pitchClassesToDarken_uponSustainOff` is nearly equal to `voicesToSilence_uponSustainOff`,
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
    voicesToSilence_uponSustainOff oldSt
  msls <- mapM mustStayLit voicesToSilence_pcs
  Right $ map snd $ filter (not . fst) $
    zip msls voicesToSilence_pcs

voicesToSilence_uponSustainOff :: St EdoApp -> Set VoiceId
voicesToSilence_uponSustainOff st = let
  sustained :: Set VoiceId =
    maybe mempty id $ st ^. stApp . edoSustaineded
  fingered :: Set VoiceId =
    S.fromList $ M.elems $ st ^. stApp . edoFingers
  in S.difference sustained fingered

sustainOff :: St EdoApp -> Either String (St EdoApp)
sustainOff st =
  mapLeft ("sustainOff: " ++) $ let
  app = st ^. stApp
  in if null $ app ^. edoSustaineded
     then Right st -- nothing is sustained, so nothing to do
     else do

  pcs :: [PitchClass EdoApp] <-
    mapM (vid_to_pitchClass st) $
    S.toList $ maybe (error "impossible") id $
    app ^. edoSustaineded
  let lit' = foldr deleteOneSustainReason (app ^. edoLit) pcs
  Right $ st & stApp . edoSustaineded .~ Nothing
             & stApp . edoLit         .~ lit'

sustainMore :: St EdoApp -> Either String (St EdoApp)
sustainMore st =
  mapLeft ("sustainMore: " ++) $
  let app = st ^. stApp
  in case M.elems $ app ^. edoFingers :: [VoiceId] of
       [] -> Right st
       vs -> do
         pcs :: [PitchClass EdoApp] <-
           mapM (vid_to_pitchClass st) $
           M.elems $ app ^. edoFingers
         let lit' = foldr insertOneSustainReason (app ^. edoLit) pcs
             vs1 :: Set VoiceId = S.fromList vs
             vs2 :: Set VoiceId = maybe vs1 (S.union vs1) $
                                  app ^. edoSustaineded
         Right $ st & stApp . edoSustaineded .~ Just vs2
                    & stApp . edoLit         .~ lit'

-- | `sustainLess st` returns `(st', pcs)`,
-- where `pcs` are the pitch classes that were being fingered in `st`,
--
-- todo ? You could argue it would be more convenient
-- if "sustainLess" was, rather than a momentary action,
-- a new state for the keyboard. You'd push a button to enter "delete mode",
-- then press keys to delete, then push the button again to exit that state.
-- This would make it easier to use with a single hand.
-- However, it would be slower to use, and harder to write.

sustainLess :: St EdoApp -> Either String ( St EdoApp
                                          , [PitchClass EdoApp] )
sustainLess st =
  mapLeft ("sustainLess: " ++) $
  let app = st ^. stApp
  in case M.elems $ app ^. edoFingers :: [VoiceId] of
       [] -> Right (st, [])
       vs -> do

         pcs :: [PitchClass EdoApp] <-
           mapM (vid_to_pitchClass st) $
           M.elems $ app ^. edoFingers
         let lit' = foldr deleteOneSustainReason (app ^. edoLit) pcs
             vs1 :: Set VoiceId = S.fromList vs
             vs2 :: Set VoiceId = maybe vs1 (flip S.difference vs1) $
                                  app ^. edoSustaineded
         Right $ ( st & stApp . edoSustaineded .~ Just vs2
                      & stApp . edoLit         .~ lit'
                 , pcs )

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
