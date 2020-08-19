{-# LANGUAGE ScopedTypeVariables,
ViewPatterns
#-}

module Montevideo.Monome.Window.Sustain (
    handler
  , label
  , sustainWindow
  , button_sustainMore
  , button_sustainOff

  , voicesToSilence_uponSustainOff -- ^ St EdoApp -> Set VoiceId
  , sustainMore                    -- ^ St EdoApp -> St EdoApp
  , sustainOff                     -- ^ St EdoApp -> St EdoApp
  , insertOneSustainedNote -- ^ PitchClass -> LitPitches -> LitPitches
  , deleteOneSustainedNote -- ^ PitchClass -> LitPitches -> LitPitches
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


label :: WindowId
label = "sustain window"

-- | Press this to turn off sustain.
-- This is also the button that lights to indicate sustain is on.
button_sustainOff :: (X,Y)
button_sustainOff = (0,14)

-- | Press this to add more notes to what's being sustained
-- (whether or not any are currently sustained).
button_sustainMore :: (X,Y)
button_sustainMore = (0,15)

sustainWindow :: Window EdoApp
sustainWindow = Window {
    windowLabel = label
  , windowContains = \(x,y) -> x == 0 &&
                               (y == 14 || y == 15)
  , windowInit = id
  , windowHandler = handler
}

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
  mapLeft ("Window.Sustain.handler: " ++) $ do
  st1 <- sustainMore st
  Right $ if null $ st1 ^. stApp . edoSustaineded
          then st
          else st1 & stPending_Monome %~ flip (++) (buttonMsgs True)

handler st ((==) button_sustainOff -> True,  True)  =
  mapLeft ("Window.Sustain.handler: " ++) $ do
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

handler _ _ =
  error "Sustain.handler: uncaught (and, I believe, impossible) input."

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
  let lit' = foldr deleteOneSustainedNote (app ^. edoLit) pcs
  Right $ st & stApp . edoSustaineded .~ Nothing
             & stApp . edoLit         .~ lit'

sustainMore :: St EdoApp -> Either String (St EdoApp)
sustainMore st =
  mapLeft ("sustainOn: " ++) $
  let app = st ^. stApp
  in case M.elems $ app ^. edoFingers :: [VoiceId] of
       [] -> Right st
       vs -> do
         pcs :: [PitchClass EdoApp] <-
           mapM (vid_to_pitchClass st) $
           M.elems $ app ^. edoFingers
         let lit' = foldr insertOneSustainedNote (app ^. edoLit) pcs
             vs1 :: Set VoiceId = S.fromList vs
             vs2 :: Set VoiceId = maybe vs1 (S.union vs1) $
                                  app ^. edoSustaineded
         Right $ st & stApp . edoSustaineded .~ Just vs2
                    & stApp . edoLit         .~ lit'

-- | When sustain is toggled, the reasons for having LEDs on change.
-- If it is turned on, some LEDs are now lit for two reasons:
-- fingers and sustain. If it is turned off,
-- sustain is no longer a reason to light up any LEDs.
insertOneSustainedNote, deleteOneSustainedNote
  :: PitchClass EdoApp -> LitPitches EdoApp -> LitPitches EdoApp
insertOneSustainedNote pc m =
  let reasons :: Set LedBecause =
        maybe S.empty id $ M.lookup pc m
  in M.insert pc (S.insert LedBecauseSustain reasons) m
deleteOneSustainedNote pc m =
  case M.lookup pc m of
    Nothing -> m -- TODO ? Should this throw an error? It shouldn't happen.
    Just reasons -> let
      reasons' = S.delete LedBecauseSustain reasons
      in if null reasons'
         then M.delete pc m
         else M.insert pc reasons' m

buttonMsgs :: Bool -> [(WindowId, ((X,Y), Bool))]
buttonMsgs light = [ ( label, (button, light) )
                   | button <- [button_sustainMore, button_sustainOff] ]
