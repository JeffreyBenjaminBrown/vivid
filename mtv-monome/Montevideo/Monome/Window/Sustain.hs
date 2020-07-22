{-# LANGUAGE DataKinds
, ScopedTypeVariables
, TupleSections
#-}

module Montevideo.Monome.Window.Sustain (
    handler
  , label
  , sustainWindow
  , theButton

  , voicesToSilence_uponSustainOff -- ^ St EdoApp -> Set VoiceId
  , toggleSustain                  -- ^ St EdoApp -> St EdoApp
  , insertOneSustainedNote -- ^ PitchClass -> LitPitches -> LitPitches
  , deleteOneSustainedNote -- ^ PitchClass -> LitPitches -> LitPitches
  ) where

import           Control.Lens
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S

import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Types.Button
import           Montevideo.Monome.Types.Initial
import           Montevideo.Monome.Window.Common
import qualified Montevideo.Monome.Window.Keyboard as Kbd


label :: WindowId
label = "sustain window"

theButton :: (X,Y)
theButton = (0,15)

sustainWindow :: Window EdoApp
sustainWindow = Window {
    windowLabel = label
  , windowContains = (==) theButton
  , windowInit = id
  , windowHandler = handler
}

handler :: St EdoApp
        -> ( (X,Y) -- ^ ignored, since the sustain window has only one button
           , Switch)
        -> St EdoApp
handler    st    (_ , False)      = st
handler    st    (_,  True)      = let
  st1 = toggleSustain st

  kbdMsgs :: [LedMsg] =
    if null $ st1 ^. stApp . edoSustaineded
    then map ( (Kbd.label,) . (,False) ) $
         concatMap (pcToXys_st st) $
         pitchClassesToDarken_uponSustainOff st st1
    else []
  sdMsgs :: [SoundMsg EdoApp] =
    if null $ st1 ^. stApp . edoSustaineded
    then map silenceMsg $ S.toList $ voicesToSilence_uponSustainOff st
    else []

  sustainButtonMsg = ( label
                     , (theButton, isJust $ st1 ^. stApp . edoSustaineded) )
  st2 = st1 & stPending_Monome %~ flip (++) (sustainButtonMsg : kbdMsgs)
            & stPending_Vivid  %~ flip (++) sdMsgs
  in foldr updateVoice st2 sdMsgs

pitchClassesToDarken_uponSustainOff ::
  St EdoApp -> St EdoApp -> Set (PitchClass EdoApp)
  -- TODO ? speed: This calls `voicesToSilence_uponSustainOff`.
  -- Would it be faster to pass the result of `voicesToSilence_uponSustainOff`
  -- as a precomputed argument? (I'm guessing the compiler fogures it out.)
pitchClassesToDarken_uponSustainOff oldSt newSt =
  -- `pitchClassesToDarken_uponSustainOff` is nearly equal to `voicesToSilence_uponSustainOff`,
  -- but it excludes visual anchors as well as fingered notes.
  S.filter (not . mustStayLit) $ voicesToSilence_pcs
  where
    mustStayLit :: PitchClass EdoApp -> Bool
    mustStayLit pc = case M.lookup pc $ newSt ^. stApp . edoLit of
      Nothing -> False
      Just s -> if null s
        then error "pitchClassesToDarken_uponSustainOff: null value in LitPitches."
        else True
    voicesToSilence_pcs :: Set (PitchClass EdoApp) =
      S.map (vid_to_pitch oldSt) $ voicesToSilence_uponSustainOff oldSt

voicesToSilence_uponSustainOff :: St EdoApp -> Set VoiceId
voicesToSilence_uponSustainOff st = let
  sustained :: Set VoiceId =
    maybe mempty id $ st ^. stApp . edoSustaineded
  fingered :: Set VoiceId =
    S.fromList $ M.keys $ st ^. stApp . edoFingers
  in S.difference sustained fingered

-- | When the sustain button is toggled --
-- which happens only when it is pressed, not when it is released --
-- the set of sustained pitches changes
-- and the set of lit keys gains new reasons to be lit.
toggleSustain :: St EdoApp -> St EdoApp
toggleSustain st = let
  app = st ^. stApp
  in if null (app ^. edoFingers) && null (app ^. edoSustaineded)
  then st
  else let

  sustainOn' :: Bool = -- new sustain state
    not $ isJust $ app ^. edoSustaineded
  sustainedVs :: Maybe (Set VoiceId) =
    if not sustainOn' then Nothing
    else Just $ S.fromList $ M.elems $ app ^. edoFingers

  lit' | sustainOn' =
         foldr insertOneSustainedNote (app ^. edoLit)
         $ map (vid_to_pitch st)
         $ M.elems $ app ^. edoFingers
       | otherwise =
         foldr deleteOneSustainedNote (app ^. edoLit)
         $ map (vid_to_pitch st) $ S.toList
         $ maybe (error "impossible") id $ app ^. edoSustaineded
  in st & stApp . edoSustaineded .~ sustainedVs
        & stApp . edoLit       .~ lit'

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
