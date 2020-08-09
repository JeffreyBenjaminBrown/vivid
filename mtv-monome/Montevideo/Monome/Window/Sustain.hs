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
import           Data.Either.Combinators
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S

import           Montevideo.Dispatch.Types.Many
import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Util.Button
import           Montevideo.Monome.Types.Most
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
        -> Either String (St EdoApp)
handler st (_ , False) = Right st
handler st (_,  True)  =
  mapLeft ("Window.Sustain.handler: " ++) $ do
  st1 <- toggleSustain st
  toDark <- pitchClassesToDarken_uponSustainOff st st1

  let
    kbdMsgs :: [LedMsg] =
      if null $ st1 ^. stApp . edoSustaineded
      then map ( (Kbd.label,) . (,False) ) $
           concatMap (pcToXys_st st) $ toDark
      else []
    sdMsgs :: [ScAction VoiceId] =
      if null $ st1 ^. stApp . edoSustaineded
      then map silenceMsg $ S.toList $ voicesToSilence_uponSustainOff st
      else []

    sustainButtonMsg = ( label
                       , ( theButton
                         , isJust $ st1 ^. stApp . edoSustaineded ) )
    st2 = st1 & stPending_Monome %~ flip (++) (sustainButtonMsg : kbdMsgs)
              & stPending_Vivid  %~ flip (++) sdMsgs
  Right $ foldr updateVoiceParams st2 sdMsgs

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
    mapM (vid_to_pitch oldSt) $ S.toList $
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

-- | When the sustain button is toggled --
-- which happens only when it is pressed, not when it is released --
-- the set of sustained pitches changes
-- and the set of lit keys gains new reasons to be lit.
toggleSustain :: St EdoApp -> Either String (St EdoApp)
toggleSustain st =
  mapLeft ("toggleSustain: " ++) $ let
  app = st ^. stApp
  in if null (app ^. edoFingers) && null (app ^. edoSustaineded)
  then Right st else do

  let
    sustainOn' :: Bool = -- new sustain state
      not $ isJust $ app ^. edoSustaineded -- TODO ? could be an optic
    sustainedVs :: Maybe (Set VoiceId) =
      if not sustainOn' then Nothing
      else Just $ S.fromList $ M.elems $ app ^. edoFingers

  pcs :: [PitchClass EdoApp] <-
    mapM (vid_to_pitch st) $
    if sustainOn'
    then M.elems $ app ^. edoFingers
    else S.toList $ maybe (error "impossible") id $
         app ^. edoSustaineded
  let lit' = if sustainOn'
             then foldr insertOneSustainedNote (app ^. edoLit) pcs
             else foldr deleteOneSustainedNote (app ^. edoLit) pcs
  Right $ st & stApp . edoSustaineded .~ sustainedVs
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
