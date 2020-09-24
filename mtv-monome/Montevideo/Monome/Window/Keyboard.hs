{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications
#-}

module Montevideo.Monome.Window.Keyboard (
    handler
  , keyboardWindow
  , label
  , edoKey_ScAction -- ^ EdoApp -> ((X,Y), Switch) -> [ScAction VoiceId]
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)

import           Montevideo.Dispatch.Types.Many
import qualified Montevideo.Monome.Config.Mtv as Config
import           Montevideo.Monome.EdoMath
import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Window.Common
import           Montevideo.Synth
import           Montevideo.Util
import           Montevideo.Monome.Window.Util


label :: WindowId
label = KeyboardWindow

keyboardWindow :: Window EdoApp
keyboardWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInitLeds = \st mi ->
      map ( ( (mi, label) ,)
            . (,True) ) $
      concatMap (pcToXys_st st) $
      M.keys $ st ^. stApp . edoLit
  , windowHandler = handler }

-- TODO ! duplicative of `JI.handler`
handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (mi, press@(xy,sw)) =
  mapLeft ("Keyboard handler: " ++) $ do
  let app = st ^. stApp
  vid <- if sw then Right $ nextVoice st
         else maybe (Left $ show xy ++ " not present in _edoFingers.")
              Right $ M.lookup (mi, xy) $ _edoFingers app
  let
    fingers'  :: Map (MonomeId, (X,Y)) VoiceId =
      app ^. edoFingers
      & if sw then M.insert (mi,xy) vid else M.delete (mi,xy)
    pcNow :: EdoPitchClass =
      pToPc_st st $ xyToEdo_app app xy
      -- what the key represents currently
    pcThen :: Maybe EdoPitchClass =
      ledBecause_toPitchClass @ EdoApp
      (app ^. edoLit) $ LedBecauseSwitch xy
      -- what the key represented when it was pressed,
      -- if it is now being released

    lit  :: LitPitches EdoApp = app ^. edoLit
    lit' :: LitPitches EdoApp = updateStLit (xy,sw) pcNow pcThen lit
    oldKeys :: Set EdoPitchClass = S.fromList $ M.keys $ lit
    newKeys :: Set EdoPitchClass = S.fromList $ M.keys $ lit'
    toDark  :: [EdoPitchClass] = S.toList $ S.difference oldKeys newKeys
    toLight :: [EdoPitchClass] = S.toList $ S.difference newKeys oldKeys

    kbdMsgs :: [LedMsg] = let
      x :: [((X,Y), Led)] =
        ( map (,False) $
          concatMap (pcToXys_st st) toDark) ++
        ( map (,True)  $
          concatMap (pcToXys_st st) toLight)
      in concat $ [ map ((mi', label) ,) x
                  | mi' <- _stKeyboards st ]
    scas :: [ScAction VoiceId] =
      edoKey_ScAction st vid press

    v :: Voice EdoApp = Voice
      { _voiceSynth  = Nothing
      , _voicePitch  = xyToEdo_app app xy
      , _voiceParams = mempty -- changed later, by `updateVoiceParams`
      }
    st1 :: St EdoApp = st
      & stApp . edoFingers .~ fingers'
      & stApp . edoLit     .~ lit'
      & stPending_Monome %~ (++ kbdMsgs)
      & stPending_Vivid  %~ (++ scas)
      & stVoices         %~ (if sw then M.insert vid v else id)
  Right $ foldr updateVoiceParams st1 scas

updateStLit :: ((X,Y), Switch)
  -> EdoPitchClass         -- ^ what xy represents now
  -> Maybe EdoPitchClass -- ^ what xy represented when last pressed
  -> LitPitches EdoApp
  -> LitPitches EdoApp

-- | When a button is newly pressed,
-- it adds anoother LedBecause to the LitPitches.
updateStLit (xy,True) pcNow _ m =
  M.insert pcNow new m where
  new = case M.lookup pcNow m of
    Nothing ->      S.singleton $ LedBecauseSwitch xy
    Just reasons -> S.insert (LedBecauseSwitch xy) reasons

-- | When a key is released, it might be that we've used the arrows
-- since pressing it. If that's so, then the pitch it triggered is elsewhere,
-- and illuminated. This removes the pitch from the LitPitches,
-- so that the appropriate pitch class will be darkened.
updateStLit (xy,False) _ mpcThen m =
  case mpcThen of
    Nothing -> m
    Just pc ->
      -- todo ? (#safety) Check that that's really what's being deleted.
      let Just reasons = M.lookup pc m
      in case S.size reasons < 2 of -- size < 1 should not happen
        True -> M.delete pc m
        False -> M.insert pc (S.delete (LedBecauseSwitch xy) reasons) m

-- TODO ! duplicative of `jiKey_ScAction`
edoKey_ScAction :: St EdoApp -> VoiceId -> ((X,Y), Switch)
                -> [ScAction VoiceId]
edoKey_ScAction st vid (xy, sw) = do
  let app = _stApp st
      pitch = xyToEdo_app app xy
      ec = app ^. edoConfig
  if S.member vid $ app ^. edoSustaineded
    then ( -- it's already sounding due to sustain
           -- TODO : This is no longer possible, because now every keypress
           -- creates a new voice.
           [] )

    else if sw -- sw <=> the key was pressed, rather than released
         then [ ScAction_New
                { _actionSynthDefEnum = Zot
                , _actionSynthName = vid
                , _actionScMsg =
                  M.mapKeys show -- show :: ZotParam -> String
                  $ M.union -- in fonclict, the first arg takes priority
                  ( M.fromList [ (Zot_freq, Config.freq *
                                            edoToFreq ec pitch) ] )
                  $ _stZotDefaults st } ]
         else [silenceMsg vid]
