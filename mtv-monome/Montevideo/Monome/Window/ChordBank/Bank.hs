-- | A window for changing what windows are on a monome.

module Montevideo.Monome.Window.ChordBank.Bank (
  chordBankWindow
  ) where

import           Prelude

import           Control.Lens
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe

import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Util
import           Montevideo.Synth
import           Montevideo.Synth.Msg
import           Montevideo.Util


label :: WindowId
label = ChordBankWindow

chordBankWindow :: Window EdoApp
chordBankWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) ->
      numBetween 0 7 x &&
      numBetween 0 7 y
  , windowInitLeds = \_ _ -> Right []
  , windowHandler = \x y -> return $ handler x y }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)

---- | TODO: Add to LitPitches.
handler st (mi, (xy, True)) = do
  let app = _stApp st
      mPitches :: Maybe [Pitch EdoApp] =
        app ^. edoChordBank . chords . at xy
  if isNothing mPitches then Right st else do

    let Just pitches = mPitches
        ec = app ^. edoConfig
        light :: [LedMsg] =
          [ ((mi, label), (xy, True)) ]
        pvvs :: [ (Pitch EdoApp, VoiceId, Voice EdoApp) ] = let
          pToV :: Pitch EdoApp -> Voice EdoApp
          pToV p = Voice
            { _voiceSynth = Nothing
            , _voicePitch = p
            , _voiceParams = mempty } -- updated by updateVoiceParams next
          in map (\(p,vi) -> (p, vi, pToV p)) $
             zip pitches [nextVoice st..]
        sound :: [ScAction VoiceId] =
          [ monome_scActionNew ec vi (_stZotDefaults st) p
          | (p,vi,_) <- pvvs ]
        st1 :: St EdoApp = st
          & stPending_Monome %~ (++ light)
          & stPending_Vivid  %~ (++ sound)
          & stVoices %~
            let g :: (a, VoiceId, VE) -> (Map VoiceId VE -> Map VoiceId VE)
                                      -> (Map VoiceId VE -> Map VoiceId VE)
                g (_,vi,v) f = f . M.insert vi v
            in foldr g id pvvs
    Right $ foldr updateVoiceParams st1 sound

-- | TODO: Subtract from LitPitches.
handler st (mi, (xy, False)) =
  if isNothing $ st ^. stApp . edoChordBank . chords . at xy
  then Right st
  else let
    darken :: [LedMsg] =
      [ ((mi, label), (xy, False)) ]
    silence :: [ScAction VoiceId] =
      [ ScAction_Free { _actionSynthDefEnum = Zot
                      , _actionSynthName = v }
      | v :: VoiceId <- st ^. stApp . edoChordBank . chordPlaying ]
    in Right $ st
       & stApp . edoChordBank . chordPlaying .~ []
       & stPending_Monome %~ (++ darken)
       & stPending_Vivid %~ (++ silence)

type VE = Voice EdoApp
