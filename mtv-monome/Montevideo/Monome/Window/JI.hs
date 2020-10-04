{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Monome.Window.JI (
    handler
  , jiWindow
  , label

  , jiKey_ScAction -- ^ JiApp -> ((X,Y), Switch) -> [ScAction VoiceId]
  , jiFreq         -- ^ JiApp -> (X,Y) -> Either String Float
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M

import           Montevideo.Dispatch.Types.Many
import qualified Montevideo.Monome.Config.Mtv as Config
import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Util
import           Montevideo.Synth
import           Montevideo.Util


label :: WindowId
label = KeyboardWindow

jiWindow :: Window JiApp
jiWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInitLeds = \_ _ -> Right []
  , windowHandler = handler }

-- TODO untested
-- TODO ! duplicative of `Keyboard.handler`
handler :: St JiApp
        -> ( MonomeId -- ^ So far, the JI app doesn't send LED messages,
                      -- so it doesn't use this argument.
           , ((X,Y), Switch ))
        -> Either String (St JiApp)
handler st (_, press@ (xy,sw)) =
  mapLeft ("JI handler: " ++) $ do
  let app = st ^. stApp
  vid <- if sw then Right $ nextVoice st
         else maybe (Left $ show xy ++ " not present in _edoFingers.")
              Right $ M.lookup xy $ _jiFingers app
  pitch :: Rational <- jiFreq app xy

  let
    fingers' = app ^. jiFingers
               & case sw of
                   True  -> M.insert xy vid
                   False -> M.delete xy
    scas :: [ScAction VoiceId] =
      jiKey_ScAction app vid press
    v :: Voice JiApp = Voice
      { _voiceSynth  = Nothing
      , _voicePitch  = pitch
      , _voiceParams = mempty -- changed later, by `updateVoiceParams`
      }
    st1 :: St JiApp = st
      & stApp . jiFingers .~ fingers'
      & stPending_Vivid   %~ (++ scas)
      & stVoices          %~ (if sw then M.insert vid v else id)
  Right $ foldr updateVoiceParams st1 scas

-- TODO ! duplicative of `edoKey_ScAction`
jiKey_ScAction :: JiApp -> VoiceId -> ((X,Y), Switch) -> [ScAction VoiceId]
jiKey_ScAction ja vid (xy,switch) = let
  doIfKeyFound :: Rational -> [ScAction VoiceId]
  doIfKeyFound freq =
    if switch
      then [ ScAction_New
             { _actionSynthDefEnum = Zot
             , _actionSynthName = vid
             , _actionScParams = M.fromList
               [ ("freq", Config.freq * Config.jiTranspose * fr freq)
               , ("amp", Config.amp) ] } ]
      else [silenceMsg vid]
  in either (const []) doIfKeyFound $ jiFreq ja xy
     -- returns [] if key out of range; key corresponds to no pitch

jiFreq :: JiApp -> (X,Y) -> Either String Rational
jiFreq ja (x,y) =
  mapLeft ("jiFreq: " ++) $ do
  let (yOctave :: Int, yShift :: Int) =
        divMod y $ length $ ja ^. jiShifts
      (xOctave :: Int, xGen :: Int) =
        divMod x $ length $ ja ^. jiGenerator
      f0 :: Rational =
        (ja ^. jiGenerator) !! xGen
        -- !! is safe here, because of the divMod that defines xGen
  Right $ f0
    * ((ja ^. jiShifts) !! yShift)
    -- (!!) is safe here, because of the divMod that defines yShift
    * (2 ^^ (yOctave + xOctave))
    -- Rational exponentiation (^^) because `yOctave + xOctave` could be < 0.
    -- THey are always an integer, though, so this could be more efficient
    -- by using integer exponentiation (^) and writing a little more code.
