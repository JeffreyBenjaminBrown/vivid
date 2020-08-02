{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds
, TupleSections
, ScopedTypeVariables
#-}

module Montevideo.Monome.Window.JI (
    handler
  , jiWindow
  , label

  , jiKey_ScAction -- ^ JiApp -> ((X,Y), Switch) -> [SoundMsg]
  , jiFreq         -- ^ JiApp -> (X,Y) -> Either String Float
  ) where

import           Prelude hiding (pred)
import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Map as M

import           Montevideo.Dispatch.Types.Many
import qualified Montevideo.Monome.Config as Config
import           Montevideo.Monome.Util.Button
import           Montevideo.Monome.Types.Most
import           Montevideo.Monome.Window.Common
import           Montevideo.Util
import           Montevideo.Synth


label :: WindowId
label = "ji window"

jiWindow :: Window JiApp
jiWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> let pred = numBetween 0 15
                               in pred x && pred y
  , windowInit = id
  , windowHandler = handler }

-- TODO untested
-- TODO ! duplicative of `Keyboard.handler`
handler :: St JiApp
        -> ((X,Y), Switch)
        -> Either String (St JiApp)
handler st press@ (xy,sw) =
  mapLeft ("JI handler: " ++) $ let
  app = st ^. stApp
  fingers' = app ^. jiFingers
             & case sw of
                 True  -> M.insert xy xy
                 False -> M.delete xy
  soundMsgs :: [ScAction VoiceId] =
    jiKey_ScAction app press

  in do
  pitch <- jiFreq app xy
  let
    st1 :: St JiApp = st
      & stApp . jiFingers                     .~ fingers'
      & stPending_Vivid                       %~ (++ soundMsgs)
      & stVoices . at xy . _Just . voicePitch .~ pitch
  Right $ foldr updateVoiceParams st1 soundMsgs

-- TODO ! duplicative of `edoKey_ScAction`
jiKey_ScAction :: JiApp -> ((X,Y), Switch) -> [ScAction VoiceId]
jiKey_ScAction ja (xy,switch) = let
  doIfKeyFound :: Rational -> [ScAction VoiceId]
  doIfKeyFound freq =
    if switch
      then [ ScAction_Send
             { _actionSynthDefEnum = Boop
             , _actionSynthName = xy
             , _actionScMsg = M.fromList
               [ ("freq", Config.freq * fr freq)
               , ("amp", Config.amp) ] } ]
      else [silenceMsg xy]
  in either (const []) doIfKeyFound $ jiFreq ja xy
     -- [] if key out of range; key corresponds to no pitch

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
