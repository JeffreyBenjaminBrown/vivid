{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Montevideo.Test.Recording where

import Test.HUnit

import           Control.Lens hiding (op)
import qualified Data.Map as M
import           Data.Ratio
import qualified Data.Vector as V

import Montevideo.Dispatch.Recording
import Montevideo.Dispatch.Types.Many
import Montevideo.Dispatch.Types.Time
import Montevideo.Synth
import Montevideo.Synth.Msg


tests :: Test
tests = TestList [
    TestLabel "test_monomeRecording_toMuseq" test_monomeRecording_toMuseq
    ]

test_monomeRecording_toMuseq :: Test
test_monomeRecording_toMuseq = TestCase $ do
  let r = Recording
        { _recordingStart = 100
        , _recordingEnd = Just 200
        , _recordingData = map (uncurry Observation)
            [ ( Time 150,
                ScAction_Free { _actionSynthDefEnum = Zot
                              , _actionSynthName = "a" } )
            , ( Time 100,
                ScAction_New { _actionSynthDefEnum = Zot
                             , _actionSynthName = "a"
                             , _actionScParams = M.singleton "freq" 333 } )
            ] }

  assertBool "" $ monomeRecording_toMuseq r ==
    ( Right $ Museq
      { _dur = 1
      , _sup = 1
      , _vec = V.fromList
               [ Event { _evLabel = "a"
                       , _evArc = (0, 1/2)
                       , _evData = M.singleton "freq" 333
                       } ] } )
