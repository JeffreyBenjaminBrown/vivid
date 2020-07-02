module Montevideo.Monome.Test where

import Test.HUnit

import qualified Montevideo.Monome.Test.Misc
import qualified Montevideo.Monome.Test.Sustain
import qualified Montevideo.Monome.Test.Windows
import qualified Montevideo.Monome.Test.JI


doTests :: IO Counts
doTests = runTestTT $ TestList
  [ Montevideo.Monome.Test.Misc.tests
  , Montevideo.Monome.Test.Windows.tests
  , Montevideo.Monome.Test.Sustain.tests
  , Montevideo.Monome.Test.JI.tests
  ]
