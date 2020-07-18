module Montevideo.Test where

import Test.HUnit

import Montevideo.Test.Dispatch
import Montevideo.Test.Util
import Montevideo.Monome.Test.Misc
import Montevideo.Monome.Test.Sustain
import Montevideo.Monome.Test.Windows
import Montevideo.Monome.Test.JI
import Montevideo.Monome.Test.EdoMath


allTests :: IO Counts
allTests = runTestTT $ TestList [
    Montevideo.Test.Dispatch.tests
  , Montevideo.Test.Util.tests
  , Montevideo.Monome.Test.EdoMath.tests
  , Montevideo.Monome.Test.JI.tests
  , Montevideo.Monome.Test.Misc.tests
  , Montevideo.Monome.Test.Sustain.tests
  , Montevideo.Monome.Test.Windows.tests
  ]
