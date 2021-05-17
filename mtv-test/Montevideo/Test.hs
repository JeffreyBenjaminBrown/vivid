module Montevideo.Test where

import Test.HUnit

import Montevideo.JI.Thanos.Test
import Montevideo.Monome.Test.EdoMath
import Montevideo.Monome.Test.JI
import Montevideo.Monome.Test.Misc
import Montevideo.Monome.Test.Sustain
import Montevideo.Monome.Test.Types.Params
import Montevideo.Monome.Test.Windows
import Montevideo.Test.Dispatch
import Montevideo.Test.Recording
import Montevideo.Test.Util


allTests :: IO Counts
allTests = runTestTT $ TestList
  [ Montevideo.JI.Thanos.Test.tests
  , Montevideo.Monome.Test.EdoMath.tests
  , Montevideo.Monome.Test.JI.tests
  , Montevideo.Monome.Test.Misc.tests
  , Montevideo.Monome.Test.Sustain.tests
  , Montevideo.Monome.Test.Types.Params.tests
  , Montevideo.Monome.Test.Windows.tests
  , Montevideo.Test.Dispatch.tests
  , Montevideo.Test.Recording.tests
  , Montevideo.Test.Util.tests
  ]
