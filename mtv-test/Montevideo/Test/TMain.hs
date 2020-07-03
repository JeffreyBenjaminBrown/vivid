module Montevideo.Test.TMain where

import Test.HUnit

import Montevideo.Test.TDispatch
import Montevideo.Test.TUtil
import Montevideo.Monome.Test.Misc
import Montevideo.Monome.Test.Sustain
import Montevideo.Monome.Test.Windows
import Montevideo.Monome.Test.JI


tests :: IO Counts
tests = runTestTT $ TestList [
    test_module_dispatch
  , test_module_util
  , Montevideo.Monome.Test.Misc.tests
  , Montevideo.Monome.Test.Windows.tests
  , Montevideo.Monome.Test.Sustain.tests
  , Montevideo.Monome.Test.JI.tests
  ]
