module Montevideo.Test where

import Test.HUnit

import Montevideo.Test.Dispatch
import Montevideo.Test.Util
import Montevideo.Monome.Test.Misc
import Montevideo.Monome.Test.Sustain
import Montevideo.Monome.Test.Windows
import Montevideo.Monome.Test.JI


tests :: IO Counts
tests = runTestTT $ TestList [
    Montevideo.Test.Dispatch.tests
  , Montevideo.Test.Util.tests
  , Montevideo.Monome.Test.Misc.tests
  , Montevideo.Monome.Test.Windows.tests
  , Montevideo.Monome.Test.Sustain.tests
  , Montevideo.Monome.Test.JI.tests
  ]
