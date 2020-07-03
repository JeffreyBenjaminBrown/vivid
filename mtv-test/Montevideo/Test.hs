module Montevideo.Test where

import Test.HUnit

import Montevideo.Test.Dispatch
import Montevideo.Test.Util
import Montevideo.Test.Monome.Misc
import Montevideo.Test.Monome.Sustain
import Montevideo.Test.Monome.Windows
import Montevideo.Test.Monome.JI


tests :: IO Counts
tests = runTestTT $ TestList [
    Montevideo.Test.Dispatch.tests
  , Montevideo.Test.Util.tests
  , Montevideo.Test.Monome.Misc.tests
  , Montevideo.Test.Monome.Windows.tests
  , Montevideo.Test.Monome.Sustain.tests
  , Montevideo.Test.Monome.JI.tests
  ]
