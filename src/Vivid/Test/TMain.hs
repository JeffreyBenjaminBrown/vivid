module Vivid.Test.TMain where

import Test.HUnit

import Vivid.Test.TDispatch
import Vivid.Test.THode


tests :: IO Counts
tests = runTestTT $ TestList [
    TestLabel "test_module_hode" test_module_hode
  , TestLabel "test_module_dispatch" test_module_dispatch
  ]
