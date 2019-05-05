module Test.TMain where

import Test.HUnit

import Test.TDispatch
import Test.THode


tests :: IO Counts
tests = runTestTT $ TestList [
    TestLabel "test_module_hode" test_module_hode
  , TestLabel "test_module_dispatch" test_module_dispatch
  ]
