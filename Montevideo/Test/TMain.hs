module Montevideo.Test.TMain where

import Test.HUnit

--import Montevideo.Test.THode
import Montevideo.Test.TDispatch
import Montevideo.Test.TUtil


tests :: IO Counts
tests = runTestTT $ TestList [
    TestLabel "test_module_dispatch" test_module_dispatch
--  , TestLabel "test_module_hode" test_module_hode
  , TestLabel "test_module_util" test_module_util
  ]
