module Vivid.Jbb.Distrib.Test (tests) where

import Data.Vector as V

import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Distrib.Museq
import Test.HUnit

tests = runTestTT $ TestList
  [ TestLabel "museq" testMuseq
  ]

testMuseq = TestCase $ do
  assertBool "empty" $       museqIsValid $ Museq { _dur = 3, _vec = V.empty }
  assertBool "empty" $ not $ museqIsValid $ Museq { _dur = 0, _vec = V.empty }
  assertBool "empty" $       museqIsValid
    $ Museq { _dur = 1, _vec = V.fromList [(0, Wait 3)]}
  -- V.cons 3 $ V.empty
