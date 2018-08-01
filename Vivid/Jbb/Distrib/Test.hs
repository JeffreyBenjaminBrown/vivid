module Vivid.Jbb.Distrib.Test (tests) where

import Test.HUnit

tests = runTestTT $ TestList
  [ TestLabel "dummy" dummy
  ]

dummy = TestCase $ do
  assertBool "dummy" True
