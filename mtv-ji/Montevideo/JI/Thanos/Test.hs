module Montevideo.JI.Thanos.Test where

import Test.HUnit
import Montevideo.JI.Thanos.Thanos


tests :: Test
tests = TestList [
    TestLabel "merk" merk
  ]

merk :: Test
merk = TestCase $ do
  assertBool "" False
