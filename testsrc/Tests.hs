{- arch-tag: Tests main file
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

module Tests(tests) where
import Test.HUnit
import qualified Inittest

test1 = TestCase ("x" @=? "x")

tests = TestList [TestLabel "test1" test1,
                  TestLabel "init" Inittest.tests
                 ]



