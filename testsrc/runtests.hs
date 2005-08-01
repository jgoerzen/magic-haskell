{- arch-tag: Test runner
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>
-}

module Main where 

import Test.HUnit
import Tests

main = runTestTT tests

