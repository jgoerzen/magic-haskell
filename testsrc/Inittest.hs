{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

module Inittest(tests) where
import Test.HUnit
import LDAP

test_base =
    let f 