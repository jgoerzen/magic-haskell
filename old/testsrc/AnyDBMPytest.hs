{- arch-tag: AnyDBM tests for Python
Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module AnyDBMPytest(tests) where
import Test.HUnit
import MissingPy.AnyDBM
import AnyDBMtest hiding (tests)

testmod m = generic_persist_test (return ())
             (\f -> openSpecificDBM m ("testtmp/" ++ m) DBM_ReadWriteCreate)
            ++
            generic_test (return ())
             (\f -> openSpecificDBM m ("testtmp/" ++ m) DBM_ReadWriteCreate)

tests = TestList [TestLabel "anydbm" (TestList $ testmod "anydbm")
                 ,TestLabel "dbhash" (TestList $ testmod "dbhash")
--                 ,TestLabel "dbm" (TestList $ testmod "dbm")
                 ,TestLabel "dumbdbm" (TestList $ testmod "dumbdbm")
                 ,TestLabel "gdbm" (TestList $ testmod "gdbm")
                 ]




