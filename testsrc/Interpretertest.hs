{-# OPTIONS -fallow-overlapping-instances #-}
{- arch-tag: Interpreter tests main file
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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

module Interpretertest(tests) where
import HUnit
import Python.Interpreter
import Foreign.C.Types
import Python.Types
import Python.Instances

test_base =
    let f msg t = TestLabel msg $ TestCase t in
    [
     f "print" $ pyRun_SimpleString "print \"Hi from Python\\n\""
    ,f "longs" $ do pyo <- toPyObject (10::CLong)
                    newl <- fromPyObject pyo
                    (10::CLong) @=? newl
    ]

test_args =
    let f msg code inp exp = TestLabel msg $ TestCase $ 
                             do let testhdict = [("testval", inp)]
                                testpydict <- toPyObject testhdict
                                retobj <- pyRun_String code Py_eval_input Nothing (Just testpydict)
                                retval <- fromPyObject retobj
                                exp @=? retval
        in
        [
         f "addition" "testval + 3" (2::CLong) (5::CLong)
{-
         TestLabel "m1" $ TestCase $
                   do testpydict <- toPyObject [(5::CInt, 2::CInt)]
                      retobj <- pyRun_String "testval + 3" 0 Nothing (Just testpydict)
                      retval <- fromPyObject retobj
                      5 @=? retval
-}
        ]
        

tests = TestList [TestLabel "base" (TestList test_base),
                  TestLabel "args" (TestList test_args)]
