{-# OPTIONS -fallow-overlapping-instances #-}
{- arch-tag: Object tests main file
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

module Objectstest(tests) where
import HUnit
import Python.Objects
import Foreign.C.Types
import Python.Types
import Data.List
import Python.Interpreter

f msg inp code exp = TestLabel msg $ TestCase $ do pyo <- toPyObject inp
                                                   r <- code pyo
                                                   exp @=? r

test_base =
    [
     f "showPyObject" (5::CInt) showPyObject "<type 'int'>: 5"
    ]

test_lists =
    [
     f "empty" ([]::[CInt]) fromPyObject ([]::[CInt])
    ,f "repr empty" ([]::[CInt]) reprOf "[]"
    ,f "some cints" [1::CInt, 2, 3] fromPyObject [1::CInt, 2, 3]
    ,f "some cints repr" [1::CInt, 2, 3] reprOf "[1, 2, 3]"
    ,f "strings" ["foo", "bar"] fromPyObject ["foo", "bar"]
    ,f "strings repr" ["foo", "bar"] reprOf "['foo', 'bar']"
    ]

test_al =
    [
     f "emptypyo" ([]::[(PyObject, PyObject)]) fromPyObject 
       ([]::[(PyObject, PyObject)])
    ,f "cint to cint" [(1::CInt, 2::CInt), (3, 4)] 
           (\x -> fromPyObject x >>= return . sort)
           [(1::CInt, 2::CInt), (3, 4)]
    ]
       
test_functions =
    [
     f "typestr" (5::CInt) (\x -> typeOf x >>= strOf) "<type 'int'>"
    ,f "repr" ["foo", "bar"] reprOf "['foo', 'bar']"
    ]

test_strings = 
    [
     f "empty" ([]::String) fromPyObject ([]::String)
    ,f "basic" "foo" fromPyObject "foo"
    ,f "dquotes" "foo\"" fromPyObject "foo\""
    ,f "squotes" "foo'" fromPyObject "foo'"
    ,f "embedded null" "foo\0bar" fromPyObject "foo\0bar"
    ,f "null only" "\0" fromPyObject "\0"
    ,f "quotes" "\"'\"" fromPyObject "\"'\""
    ]

test_ints =
    [
     f "0L" (0::CLong) fromPyObject (0::CLong)
    ,f "-5L" (-5::CLong) fromPyObject (-5::CLong)
    ,f "5L" (5::CLong) fromPyObject (5::CLong)
    ,f "max long" (maxBound::CLong) fromPyObject (maxBound::CLong)
    ,f "min long" (minBound::CLong) fromPyObject (minBound::CLong)
    ,f "0i" (0::CInt) fromPyObject (0::CInt)
    ,f "-5i" (-5::CInt) fromPyObject (-5::CInt)
    ,f "5i" (5::CInt) fromPyObject (5::CInt)
    ,f "min int" (minBound::CInt) fromPyObject (minBound::CInt)
    ,f "max int" (maxBound::CInt) fromPyObject (maxBound::CInt)
    ,f "long/int" (12345::CLong) fromPyObject (12345::CInt)
    ,f "int/long" (12354::CInt) fromPyObject (12354::CInt)
    ,f "repr max" (maxBound::CLong) reprOf (show (maxBound::CLong))
    ,f "str min" (minBound::CLong) strOf (show (minBound::CLong))
    ]

test_longs =
    [
     f "0" (0::Integer) fromPyObject (0::Integer)
    ,f "-5" (-5::Integer) fromPyObject (-5::Integer)
    ,f "5" (5::Integer) fromPyObject (5::Integer)
    ,f "2^384" ((2 ^ 384)::Integer) fromPyObject ((2 ^ 384)::Integer)
    ,f "2^384*-1" (( 2 ^ 384 * (-1))::Integer) fromPyObject ((2 ^ 384 * (-1))::Integer)
    ,f "str 2^384" ((2 ^ 384)::Integer) strOf (show ((2 ^ 384)::Integer))
    ]
       
test_doubles =
    [
     f "0" (0::CDouble) fromPyObject (0::CDouble)
    ,f "-5" (-5::CDouble) fromPyObject (-5::CDouble)
    ,f "5.1234" (5.1234::CDouble) fromPyObject (5.1234::CDouble)
    ,f "str 5.1234" (5.1234::CDouble) strOf "5.1234"
    ,f "2^384" ((2^384)::CDouble) fromPyObject ((2^384)::CDouble)
    ,f "2^384*-1" ((2^384 * (-1)::CDouble)) fromPyObject ((2^384 * (-1)::CDouble))
    ,f "1/(2^384)" ((1 / (2 ^ 384))::CDouble) fromPyObject
       ((1 / (2 ^ 384))::CDouble)
    ]

test_call =
    [
     TestCase $ do func <- pyRun_String "repr" Py_eval_input []
                   r <- pyObject_CallHs func [5::Integer] ([]::[(String, String)])
                   "5L" @=? r
    ]

test_dir =
    [
     TestCase $ do dv <- toPyObject ([]::String) >>= dirPyObject
                   assertBool "replace" $ "replace" `elem` dv
                   assertBool "rindex" $ "rindex" `elem` dv
    ]
                
tests = TestList [TestLabel "base" (TestList test_base),
                  TestLabel "lists/tuples" (TestList test_lists),
                  TestLabel "al" (TestList test_al),
                  TestLabel "functions" (TestList test_functions),
                  TestLabel "strings" (TestList test_strings),
                  TestLabel "ints" (TestList test_ints),
                  TestLabel "longs" (TestList test_longs),
                  TestLabel "doubles" (TestList test_doubles),
                  TestLabel "dir" (TestList test_dir),
                  TestLabel "call" (TestList test_call)
                 ]