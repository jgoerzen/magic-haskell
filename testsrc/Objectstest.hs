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
       

tests = TestList [TestLabel "base" (TestList test_base),
                  TestLabel "lists/tuples" (TestList test_lists),
                  TestLabel "al" (TestList test_al)
                 ]