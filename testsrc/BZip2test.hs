{-# OPTIONS -fallow-overlapping-instances #-}
{- arch-tag: BZip2 tests main file
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

module BZip2test(tests) where
import HUnit
import Python.Exceptions
import MissingPy.FileArchive.BZip2
import Data.List
import MissingH.IO.HVIO
import System.IO
import System.IO.Error
import Testutil
import System.Directory
import qualified Control.Exception

finally = Control.Exception.finally

f' mf fn exp = TestLabel fn $ TestCase $ handlePy exc2ioerror $
                      do bzf <- openBz2 ("testsrc/bz2files/" ++ fn) ReadMode 9
                         c <- vGetContents bzf
                         mf exp c
                         vClose bzf
                     
f fn exp = f' (@=?) fn exp
f2 fn exp = f' nodisptest fn exp

-- @=? loads the whole thing into memory.  ick.  This is much better.
nodisptest a b =
    if a == b
       then return ()
       else assertFailure "Data mismatch"

test_bunzip2 =
    [
     f "t1.bz2" "Test 1"
    ,f "t2.bz2" "Test 1Test 2"
    ,f "empty.bz2" ""
    ,TestCase $ do bzf <- openBz2 "testsrc/bz2files/zeros.bz2" ReadMode 1
                   c <- vGetContents bzf
                   10485760 @=? length c
                   vClose bzf
    ,f2 "zeros.bz2" (replicate 10485760 '\0')
    ]

test_bzip2 = TestCase $ 
    handlePy exc2ioerror $
    do bzf <- openBz2 "testsrc/bz2files/deleteme.bz2" WriteMode 9
       finally (do vPutStr bzf "Test 2\n"
                   vFlush bzf
                   vPutStr bzf "Test 3\n"
                   vPutStr bzf (replicate 1048576 't')
                   putStr "\n\n"
                   print "1"
                   vPutChar bzf '\n'
                   print "2"
                   vClose bzf
                   bzf2 <- openBz2 "testsrc/bz2files/deleteme.bz2" ReadMode 9
                   print "3"
                   vGetLine bzf2 >>= (@=? "Test 2")
                   vGetLine bzf2 >>= (@=? "Test 3")
                   vGetLine bzf2
                   assertRaises "eof" (Control.Exception.IOException $ mkIOError eofErrorType "" Nothing Nothing) (vGetLine bzf2)

                   vRewind bzf2
                   print "4"
                   c <- vGetContents bzf2
                   print "5"
                   ("Test 2\nTest 3\n" ++ (replicate 1048576 't') ++ "\n") 
                      @=? c
                   print "6"
                   --assertRaises "eof" (Control.Exception.IOException $ mkIOError eofErrorType "" Nothing Nothing) (vGetLine bzf2)
                   print "7"
                   vClose bzf2
               ) (removeFile "testsrc/bz2files/deleteme.bz2")
                   

tests = TestList [TestLabel "bzip2" test_bzip2,
                  TestLabel "bunzip2" (TestList test_bunzip2)

                 ]