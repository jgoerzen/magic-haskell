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

f fn exp = TestLabel fn $ TestCase $ 
                      do bzf <- openBz2 ("testsrc/bz2files/" ++ fn) ReadMode 9
                         c <- vGetContents bzf
                         exp @=? c
                         vClose bzf
                             

test_bunzip2 =
    [
     f "t1.bz2" "Test 1"
    ,f "t2.bz2" "Test 1Test 2"
    ,TestCase $ do bzf <- openBz2 "testsrc/bz2files/zeros.bz2" ReadMode 1
                   c <- vGetContents bzf
                   10485760 @=? length c
                   vClose bzf
    ,f "zeros.gz" (replicate 10485760 '\0')
    ]

test_bzip2 = TestCase $ 
    handlePy exc2ioerror $
    do bzf <- openBz2 "testsrc/bz2files/deleteme.bz2" ReadWriteMode 9
       finally (do vPutStr bzf "Test 1\n"
                   vSeek bzf AbsoluteSeek 5
                   vPutChar bzf '2'
                   vRewind bzf
                   vGetLine bzf >>= (@=?) "Test 2"
                   vSeek bzf AbsoluteSeek 7
                   vFlush bzf
                   vPutStr bzf "Test 3\n"
                   vPutStr bzf (replicate 1048576 't')
                   vPutChar bzf '\n'
                   vClose bzf
                   bzf2 <- openBz2 "testsrc/bz2files/deleteme.bz2" ReadMode 9
                   vGetLine bzf2 >>= (@=? "Test 2")
                   vGetLine bzf2 >>= (@=? "Test 3")
                   vRewind bzf2
                   c <- vGetContents bzf2
                   ("Test 2\nTest 3\n" ++ (replicate 1048576 't') ++ "\n") 
                      @=? c
                   assertRaises "eof" (Control.Exception.IOException $ mkIOError eofErrorType "" Nothing Nothing) (vGetLine bzf2)
                   vClose bzf2
               ) (removeFile "testsrc/bz2files/deleteme.bz2")
                   

tests = TestList [TestLabel "bzip2" test_bzip2,
                  TestLabel "bunzip2" (TestList test_bunzip2)

                 ]