{-# OPTIONS -fallow-overlapping-instances #-}
{- arch-tag: GZip tests main file
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

module GZiptest(tests) where
import HUnit
import Python.Exceptions
import MissingPy.FileArchive.GZip
import Data.List
import MissingH.IO.HVIO
import System.IO
import System.IO.Error
import Testutil
import System.Directory
import qualified Control.Exception

finally = Control.Exception.finally

f fn exp = TestCase $ do gzf <- openGz ("testsrc/gzfiles/" ++ fn) ReadMode 9
                         c <- vGetContents gzf
                         exp @=? c
                         vClose gzf
                             

test_gunzip =
    [
     f "t1.gz" "Test 1"
     --    , "t1bad has errors
    ,f "t2.gz" "Test 1Test 2"
    ,TestCase $ do gzf <- openGz "testsrc/gzfiles/zeros.gz" ReadMode 1
                   c <- vGetContents gzf
                   10485760 @=? length c
                   vClose gzf
    --,f "zeros.gz" (replicate 10485760 '\0')
    ]

test_gzip = TestCase $ 
    handlePy (\e -> do e2 <- formatException e
                       fail $ show e2
                ) $
    do gzf <- openGz "testsrc/gzfiles/deleteme.gz" ReadWriteMode 9
       finally (do vPutStr gzf "Test 2\n"
                   vSeek gzf AbsoluteSeek 7
                   vFlush gzf
                   vPutStr gzf "Test 3\n"
                   vPutStr gzf (replicate 1048576 't')
                   vPutChar gzf '\n'
                   vClose gzf
                   gzf2 <- openGz "testsrc/gzfiles/deleteme.gz" ReadMode 9
                   vGetLine gzf2 >>= (@=? "Test2")
                   vGetLine gzf2 >>= (@=? "Test3")
                   vRewind gzf2
                   c <- vGetContents gzf2
                   ("Test 2\nTest 3\n" ++ (replicate 1048576 't') ++ "\n") 
                      @=? c
                   assertRaises "eof" (Control.Exception.IOException $ mkIOError eofErrorType "" Nothing Nothing) (vGetLine gzf2)
                   vClose gzf2
               ) (removeFile "testsrc/gzfiles/deleteme.gz")
                   

tests = TestList [TestLabel "gzip" test_gzip,
                  TestLabel "gunzip" (TestList test_gunzip)

                 ]