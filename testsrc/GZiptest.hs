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
import MissingPy.FileArchive.GZip
import Data.List
import MissingH.IO.HVIO
import System.IO
import qualified Control.Exception

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
tests = TestList [TestLabel "gunzip" (TestList test_gunzip)
                 ]