{- arch-tag: Python low-level exception handling
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

{- |
   Module     : Python.Exceptions
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Python low-level exception handling

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Exceptions (-- * Types
                          PyException(..),
                          -- * General Catching
                          catchPy,
                          pyExceptions,
                          -- * Catching of specific Python exceptions
                          catchSpecificPy,
                          -- * Exception Object Operations
                          formatException,
                          doesExceptionMatch
                         )
where

import Python.Utils
import Foreign.C.Types
import Python.Objects
import Foreign
import Python.Types
import Data.Dynamic
import Data.Typeable
import Control.Exception
import Python.Interpreter

{- | Execute the given IO action.

If it raises a 'PyException', then execute the supplied handler and return
its return value.  Otherwise, process as normal. -}
catchPy :: IO a -> (PyException -> IO a) -> IO a
catchPy = catchDyn

{- | Like catchPy, but catches only instances of the Python class given
(see 'doesExceptionMatch'). -}
catchSpecificPy :: PyObject -> IO a -> (PyException -> IO a) -> IO a
catchSpecificPy pyo action handlerfunc =
    let handler e = do d <- doesExceptionMatch e pyo
                       if d
                          then handlerfunc e
                          else throwDyn e
        in catchPy action handler

{- | Useful as the first argument to catchJust, tryJust, or handleJust.
Return Nothing if the given exception is not a 'PyException', or 
the exception otherwise. -}
pyExceptions :: Exception -> Maybe PyException
pyExceptions exc = dynExceptions exc >>= fromDynamic

{- | When an exception is thrown, it is not immediately formatted.

This call will format it. -}
formatException :: PyException -> IO PyException
formatException e =
{-
    do fmt <- callByName "traceback.format_exception" 
                [excType e, excValue e, excTraceBack e] [] >>= fromPyObject
-}
    do ename <- strOf (excType e)
       evalue <- strOf (excValue e)
       let fmt = ename ++ ": " ++ evalue
       return $ e  {excFormatted = fmt}

{- | Returns true if the passed 'PyException' matches the given Python
exception class or one of its subclasses.  Standard Python exception classes
are given in 'Python.Exceptions.ExcTypes'. -}
doesExceptionMatch :: PyException -> PyObject -> IO Bool
doesExceptionMatch e pyo =
    withPyObject (excType e) (\ctyp ->
     withPyObject pyo (\cpo ->
      do r <- pyErr_GivenExceptionMatches ctyp cpo
         if r == 0
            then return False
            else return True
                      ))

foreign import ccall unsafe "glue.h PyErr_GivenExceptionMatches"
 pyErr_GivenExceptionMatches :: Ptr CPyObject -> Ptr CPyObject -> IO CInt