{- arch-tag: Python low-level utilities
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
   Module     : Python.Utils
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

Python low-level utilities

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Utils (fromCPyObject,
                     withPyObject,
                     raisePyException,
                     maybeWithPyObject
                    )
    where
import Python.Types
import Foreign.C.Types
import Foreign.C
import Foreign
import Foreign.Ptr
import Foreign.Marshal.Array

{- | Convert a Ptr 'CPyObject' to a 'PyObject'. -}
fromCPyObject :: Ptr CPyObject -> IO PyObject
fromCPyObject po =
    if po == nullPtr
       then raisePyException
       else do fp <- newForeignPtr py_decref po
               return $ PyObject fp

{- | Called when a Python exception has been detected.  It will raise
the exception in Haskell. -}
raisePyException :: IO a
raisePyException =
{-
    do cpy <- getexc
       let (exc, val, tb) = cpy
       pyErr_Print
-}
    do pyErr_Print
       fail "Python Error!"
    where getexc = do cexc <- hspy_getexc
                      exc <- peekArray 3 cexc
                      exc2 <- mapM fromCPyObject exc
                      case exc2 of
                               [x, y, z] -> return (x, y, z)
                               _ -> fail "Got unexpected number of elements"
    
{- | Uses a 'PyObject' in a function that needs Ptr 'CPyObject'. -}
withPyObject :: PyObject -> (Ptr CPyObject -> IO b) -> IO b
withPyObject (PyObject x) = withForeignPtr x    

{- | Same as 'withPyObject', but uses nullPtr if the input is Nothing. -}
maybeWithPyObject :: Maybe PyObject -> (Ptr CPyObject -> IO b) -> IO b
maybeWithPyObject Nothing func = func nullPtr
maybeWithPyObject (Just x) y = withPyObject x y


foreign import ccall "glue.h &hspy_decref"
 py_decref :: FunPtr (Ptr CPyObject -> IO ())

foreign import ccall unsafe "glue.h hspy_getexc"
 hspy_getexc :: IO (Ptr (Ptr CPyObject))

foreign import ccall unsafe "glue.h PyErr_Print"
 pyErr_Print :: IO ()


