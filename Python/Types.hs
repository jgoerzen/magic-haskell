{- arch-tag: Python types
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
   Module     : Python.Types
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

Interfaces to low-level Python types

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Types (
                     PyObject
                    )
where

import Foreign
import Foreign.C
import Foreign.C.Types

type CPyObject = ()

-- | The type of Python objects.
newtype PyObject = PyObject (ForeignPtr CPyObject)

{- | Members of this class can be converted from a Haskell type
to a Python object. -}
class (Show a, Eq a, Ord a) => ToPyObject a where
    toPyObject :: a -> IO PyObject

{- | Members of this class can be derived from a Python object. -}
{-
class (Show a, Eq, a, Ord a) => FromPyObject a where
    fromPyObject :: PyObject -> IO a
-}

instance ToPyObject CInt where
    toPyObject x = 
        withCString "i" $ \cstr ->
            do po <- py_buildvalue cstr x
               fp <- newForeignPtr py_decref po
               return $ PyObject fp

----------------------------------------------------------------------
-- C imports

foreign import ccall unsafe "glue.h Py_BuildValue"
 py_buildvalue :: CString -> CInt -> IO (Ptr CPyObject)

foreign import ccall "glue.h &hspy_decref"
 py_decref :: FunPtr (Ptr CPyObject -> IO ())

