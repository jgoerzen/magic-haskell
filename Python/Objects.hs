{-# OPTIONS -fallow-overlapping-instances #-}

{- arch-tag: Python type instances
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
   Module     : Python.Objects
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

Python type instances and object utilities

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Objects (
                       ToPyObject(..),
                       FromPyObject(..),
                       PyObject,
                       typeOf,
                       strOf,
                       reprOf,
                       showPyObject
                      )
where
import Python.Types
import Python.Utils
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.List

{- | Members of this class can be converted from a Haskell type
to a Python object. -}
class ToPyObject a where
    toPyObject :: a -> IO PyObject

{- | Members of this class can be derived from a Python object. -}
class FromPyObject a where
    fromPyObject :: PyObject -> IO a

----------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------
{- | Gets the type of a Python object.  Same as type(x) in Python. -}
typeOf :: PyObject -> IO PyObject
typeOf x = withPyObject x (\pyo -> pyObject_Type pyo >>= fromCPyObject)
                                      
{- | Gets a string representation of a Python object.  Same 
as str(x) in Python. -}
strOf :: PyObject -> IO String
strOf x = withPyObject x 
            (\pyo -> pyObject_Str pyo >>= fromCPyObject >>= fromPyObject)

{- | Gets the Python representation of a Python object.
Same as repr(x) in Python. -}
reprOf :: PyObject -> IO String
reprOf x = withPyObject x
             (\pyo -> pyObject_Repr pyo >>= fromCPyObject >>= fromPyObject)

{- | Displays a Python object and its type. -}
showPyObject :: PyObject -> IO String
showPyObject x = do typestr <- typeOf x >>= strOf
                    contentstr <- strOf x
                    return $ typestr ++ ": " ++ contentstr

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

-- FIXME: ERROR CHECKING!

-- | Lists from a PyObject
instance ToPyObject [PyObject] where
    toPyObject mainlist =
        do l <- pyList_New 0
           mapM_ (\pyo -> withPyObject pyo (pyList_Append l)) mainlist
           fromCPyObject l

-- | Dicts from ALs
instance ToPyObject [(PyObject, PyObject)] where
    toPyObject mainlist =
        do d <- pyDict_New
           mapM_ (setitem d) mainlist
           fromCPyObject d
        where setitem l (key, value) =
                  withPyObject key (\keyo ->
                      withPyObject value (\valueo ->
                          pyDict_SetItem l keyo valueo))
                                       
-- | Dicts from Haskell objects
instance (ToPyObject a, ToPyObject b) => ToPyObject [(a, b)] where
    toPyObject mainlist =
        let convone (i1, i2) = do oi1 <- toPyObject i1
                                  oi2 <- toPyObject i2
                                  return (oi1, oi2)
        in do newl <- mapM convone mainlist
              toPyObject newl

instance ToPyObject CString where
   toPyObject x = 
            withCString "s" $ \cstr ->
                py_buildvalues cstr x >>= fromCPyObject

instance ToPyObject String where
    toPyObject x = withCString x toPyObject

instance FromPyObject String where
    fromPyObject x = withPyObject x (\po ->
        alloca (\lenptr ->
           alloca (\strptr ->
            do pyString_AsStringAndSize po strptr lenptr
               len <- peek lenptr
               cstr <- peek strptr
               peekCStringLen (cstr, (fromIntegral) len)
                  )
               )
                                    )
           

instance ToPyObject CInt where
    toPyObject x = 
        withCString "i" $ \cstr ->
            do po <- py_buildvalue cstr x
               fromCPyObject po

instance ToPyObject CLong where
    toPyObject x =
        pyInt_FromLong x >>= fromCPyObject

instance FromPyObject CLong where
    fromPyObject po = 
        withPyObject po pyInt_AsLong

{-
-- | Lists from anything else
instance ToPyObject a => ToPyObject [a] where
    toPyObject mainlist = 
        do newlist <- mapM toPyObject mainlist
           toPyObject newlist
-}

----------------------------------------------------------------------
-- C imports
----------------------------------------------------------------------

foreign import ccall unsafe "glue.h Py_BuildValue"
 py_buildvalue :: CString -> CInt -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h Py_BuildValue"
 py_buildvalues :: CString -> CString -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyList_New"
 pyList_New :: CInt -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyList_Append"
 pyList_Append :: Ptr CPyObject -> Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyDict_New"
 pyDict_New :: IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyDict_SetItem"
 pyDict_SetItem :: Ptr CPyObject -> Ptr CPyObject -> Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyInt_FromLong"
 pyInt_FromLong :: CLong -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyInt_AsLong"
 pyInt_AsLong :: Ptr CPyObject -> IO CLong

foreign import ccall unsafe "glue.h PyObject_Str"
 pyObject_Str :: Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyObject_Repr"
 pyObject_Repr :: Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyObject_Type"
 pyObject_Type :: Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyString_AsStringAndSize"
 pyString_AsStringAndSize :: Ptr CPyObject -> Ptr CString -> Ptr CInt -> IO ()
