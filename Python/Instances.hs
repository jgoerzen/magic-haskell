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
   Module     : Python.Types
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

Python type instances

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Instances (
                        )
where
import Python.Types
import Python.Utils
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Data.List

-- FIXME: ERROR CHECKING!

-- | Lists from a PyObject
instance PyObjectConv [PyObject] where
    toPyObject mainlist =
        do l <- pyList_New 0
           mapM_ (\pyo -> withPyObject pyo (pyList_Append l)) mainlist
           fromCPyObject l

instance PyObjectConv CString where
   toPyObject x = 
            withCString "s" $ \cstr ->
                py_buildvalues cstr x >>= fromCPyObject

instance PyObjectConv String where
    toPyObject x = withCString x toPyObject

instance PyObjectConv CInt where
    toPyObject x = 
        withCString "i" $ \cstr ->
            do po <- py_buildvalue cstr x
               fromCPyObject po


----------------------------------------------------------------------
-- C imports

foreign import ccall unsafe "glue.h Py_BuildValue"
 py_buildvalue :: CString -> CInt -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h Py_BuildValue"
 py_buildvalues :: CString -> CString -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyList_New"
 pyList_New :: CInt -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyList_Append"
 pyList_Append :: Ptr CPyObject -> Ptr CPyObject -> IO CInt