{- arch-tag: Python foreign imports
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
   Module     : Python.ForeignImports
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Low-level C interface.  Handles foreign imports for everything but 
"Python.Exceptions.ExcTypes".

This is not to be exposed outside this library.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.ForeignImports where
import Foreign.C.Types
import Foreign
import Python.Types
import Foreign.C.String
import System.IO

#include "glue.h"

sf2c :: StartFrom -> CInt
sf2c Py_eval_input = #const Py_eval_input
sf2c Py_file_input = #const Py_file_input
sf2c Py_single_input = #const Py_single_input

foreign import ccall unsafe "glue.h PyErr_GivenExceptionMatches"
 pyErr_GivenExceptionMatches :: Ptr CPyObject -> Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h Py_Initialize"
  cpy_initialize :: IO ()

foreign import ccall unsafe "glue.h PyRun_SimpleString"
  cpyRun_SimpleString :: CString -> IO CInt

foreign import ccall unsafe "glue.h PyRun_String"
  cpyRun_String :: CString -> CInt -> Ptr CPyObject -> Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyImport_ImportModuleEx"
 cpyImport_ImportModuleEx :: CString -> Ptr CPyObject -> Ptr CPyObject -> Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyDict_SetItemString"
 pyDict_SetItemString :: Ptr CPyObject -> CString -> Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyImport_GetModuleDict"
 pyImport_GetModuleDict :: IO (Ptr CPyObject)


foreign import ccall unsafe "glue.h PyString_FromStringAndSize"
 pyString_FromStringAndSize :: CString -> CInt -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyInt_FromLong"
 pyInt_FromLong :: CLong -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyInt_AsLong"
 pyInt_AsLong :: Ptr CPyObject -> IO CLong

foreign import ccall unsafe "glue.h PyLong_FromString"
 pyLong_FromString :: CString -> Ptr CString -> CInt -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyList_New"
 pyList_New :: CInt -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyList_Append"
 pyList_Append :: Ptr CPyObject -> Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyDict_New"
 pyDict_New :: IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyObject_SetItem"
 pyObject_SetItem :: Ptr CPyObject -> Ptr CPyObject -> Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyObject_Repr"
 pyObject_Repr :: Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyObject_Type"
 pyObject_Type :: Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyString_AsStringAndSize"
 pyString_AsStringAndSize :: Ptr CPyObject -> Ptr CString -> Ptr CInt -> IO ()

foreign import ccall unsafe "glue.h hspy_list_check"
 pyList_Check :: Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h hspy_tuple_check"
 pyTuple_Check :: Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyList_Size"
 pyList_Size :: Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyTuple_Size"
 pyTuple_Size :: Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyList_GetItem"
 pyList_GetItem :: Ptr CPyObject -> CInt -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyTuple_GetItem"
 pyTuple_GetItem :: Ptr CPyObject -> CInt -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyMapping_Items"
 pyMapping_Items :: Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyFloat_FromDouble"
 pyFloat_FromDouble :: CDouble -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyFloat_AsDouble"
 pyFloat_AsDouble :: Ptr CPyObject -> IO CDouble

foreign import ccall unsafe "glue.h PyObject_Dir"
 pyObject_Dir :: Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall "glue.h PyObject_Call"
 cpyObject_Call :: Ptr CPyObject -> Ptr CPyObject -> Ptr CPyObject ->
                   IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyList_AsTuple"
 cpyList_AsTuple :: Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyObject_GetAttrString"
 pyObject_GetAttrString :: Ptr CPyObject -> CString -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyObject_HasAttrString"
 pyObject_HasAttrString :: Ptr CPyObject -> CString -> IO CInt

foreign import ccall unsafe "glue.h PyObject_SetAttrString"
 pyObject_SetAttrString :: Ptr CPyObject -> CString -> Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyObject_Str"
 pyObject_Str :: Ptr CPyObject -> IO (Ptr CPyObject)


foreign import ccall unsafe "glue.h PyModule_GetDict"
 cpyModule_GetDict :: Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall "glue.h &hspy_decref"
 py_decref :: FunPtr (Ptr CPyObject -> IO ())

foreign import ccall "glue.h hspy_incref"
 py_incref :: Ptr CPyObject -> IO ()

foreign import ccall unsafe "glue.h hspy_getexc"
 hspy_getexc :: IO (Ptr (Ptr CPyObject))

foreign import ccall unsafe "glue.h PyErr_Fetch"
 pyErr_Fetch :: Ptr (Ptr CPyObject) -> Ptr (Ptr CPyObject) -> Ptr (Ptr CPyObject) -> IO ()

foreign import ccall unsafe "glue.h PyErr_NormalizeException"
 pyErr_NormalizeException :: Ptr (Ptr CPyObject) -> Ptr (Ptr CPyObject) -> Ptr (Ptr CPyObject) -> IO ()

foreign import ccall unsafe "glue.h PyErr_Clear"
 pyErr_Clear :: IO ()

foreign import ccall unsafe "glue.h PyErr_Print"
 pyErr_Print :: IO ()

foreign import ccall unsafe "glue.h PyImport_AddModule"
 cpyImport_AddModule :: CString -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h hspy_none"
 cNone :: IO (Ptr CPyObject)
