{-# OPTIONS -fallow-overlapping-instances #-}
{- arch-tag: Python interpreter module
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
   Module     : Python.Interpreter
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Interface to Python interpreter

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Interpreter (
                          py_initialize,
                          pyRun_SimpleString,
                          pyRun_String,
                          pyRun_StringHs,
                          StartFrom(..)
                          )
where

#include <Python.h>

import Python.Utils
import Python.Objects
import Python.Types
import Foreign
import Foreign.C.String
import Foreign.C

data StartFrom = Py_eval_input
               | Py_file_input
               | Py_single_input

sf2c :: StartFrom -> CInt
sf2c Py_eval_input = #const Py_eval_input
sf2c Py_file_input = #const Py_file_input
sf2c Py_single_input = #const Py_single_input

pyRun_SimpleString :: String -> IO ()
pyRun_SimpleString x = withCString x (\cs ->
                                          do cpyRun_SimpleString cs
                                             return ()
                                     )

-- | Like 'pyRun_String', but take more Haskellish args and results.
pyRun_StringHs :: (ToPyObject a, ToPyObject b, FromPyObject c) =>
                  String        -- ^ Command to run
               -> StartFrom     -- ^ Start token
               -> [(String, a)] -- ^ Globals (may be empty)
               -> [(String, b)] -- ^ Locals (may be empty)
               -> IO c
pyRun_StringHs cmd start globals locals =
    let conv (k, v) = do v1 <- toPyObject v
                         return (k, v1)
        in do
           rglobals <- mapM conv globals
           rlocals <- mapM conv locals
           pyRun_String cmd start rglobals rlocals >>= fromPyObject
    
-- | Run some code in Python.
pyRun_String :: String          -- ^ Command to run
             -> StartFrom       -- ^ Start Token
             -> [(String, PyObject)] -- ^ Globals (may be empty)
             -> [(String, PyObject)] -- ^ Locals (may be empty)
             -> IO PyObject     -- ^ Result
pyRun_String command startfrom xglobals xlocals =
    let cstart = sf2c startfrom
        in do dobj <- getDefaultGlobals
              d <- fromPyObject dobj
              rlocals <- toPyObject (d ++ xlocals)
              rglobals <- toPyObject (d ++ xglobals)
              withCString command (\ccommand ->
               withPyObject rglobals (\cglobals ->
                withPyObject rlocals (\clocals ->
                 cpyRun_String ccommand cstart cglobals clocals >>= fromCPyObject
                              )))

foreign import ccall unsafe "Python.h Py_Initialize"
  py_initialize :: IO ()

foreign import ccall unsafe "Python.h PyRun_SimpleString"
  cpyRun_SimpleString :: CString -> IO CInt

foreign import ccall unsafe "Python.h PyRun_String"
  cpyRun_String :: CString -> CInt -> Ptr CPyObject -> Ptr CPyObject -> IO (Ptr CPyObject)
