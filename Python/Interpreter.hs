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

Interface to Python interpreter

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Interpreter (
                          py_initialize,
                          pyRun_SimpleString,
                          pyRun_String
                          )
where
import Python.Utils
import Python.Types
import Foreign
import Foreign.C.String
import Foreign.C

pyRun_SimpleString :: String -> IO ()
pyRun_SimpleString x = withCString x (\cs ->
                                          do cpyRun_SimpleString cs
                                             return ()
                                     )
    
pyRun_String :: String          -- ^ Command to run
             -> CInt            -- ^ Start Token (use 0)
             -> Maybe PyObject  -- ^ Globals (or Nothing for defaults)
             -> Maybe PyObject  -- ^ Locals (or Nothing for defaults)
             -> IO PyObject     -- ^ Result
pyRun_String command start globals locals =
    withCString command (\ccommand ->
        maybeWithPyObject globals (\cglobals ->
            maybeWithPyObject locals (\clocals ->
                cpyRun_String ccommand start cglobals clocals >>= fromCPyObject
                              )))

foreign import ccall unsafe "Python.h Py_Initialize"
  py_initialize :: IO ()

foreign import ccall unsafe "Python.h PyRun_SimpleString"
  cpyRun_SimpleString :: CString -> IO CInt

foreign import ccall unsafe "Python.h PyRun_String"
  cpyRun_String :: CString -> CInt -> Ptr CPyObject -> Ptr CPyObject -> IO (Ptr CPyObject)
