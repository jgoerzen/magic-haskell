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
                          pyRun_SimpleString
                          )
where

pyRun_SimpleString :: String -> IO PyObject
pyRun_SimpleString x = withCString x (cpyRun_SimpleString >>= fromCPyObject)
    

foreign import ccall unsafe "Python.h Py_Initialize"
  py_initialize :: IO ()