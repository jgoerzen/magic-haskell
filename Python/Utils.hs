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

module Python.Utils (fromCPyObject
                    )
    where
import Python.Types
import Foreign.C.Types
import Foreign.C
import Foreign

fromCPyObject :: Ptr CPyObject -> IO PyObject
fromCPyObject po =
    do fp <- newForeignPtr py_decref po
       return $ PyObject fp


foreign import ccall "glue.h &hspy_decref"
 py_decref :: FunPtr (Ptr CPyObject -> IO ())
