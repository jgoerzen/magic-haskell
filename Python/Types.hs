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
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

Interfaces to low-level Python types.  You should probably not use this module
directly.  You probably want 'Python.Objects' instead.

You'll only need this module directly if you are importing new functions
from the Python C API.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Types (
                     PyObject(..),
                     CPyObject
                    )
where

import Foreign
import Foreign.C
import Foreign.C.Types

type CPyObject = ()

-- | The type of Python objects.
newtype PyObject = PyObject (ForeignPtr CPyObject)



