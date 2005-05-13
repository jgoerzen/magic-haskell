{- arch-tag: Python low-level exception definitions
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
   Module     : Python.Exceptions.ExcTypes
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Python low-level exception definitions

These are definitions of the built-in Python exception objects.  You can
use them with 'MissingPy.Python.Exceptions.doesExceptionMatch' and
'MissingPy.Python.Exceptions.catchSpecificPy'.

The meanings of these exceptions can be found at
<http://www.python.org/doc/current/lib/module-exceptions.html>.

Please note that windowsError is available only on Microsoft platforms.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Exceptions.ExcTypes
(pyMainException, 
standardError, arithmeticError, lookupError, assertionError, attributeError,
pyEOFError, environmentError, floatingPointError, pyIOError, importError,
indexError, keyError, keyboardInterrupt, memoryError, nameError,
notImplementedError, pyOSError, overflowError, referenceError, runtimeError,
syntaxError, systemError, systemExit, typeError, valueError,
#ifdef MS_WINDOWS
windowsError,
#endif
zeroDivisionError)
where
import Python.Types
import Python.Objects
import System.IO.Unsafe
import Python.Utils
import Foreign

#include <Python.h>

-- e :: Ptr (Ptr CPyObject) -> PyObject
{-# NOINLINE exctypes_internal_e #-}
exctypes_internal_e x =                   do p <- peek x
                                             fp <- newForeignPtr_ p
                                             return $ PyObject fp

-- | Exception in Python
pyMainException :: PyObject
pyMainException = unsafePerformIO $ exctypes_internal_e cException

standardError :: PyObject
standardError = unsafePerformIO $ exctypes_internal_e cStandardError

arithmeticError :: PyObject
arithmeticError = unsafePerformIO $ exctypes_internal_e cArithmeticError

lookupError :: PyObject
lookupError = unsafePerformIO $ exctypes_internal_e cLookupError

assertionError :: PyObject
assertionError = unsafePerformIO $ exctypes_internal_e cAssertionError

attributeError :: PyObject
attributeError = unsafePerformIO $ exctypes_internal_e cAttributeError

pyEOFError :: PyObject
pyEOFError = unsafePerformIO $ exctypes_internal_e cEOFError

environmentError :: PyObject
environmentError = unsafePerformIO $ exctypes_internal_e cEnvironmentError

floatingPointError :: PyObject
floatingPointError = unsafePerformIO $ exctypes_internal_e cFloatingPointError

pyIOError :: PyObject
pyIOError = unsafePerformIO $ exctypes_internal_e cStandardError

importError :: PyObject
importError = unsafePerformIO $ exctypes_internal_e cImportError

indexError :: PyObject
indexError = unsafePerformIO $ exctypes_internal_e cIndexError

keyError :: PyObject
keyError = unsafePerformIO $ exctypes_internal_e cKeyError

keyboardInterrupt :: PyObject
keyboardInterrupt = unsafePerformIO $ exctypes_internal_e cKeyboardInterrupt

memoryError :: PyObject
memoryError = unsafePerformIO $ exctypes_internal_e cMemoryError

nameError :: PyObject
nameError = unsafePerformIO $ exctypes_internal_e cNameError

notImplementedError :: PyObject
notImplementedError = unsafePerformIO $ exctypes_internal_e cNotImplementedError

pyOSError :: PyObject
pyOSError = unsafePerformIO $ exctypes_internal_e cOSError

overflowError :: PyObject
overflowError = unsafePerformIO $ exctypes_internal_e cOverflowError

referenceError :: PyObject
referenceError = unsafePerformIO $ exctypes_internal_e cReferenceError

runtimeError :: PyObject
runtimeError = unsafePerformIO $ exctypes_internal_e cRuntimeError

syntaxError :: PyObject
syntaxError = unsafePerformIO $ exctypes_internal_e cSyntaxError

systemError :: PyObject
systemError = unsafePerformIO $ exctypes_internal_e cSystemError

systemExit :: PyObject
systemExit = unsafePerformIO $ exctypes_internal_e cSystemExit

{-# NOINLINE typeError #-}
typeError :: PyObject
--typeError = unsafePerformIO $ exctypes_internal_e cTypeError
typeError = unsafePerformIO $
              do p <- cTypeError2
                 fp <- newForeignPtr_ p
                 return $ PyObject fp

valueError :: PyObject
valueError = unsafePerformIO $ exctypes_internal_e cValueError

#ifdef MS_WINDOWS
windowsError :: PyObject
windowsError = unsafePerformIO $ exctypes_internal_e cWindowsError
#endif

zeroDivisionError :: PyObject
zeroDivisionError = unsafePerformIO $ exctypes_internal_e cZeroDivisionError


foreign import ccall unsafe "&PyExc_Exception" cException :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_StandardError" cStandardError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_ArithmeticError" cArithmeticError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_LookupError" cLookupError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_AssertionError" cAssertionError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_AttributeError" cAttributeError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_EOFError" cEOFError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_EnvironmentError" cEnvironmentError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_FloatingPointError" cFloatingPointError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_IOError" cIOError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_ImportError" cImportError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_IndexError" cIndexError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_KeyError" cKeyError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_KeyboardInterrupt" cKeyboardInterrupt :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_MemoryError" cMemoryError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_NameError" cNameError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_NotImplementedError" cNotImplementedError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_OSError" cOSError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_OverflowError" cOverflowError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_ReferenceError" cReferenceError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_RuntimeError" cRuntimeError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_SyntaxError" cSyntaxError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_SystemExit" cSystemExit :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_SystemError" cSystemError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_TypeError" cTypeError :: Ptr (Ptr CPyObject)
foreign import ccall unsafe "&PyExc_ValueError" cValueError :: Ptr (Ptr CPyObject)
#ifdef MS_WINDOWS
foreign import ccall unsafe "&PyExc_WindowsError" cWindowsError :: Ptr (Ptr CPyObject)
#endif
foreign import ccall unsafe "&PyExc_ZeroDivisionError" cZeroDivisionError :: Ptr (Ptr CPyObject)

foreign import ccall unsafe "glue.h hspy_test"
 cTypeError2 :: IO (Ptr CPyObject)
