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

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Python type instances and object utilities

Written by John Goerzen, jgoerzen\@complete.org
-}

module Python.Objects (
                       -- * Basic Object Types
                       PyObject,
                       -- * Conversions between Haskell and Python Objects
                       ToPyObject(..),
                       FromPyObject(..),
                       -- * Information about Python Objects
                       typeOf,
                       strOf,
                       reprOf,
                       showPyObject,
                       dirPyObject,
                       -- * Conversions between Python Objects
                       pyList_AsTuple,
                       -- * Calling Python Objects
                       pyObject_Call,
                       pyObject_CallHs
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
import System.IO.Unsafe

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

{- | Displays a list of keys contained in the Python object. -}
dirPyObject :: PyObject -> IO [String]
dirPyObject x = withPyObject x (\cpyo ->
                   do dr <- pyObject_Dir cpyo >>= fromCPyObject
                      fromPyObject dr
                               )

{- | Call a Python object with all-Haskell parameters.
Similar to 'PyObject_Call'.  This limits you to a single item type for
the regular arguments and another single item type for the keyword arguments. 
Nevertheless, it could be a handy shortcut at times.

For a higher-level wrapper, see 'Python.Interpreter.callByName'. -}
pyObject_CallHs :: (ToPyObject a, ToPyObject b, FromPyObject c) =>
                   PyObject     -- ^ Object t
                -> [a]          -- ^ List of non-keyword parameters
                -> [(String, b)] -- ^ List of keyword parameters
                -> IO c         -- ^ Return value
pyObject_CallHs callobj simpleargs kwargs =
    let conv (k, v) = do v1 <- toPyObject v
                         return (k, v1)
        in
        do s <- mapM toPyObject simpleargs
           k <- mapM conv kwargs
           pyObject_Call callobj s k >>= fromPyObject

{- | Call a Python object (function, etc).

For a higher-level wrapper, see 'Python.Interpreter.callByName'.
 -}
pyObject_Call :: PyObject       -- ^ Object to call
              -> [PyObject]     -- ^ List of non-keyword parameters (may be empty)
              -> [(String, PyObject)] -- ^ List of keyword parameters (may be empty)
              -> IO PyObject    -- ^ Return value
pyObject_Call callobj simpleparams kwparams =
        do pyosimple <- toPyObject simpleparams >>= pyList_AsTuple
           pyokw <- toPyObject kwparams
           cval <- withPyObject callobj (\ccallobj ->
                    withPyObject pyosimple (\cpyosimple ->
                     withPyObject pyokw (\cpyokw ->
                      cpyObject_Call ccallobj cpyosimple cpyokw)))
           fromCPyObject cval
       
-- ^ Converts a Python list to a tuple.
pyList_AsTuple :: PyObject -> IO PyObject
pyList_AsTuple x =
    withPyObject x (\cpo -> cpyList_AsTuple cpo >>= fromCPyObject)

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

-- FIXME: ERROR CHECKING!

--------------------------------------------------
-- [PyObject] Lists

-- | Lists from a PyObject
instance ToPyObject [PyObject] where
    toPyObject mainlist =
        do l <- pyList_New 0
           mapM_ (\pyo -> withPyObject pyo (pyList_Append l)) mainlist
           fromCPyObject l

-- | Tuples and Lists to [PyObject] lists
instance FromPyObject [PyObject] where
    fromPyObject x = 
        let worker cpyo =
                do islist <- pyList_Check cpyo
                   istuple <- pyTuple_Check cpyo
                   if islist /= 0
                      then fromx pyList_Size pyList_GetItem cpyo
                      else if istuple /= 0
                                 then fromx pyTuple_Size pyTuple_GetItem cpyo
                                 else fail "Error fromPyObject to [PyObject]: Passed object not a list or tuple."
            fromx sizefunc itemfunc cpyo = do size <- sizefunc cpyo
                                              fromx_worker 0 size itemfunc cpyo
            fromx_worker counter size itemfunc cpyo =
                if counter >= size 
                   then return []
                   else do thisitem <- itemfunc cpyo counter
                           py_incref thisitem
                           thisobj <- fromCPyObject thisitem
                           next <- unsafeInterleaveIO $ fromx_worker (succ counter) size itemfunc cpyo
                           return $ thisobj : next
            in
            withPyObject x worker

--------------------------------------------------
-- Association Lists

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

-- | ALs from Dicts
instance FromPyObject [(PyObject, PyObject)] where
    fromPyObject pydict = withPyObject pydict (\cpydict ->
        do items <- pyDict_Items cpydict >>= fromCPyObject
           -- Now, make a Haskell [[PyObject, PyObject]] list
           itemlist <- fromPyObject items
           -- Finally, convert it to a list of tuples.
           return $ map list2tup itemlist
                                              )
        where list2tup x = case x of
                                  x1:x2:[] -> (x1, x2)
                                  _ -> error "Expected 2-tuples in fromPyObject dict"
                                       
-- | This is a common variant used for arg lists
instance ToPyObject a => ToPyObject [(a, PyObject)] where
    toPyObject mainlist =
        let conv (k, v) = do k1 <- toPyObject k
                             return (k1, v)
            in mapM conv mainlist >>= toPyObject
instance FromPyObject a => FromPyObject [(a, PyObject)] where
    fromPyObject pyo =
        let conv (k, v) = do k1 <- fromPyObject k
                             return (k1, v)
        in do list <- (fromPyObject pyo)::IO [(PyObject, PyObject)]
              mapM conv list


-- | Dicts from Haskell objects
instance (ToPyObject a, ToPyObject b) => ToPyObject [(a, b)] where
    toPyObject mainlist =
        let convone (i1, i2) = do oi1 <- toPyObject i1
                                  oi2 <- toPyObject i2
                                  return (oi1, oi2)
        in do newl <- mapM convone mainlist
              toPyObject newl

-- | Dicts to Haskell objects
instance (FromPyObject a, FromPyObject b) => FromPyObject [(a, b)] where
    fromPyObject pydict =
        let conv (x, y) = do x1 <- fromPyObject x
                             y1 <- fromPyObject y
                             return (x1, y1)
            in do pyodict <- ((fromPyObject pydict)::IO [(PyObject, PyObject)])
                  mapM conv pyodict

--------------------------------------------------
-- Strings

-- CStringLen to PyObject.  Use CStringLen to handle embedded nulls.
instance ToPyObject CStringLen where
   toPyObject (x, len) = 
       pyString_FromStringAndSize x (fromIntegral len) >>= fromCPyObject

-- String to PyObject
instance ToPyObject String where
    toPyObject x = withCString x (\cstr -> toPyObject (cstr, length x))

-- PyObject to String
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

--------------------------------------------------
-- Numbers, Python Ints

-- Python ints are C longs
instance ToPyObject CLong where
    toPyObject x =  pyInt_FromLong x >>= fromCPyObject

-- And convert back.
instance FromPyObject CLong where
    fromPyObject x = withPyObject x pyInt_AsLong

-- We'll also support CInts.
instance ToPyObject CInt where
    toPyObject x = toPyObject ((fromIntegral x)::CLong)

instance FromPyObject CInt where
    fromPyObject x = do y <- (fromPyObject x)::IO CLong
                        return $ fromIntegral y

--------------------------------------------------
-- Numbers, Python Longs

instance ToPyObject Integer where
    toPyObject i = 
        -- Use strings here since no other C type supports
        -- unlimited precision.
        let repr = show i
        in withCString repr (\cstr -> 
             pyLong_FromString cstr nullPtr 10 >>= fromCPyObject)
                                 
instance FromPyObject Integer where
    fromPyObject pyo = 
        do longstr <- strOf pyo
           return $ read longstr

--------------------------------------------------
-- Numbers, anything else.
{- For these, we attempt to guess whether to handle it as an
int or a long. -}
{-
Disabled for now; this is a low-level interface, and it seems to be overly
complex for this.

instance Integral a => ToPyObject a where
    toPyObject x =
        let intval = toInteger x
            in
            if (intval < (toInteger (minBound::CLong)) ||
                intval > (toInteger (maxBound::CLong)))
                then toPyObject intval
                else toPyObject ((fromIntegral x)::CLong)

-- On the return conversion, we see what the bounds for
-- the desired type are, and treat it thusly.
instance (Bounded a, Integral a) => FromPyObject a where
    fromPyObject x =
        let minpyint = toInteger (minBound::CLong)
            maxpyint = toInteger (maxBound::CLong)
            minpassed = toInteger (minBound::a)
            maxpassed = toInteger (maxBound::a)
            in if (minpassed < minpyint || maxpassed > maxpyint)
                  then do intval <- fromPyObject x
                          return $ fromInteger intval
                  else do longval <- ((fromPyObject x)::IO CLong)
                          return $ fromIntegral longval

-}

--------------------------------------------------
-- Floating-Point Values

instance ToPyObject CDouble where
    toPyObject x = pyFloat_FromDouble x >>= fromCPyObject

instance FromPyObject CDouble where
    fromPyObject x = withPyObject x pyFloat_AsDouble

-- | Lists from anything else
instance ToPyObject a => ToPyObject [a] where
    toPyObject mainlist = 
        do newlist <- mapM toPyObject mainlist
           toPyObject newlist

instance FromPyObject a => FromPyObject [a] where
    fromPyObject pylistobj = 
        do pylist <- fromPyObject pylistobj
           mapM fromPyObject pylist

----------------------------------------------------------------------
-- C imports
----------------------------------------------------------------------

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

foreign import ccall unsafe "glue.h PyDict_SetItem"
 pyDict_SetItem :: Ptr CPyObject -> Ptr CPyObject -> Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyObject_Str"
 pyObject_Str :: Ptr CPyObject -> IO (Ptr CPyObject)

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

foreign import ccall unsafe "glue.h hspy_incref"
 py_incref :: Ptr CPyObject -> IO ()

foreign import ccall unsafe "glue.h PyDict_Items"
 pyDict_Items :: Ptr CPyObject -> IO (Ptr CPyObject)

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
