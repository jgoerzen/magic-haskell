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
                          -- * Intrepreting Code
                          pyRun_SimpleString,
                          pyRun_String,
                          pyRun_StringHs,
                          StartFrom(..),
                          -- * Calling Code
                          callByName,
                          noParms,
                          noKwParms,
                          -- * Imports
                          pyImport,
                          pyImport_ImportModule,
                          pyImport_AddModule,
                          pyModule_GetDict
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
pyRun_StringHs :: (ToPyObject b, FromPyObject c) =>
                  String        -- ^ Command to run
               -> StartFrom     -- ^ Start token
--               -> [(String, a)] -- ^ Globals (may be empty)
               -> [(String, b)] -- ^ Locals (may be empty)
               -> IO c
pyRun_StringHs cmd start locals =
    let conv (k, v) = do v1 <- toPyObject v
                         return (k, v1)
        in do
           --rglobals <- mapM conv globals
           rlocals <- mapM conv locals
           pyRun_String cmd start rlocals >>= fromPyObject
    
-- | Run some code in Python.
pyRun_String :: String          -- ^ Command to run
             -> StartFrom       -- ^ Start Token
--             -> [(String, PyObject)] -- ^ Globals (may be empty)
             -> [(String, PyObject)] -- ^ Locals (may be empty)
             -> IO PyObject     -- ^ Result
pyRun_String command startfrom xlocals =
    let cstart = sf2c startfrom
        in do dobj <- getDefaultGlobals
              rlocals <- toPyObject xlocals
              withCString command (\ccommand ->
               withPyObject dobj (\cglobals ->
                withPyObject rlocals (\clocals ->
                 cpyRun_String ccommand cstart cglobals clocals >>= fromCPyObject
                              )))

{- | Call a function or callable object by name.

You can use 'noParms' and 'noKwParms' if you have no simple or
keyword parameters to pass, respectively. -}
callByName :: (ToPyObject a, ToPyObject b, FromPyObject c) =>
              String            -- ^ Object/function name
           -> [a]               -- ^ List of non-keyword parameters
           -> [(String, b)]     -- ^ List of keyword parameters
           -> IO c
callByName fname sparms kwparms =
    do putStrLn "inCallByName"
       getDefaultGlobals >>= showPyObject >>= putStrLn
       func <- pyRun_String fname Py_eval_input []
       showPyObject func >>= putStrLn
       pyObject_CallHs func sparms kwparms

{- | Import a module into the current environment in the normal sense
(similar to \"import\" in Python).
-}
pyImport :: String -> IO ()
pyImport x = 
    do r <- pyImport_ImportModule x 
       globals <- getDefaultGlobals
       cdict <- pyImport_GetModuleDict
       py_incref cdict
       dict <- fromCPyObject cdict >>= fromPyObject
       case lookup x dict of
           Nothing -> do putStrLn $ "No " ++ x 
                         return ()
           Just pyo -> do putStrLn $ "Found " ++ x
                          withPyObject globals (\cglobals ->
                           withPyObject pyo (\cmodule ->
                            withCString x (\cstr ->
                             pyDict_SetItemString cglobals cstr cmodule)))
                          return ()

{- | Wrapper around C PyImport_ImportModule, which imports a module.

You may want the higher-level 'pyImport' instead. -}
pyImport_ImportModule :: String -> IO PyObject
pyImport_ImportModule x =
    do globals <- getDefaultGlobals
       fromlist <- toPyObject ['*']
       cr <- withPyObject globals (\cglobals ->
              withPyObject fromlist (\cfromlist ->
               withCString x (\cstr -> 
                cpyImport_ImportModuleEx cstr cglobals cglobals cfromlist)))
       fromCPyObject cr


foreign import ccall unsafe "Python.h Py_Initialize"
  py_initialize :: IO ()

foreign import ccall unsafe "Python.h PyRun_SimpleString"
  cpyRun_SimpleString :: CString -> IO CInt

foreign import ccall unsafe "Python.h PyRun_String"
  cpyRun_String :: CString -> CInt -> Ptr CPyObject -> Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyImport_ImportModuleEx"
 cpyImport_ImportModuleEx :: CString -> Ptr CPyObject -> Ptr CPyObject -> Ptr CPyObject -> IO (Ptr CPyObject)

foreign import ccall unsafe "glue.h PyDict_SetItemString"
 pyDict_SetItemString :: Ptr CPyObject -> CString -> Ptr CPyObject -> IO CInt

foreign import ccall unsafe "glue.h PyImport_GetModuleDict"
 pyImport_GetModuleDict :: IO (Ptr CPyObject)