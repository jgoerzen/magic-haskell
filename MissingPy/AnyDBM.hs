{-# OPTIONS -fallow-overlapping-instances #-}

{- arch-tag: Interface to anydbm.py
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
   Module     : MissingPy.AnyDBM
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

This module interfaces "MissingH.AnyDBM" to Python's anydbm.py.
Implementations for specific Python *dbm modules are also available.

See and import "MissingH.AnyDBM" to use these features.

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingPy.AnyDBM(
                       )
where


import Python.Objects.Dict
import Python.Interpreter
import Python.Utils
import Python.Objects
import MissingH.AnyDBM

{- | Flags used to open a dbm-type database -}
data PyDBMOpenFlags = 
   DBM_ReadOnly                 -- ^ Open an /existing/ database for read only
 | DBM_ReadWrite                -- ^ Open an /existing/ database for reading and writing
 | DBM_ReadWriteCreate          -- ^ Open a database for reading and writing, creating if it doesn't exist
 | DBM_ReadWriteNew             -- ^ Open a database, creating it anew each time (deleting any existing data)
flag2str :: PyDBMOpenFlags -> String
flag2str DBM_ReadOnly = "r"
flag2str DBM_ReadWrite = "w"
flag2str DBM_ReadWriteCreate = "c"
flag2str DBM_ReadWriteNew = "n"

{- | Opens a persistent storage database using the \"best\" storage mechanism
available to Python on this system.  This will usually be one of the *dbm
services, though in rare circumstances, could be \"dumbdbm\", which is
only marginally better than "MissingH.AnyDBM.StringDBM".
-}
openAnyDBM :: FilePath -> PyDBMOpenFlags -> IO PyDict
openAnyDBM = openSpecificDBM "anydbm"

{- | Open a database using a specific module given by the first parameter.  The
module supported are:

* dbhash

* dbm

* dumbdbm

* gdbm

SECURITY NOTE: the string is not validated before being passed to Python.
Do not pass an arbitrary value to this function.
-}
openSpecificDBM :: String       -- ^ Python module name to use
                -> FilePath     -- ^ Path to database files
                -> PyDBMOpenFlags -- ^ Flags to use when opening
                -> IO PyDict    -- ^ Result
openSpecificDBM mod fp flag =
    let flagstr = flag2str flag
        in
        do pyImport mod
           fileobj <- toPyObject fp
           flagobj <- toPyObject flagstr
           obj <- callByName (mod ++ ".open") [fileobj, flagobj] []
           return $ mkPyDict obj
