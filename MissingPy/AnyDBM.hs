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


import Python.Dict
import MissingH.AnyDBM

{- | Flags used to open a dbm-type database -}
data PyDBMOpenFlags = 
   DBM_ReadOnly                 -- ^ Open an /existing/ database for read only
 | DBM_ReadWrite                -- ^ Open an /existing/ database for reading and writing
 | DBM_ReadWriteCreate          -- ^ Open a database for reading and writing, creating if it doesn't exist
 | DBM_ReadWriteNew             -- ^ Open a database, creating it anew each time

{- |Open a GZip file.  The compression level should be from 1
(least compression) to 9 (most compression).  This is ignored when the
file is opened read-only.

Once opened, the functions defined in 'MissingH.IO.HVIO' can be used to 
work with it. -}
openGz :: FilePath              -- ^ File to open
       -> IOMode                -- ^ Mode to open with
       -> Int                   -- ^ Compression Level
       -> IO PyFile             -- ^ Resulting handle
openGz fp mode level =
    do ofp <- toPyObject fp
       omode <- toPyObject (openModeConv mode)
       ocl <- toPyObject ((fromIntegral level)::CLong)
       pyImport "gzip"
       obj <- callByName "gzip.open" [ofp, omode, ocl] []
       return $ mkPyFile obj
