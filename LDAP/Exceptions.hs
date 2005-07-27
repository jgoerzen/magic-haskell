{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Exceptions
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Handling LDAP Exceptions

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Exceptions (-- * Types
                        LDAPException(..),
                        -- * General Catching
                        catchLDAP,
                        handleLDAP,
                        )

where
import Data.Dynamic
import Data.Typeable
import Control.Exception

{- | The basic type of LDAP exceptions.  These are raised when an operation
does not indicate success. -}

data LDAPException = LDAPException 
    {code :: LDAPInt,           -- ^ Numeric error code
     description :: String      -- ^ Description of error
    }
instance Show LDAPException where
    show x = "LDAPException " ++ show (code x) ++ ": " ++ description x

instance Eq LDAPException where
    x == y = code x == code y

instance Ord LDAPException where
    compare x y = compare (code x) (code y)

{- | Execute the given IO action.

If it raises a 'LDAPException', then execute the supplied handler and return
its return value.  Otherwise, process as normal. -}
catchLDAP :: IO a -> (LDAPException -> IO a) -> IO a
catchLDAP = catchDyn

{- | Like 'catchLDAP', with the order of arguments reversed. -}
handleLDAP :: (LDAPException -> IO a) -> IO a -> IO a
handleLDAP = flip catchLDAP

