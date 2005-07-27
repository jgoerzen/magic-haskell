{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Utils
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

LDAP low-level utilities

Written by John Goerzen, jgoerzen\@complete.org

Please use sparingly and with caution.  The documentation for their behavior
should be considered to be the source code.

-}

module LDAP.Utils(checkLE) where
import Foreign.Ptr
import LDAP.Constants
import LDAP.Exceptions
import LDAP.Types
import Control.Exception
import Data.Dynamic
import Foreign.C.Error
import Foreign.C.String


{- | Check the return value.  If it's something other than 
'LDAP.Constants.ldapSuccess', raise an LDAP exception. -}

checkLE :: String -> IO LDAPInt -> IO ()
checkLE callername action =
    do result <- action
       if result == ldapSuccess
          then return ()
          else do s <- (ldap_err2string result >>= peekCString)
                  let exc = LDAPException {code = result, 
                                           description = s,
                                           caller = callername}
                  throwDyn exc

{- | Raise an IOError based on errno if getting a NULL.  Identical
to Foreign.C.Error.throwErrnoIfNull. -}
checkNULL :: String -> IO (Ptr a) -> IO (Ptr a)
checkNULL = throwErrnoIfNull

foreign import ccall unsafe "ldap.h ldap_err2string"
   ldap_err2string :: LDAPInt -> IO CString
