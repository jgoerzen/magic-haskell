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

module LDAP.Utils(checkLE, checkNULL, LDAPPtr, fromLDAPPtr,
                  withLDAPPtr, maybeWithLDAPPtr) where
import Foreign.Ptr
import LDAP.Constants
import LDAP.Exceptions
import LDAP.Types
import LDAP.Data
import LDAP.TypesLL
import Control.Exception
import Data.Dynamic
import Foreign.C.Error
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign

{- | Check the return value.  If it's something other than 
'LDAP.Constants.ldapSuccess', raise an LDAP exception. -}
checkLE :: String -> LDAP -> IO LDAPInt -> IO ()
checkLE callername ld action =
    do result <- action
       if result == fromIntegral (fromEnum LdapSuccess)
          then return ()
          else do s <- (ldap_err2string result >>= peekCString)
                  let exc = LDAPException {code = (toEnum (fromIntegral result)), 
                                           description = s,
                                           caller = callername}
                  throwDyn exc

{- | Raise an IOError based on errno if getting a NULL.  Identical
to Foreign.C.Error.throwErrnoIfNull. -}
checkNULL :: String -> IO (Ptr a) -> IO (Ptr a)
checkNULL = throwErrnoIfNull

{- | Value coming in from C -}
type LDAPPtr = Ptr CLDAP

{- | Convert a LDAPPtr into a LDAP type.  Checks it with 'checkNULL'
automatically. -}
fromLDAPPtr :: String -> IO LDAPPtr -> IO LDAP
fromLDAPPtr caller action =
    do ptr <- checkNULL caller action
       newForeignPtr ldap_unbind ptr

{- | Use a 'LDAP' in a function that needs 'LDAPPtr'. -}
withLDAPPtr :: LDAP -> (LDAPPtr -> IO a) -> IO a
withLDAPPtr ld = withForeignPtr ld

{- | Same as 'withLDAPPtr', but uses nullPtr if the input is Nothing. -}
maybeWithLDAPPtr :: Maybe LDAP -> (LDAPPtr -> IO a) -> IO a
maybeWithLDAPPtr Nothing func = func nullPtr
maybeWithLDAPPtr (Just x) y = withLDAPPtr x y

{- | Returns a string, doesn't raise exceptions on err (just crashes) -}
ldapGetOptionStrNoEc :: LDAP -> LDAPOptionCode -> IO String
ldapGetOptionStrNoEc ld oc =
    withLDAPPtr ld (\pld ->
     alloca (f pld))
    where
    oci = fromEnum oc
    f pld (ptr::Ptr CString) = 
        do res <- ldap_get_option pld (fromIntegral oci) (castPtr ptr)
           if res /= 0
              then fail $ "Crash in ldap_get_option, code " ++ show res
              else do cstr <- peek ptr
                      fp <- newForeignPtr ldap_memfree_call cstr
                      withForeignPtr fp (peekCString)

foreign import ccall unsafe "ldap.h &ldap_unbind"
  ldap_unbind :: FunPtr (LDAPPtr -> IO ()) -- ldap_unbind, ignoring retval

foreign import ccall unsafe "ldap.h ldap_err2string"
  ldap_err2string :: LDAPInt -> IO CString

foreign import ccall unsafe "ldap.h ldap_get_option"
  ldap_get_option :: LDAPPtr -> LDAPInt -> Ptr () -> IO LDAPInt

foreign import ccall unsafe "ldap.h &ldap_memfree"
  ldap_memfree_call :: FunPtr (CString -> IO ())