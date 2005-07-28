{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Init
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Initialization and shutdown for LDAP programs

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Init(ldapOpen,
                 ldapInit,
                 ldapSimpleBind)
where

import Foreign.Ptr
import Foreign.C.String
import LDAP.Types
import Foreign.C.Types
import LDAP.Utils

{- | Preferred way to initialize a LDAP connection. 
The default port is given in 'LDAP.Constants.ldapPort'.

Could throw IOError on failure. -}
ldapInit :: String              -- ^ Host
         -> LDAPInt             -- ^ Port
         -> IO LDAP             -- ^ New LDAP Obj
ldapInit host port =
    withCString host (\cs ->
       fromLDAPPtr "ldapInit" $ cldap_init cs port)

{- | Like 'ldapInit', but establish network connection immediately. -}
ldapOpen :: String              -- ^ Host
            -> CInt             -- ^ Port
            -> IO LDAP          -- ^ New LDAP Obj
ldapOpen host port =
    withCString host (\cs ->
                      fromLDAPPtr "ldapOpen" $ cldap_open cs port)

{- | Bind to the remote server. -}
ldapSimpleBind :: LDAP          -- ^ LDAP Object
               -> String        -- ^ DN (Distinguished Name)
               -> String        -- ^ Password
               -> IO ()
ldapSimpleBind ld dn passwd =
    withLDAPPtr ld (\ptr ->
     withCString dn (\cdn ->
      withCString passwd (\cpasswd -> 
        checkLE "ldapSimpleBind" ld
                            (ldap_simple_bind_s ptr cdn cpasswd))))

foreign import ccall unsafe "ldap.h ldap_init"
  cldap_init :: CString -> CInt -> IO LDAPPtr


foreign import ccall unsafe "ldap.h ldap_open"
  cldap_open :: CString -> CInt -> IO LDAPPtr

foreign import ccall unsafe "ldap.h ldap_simple_bind_s"
  ldap_simple_bind_s :: LDAPPtr -> CString -> CString -> IO LDAPInt