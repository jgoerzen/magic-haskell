{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : CDK.Init
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
                 ldapInit)
where

import Foreign.Ptr
import Foreign.C.String
import LDAP.Types
import Foreign.C.Types
import LDAP.Utils

{- | Preferred way to initialize a LDAP connection. 
The default port is given in 'LDAP.Constants.ldapPort'.

Could throw IOError on failure. -}
ldapInit :: String -> LDAPInt -> IO LDAP
ldapInit host port =
    withCString host (\cs ->
       checkNULL "ldapInit" $ cldap_init cs port)

{- | Like 'ldapInit', but establish network connection immediately. -}
ldapOpen :: String -> CInt -> IO LDAP
ldapOpen host port =
    withCString host (\cs ->
                      checkNULL "ldapOpen" $ cldap_open cs port)

foreign import ccall unsafe "ldap.h ldap_init"
  cldap_init :: CString -> CInt -> IO LDAP


foreign import ccall unsafe "ldap.h ldap_open"
  cldap_open :: CString -> CInt -> IO LDAP

