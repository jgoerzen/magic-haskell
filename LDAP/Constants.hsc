{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Constants
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

LDAP constants for use in your programs

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Constants
    (ldapPort)
where
import Foreign.C.Types
import LDAP.Types

#include "ldap.h"

#enum LDAPInt, , LDAP_PORT
