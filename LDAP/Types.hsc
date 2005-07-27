{-  -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Types
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Basic types for LDAP programs.

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Types(LDAP, LDAPInt)
where

import Foreign.Ptr
import Data.Word
import Data.Int
import Foreign.C.Types

#include <ldap.h>

data CLDAP

{- | Main LDAP object type -}
type LDAP = Ptr CLDAP

{- | Convenience type so we use the correct ints for the LDAP library. -}
type LDAPInt = CInt
