{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Modify
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

LDAP Searching

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Modify (LDAPModOp
                   )
where

import LDAP.Utils
import LDAP.Types
import LDAP.TypesLL
import LDAP.Data
import Foreign
import Foreign.C.String
import LDAP.Result
import Control.Exception(finally)

#include <ldap.h>

data LDAPMod = LDAPMod {modOp :: LDAPModOp -- ^ Type of operation to perform
                       ,modType :: String -- ^ Name of attribute to edit
                       ,modVals :: [String] -- ^ New values
                       }
             deriving (Eq, Show)

ldapModify :: LDAP              -- ^ LDAP connection object
           -> String            -- ^ DN to modify
           -> [LDAPMod]         -- ^ Changes to make
           -> IO ()

data CLDAPMod

foreign import ccall unsafe "ldap.h ldap_modify_s"
  ldap_modify_s :: LDAPPtr -> CString -> Ptr (Ptr CLDAPMod) -> IO LDAPInt

