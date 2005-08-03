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

LDAP changes

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Modify (LDAPModOp(..), LDAPMod(..),
                    ldapModify
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
import Data.Bits

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
ldapModify ld dn changelist =
    withLDAPPtr ld (\cld ->
    withCString dn (\cdn ->
    withCLDAPModArr0 changelist (\cmods ->
    do checkLE "ldapModify" ld $ ldap_modify_s cld cdn cmods
       return ()
            )))

data CLDAPMod

newCLDAPMod :: LDAPMod -> IO (Ptr CLDAPMod)
newCLDAPMod lm =
    do (ptr::(Ptr CLDAPMod)) <- mallocBytes #{size LDAPMod}
       cmodtype <- newCString (modType lm)
       let (cmodop::LDAPInt) = 
               (fromIntegral . fromEnum . modOp $ lm) .|. 
               #{const LDAP_MOD_BVALUES}
       bervals <- mapM newBerval (modVals lm)
       (arrptr::Ptr (Ptr Berval)) <- newArray0 nullPtr bervals 
       ( #{poke LDAPMod, mod_op} ) ptr cmodop
       ( #{poke LDAPMod, mod_type } ) ptr cmodtype
       ( #{poke LDAPMod, mod_vals } ) ptr arrptr
       return ptr

freeCLDAPMod :: Ptr CLDAPMod -> IO ()
freeCLDAPMod ptr =
    do -- Free the array of Bervals
       (arrptr::Ptr (Ptr Berval)) <- ( #{peek LDAPMod, mod_vals} ) ptr
       (arr::[Ptr Berval]) <- peekArray0 nullPtr arrptr
       mapM_ freeHSBerval arr
       free arrptr
       -- Free the modtype
       (cmodtype::CString) <- ( #{peek LDAPMod, mod_type} ) ptr
       free cmodtype
       -- mod_op is an int and doesn't need freeing
       -- free the LDAPMod itself.
       free ptr
       
withCLDAPModArr0 :: [LDAPMod] -> (Ptr (Ptr CLDAPMod) -> IO a) -> IO a
withCLDAPModArr0 = withAnyArr0 newCLDAPMod freeCLDAPMod

foreign import ccall unsafe "ldap.h ldap_modify_s"
  ldap_modify_s :: LDAPPtr -> CString -> Ptr (Ptr CLDAPMod) -> IO LDAPInt

