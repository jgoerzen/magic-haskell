{- -*- Mode: haskell; -*-
Haskell LDAP Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : LDAP.Search
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

LDAP Searching

Written by John Goerzen, jgoerzen\@complete.org
-}

module LDAP.Search (SearchAttributes
                   )
where

import LDAP.Utils
import LDAP.Types
import LDAP.Data
import Foreign

#include <ldap.h>

{- | Defines what attributes to return with the search result. -}
data SearchAttributes =
  LDAPNoAttrs                   -- ^ No attributes
  LDAPAllUserAttrs              -- ^ User attributes only
  LDAPAttrList [String]         -- ^ User-specified list

sa2sl :: SearchAttributes -> [String]
sa2sl LDAPNoAttrs = [ #{const_str LDAP_NO_ATTRS} ]
sa2sl LDAPAllUserAttrs = [ #{const_str LDAP_ALL_USER_ATTRIBUTES} ]
sa2sl (LDAPAttrList x) = x

ldapSearch :: LDAP              -- ^ LDAP connection object
           -> Maybe String      -- ^ Base DN for search, if any
           -> LDAPScope         -- ^ Scope of the search
           -> Maybe String      -- ^ Filter to be used (none if Nothing)
           -> SearchAttributes  -- ^ Desired attributes in result set
           -> Bool              -- ^ If True, exclude attribute values (return types only)
           -> IO [LDAPEntry]

ldapSearch ld base scope filter attrs attrsonly =
  withLDAPPtr ld (\cld ->
  withMString base (\cbase ->
  withMString filter (\cfilter ->
  withCStringArr (sa2sl attrs) (\cattrs ->
  do msgid <- checkLEn1 "ldapSearch" ld $
              ldap_search cld cbase (fromIntegral $ fromEnum scope)
                          cfilter cattrs (fromBool attrsonly)
     res1 <- ldap_1result ld msgid
     withForeignPtr res1 (\cres1 ->
      do felm <- ldap_first_entry cld cres1
         if felm == nullPtr
            then return []
            else getattrs ld felm
     
  ))))

data BerElement

getattrs :: LDAP -> (Ptr LDAPMessage) -> IO [(String, [String])]
getattrs ld lmptr =
    withLDAPPtr ld (\cld -> alloca (f cld))
    where f cld (ptr::Ptr (Ptr BerElement)) =
              do cstr <- ldap_first_attribute cld lmptr ptr
                 if cstr == nullPtr
                    then return []
                    else do str <- peekCString
                            ldap_memfree cstr
                            bptr <- peek ptr
                            values <- getvalues
                            nextitems <- getnextitems cld lmptr bptr
                            return $ (str, values):nextitems

getnextitems :: Ptr CLDAP -> Ptr LDAPMessage -> Ptr BerElement 
             -> IO [(String, [String])]
getnextitems cld lmptr bptr =
    do cstr <- ldap_next_attribute cld lmptr bptr
       if cstr == nullPtr
          then return []
          else do str <- peekCString
                  ldap_memfree cstr
                  values <- getvalues
                  nextitems <- getnextitems cld lmptr bptr
                  return $ (str, values):nextitems

getvalues 

foreign import ccall unsafe "ldap.h ldap_search"
  ldap_search :: LDAPPtr -> CString -> LDAPInt -> CString -> Ptr CString ->
                 LDAPInt -> IO LDAPInt

foreign import ccall unsafe "ldap.h ldap_first_entry"
  ldap_first_entry :: LDAPPtr -> Ptr LDAPMessage -> IO (Ptr LDAPMessage)

foreign import ccall unsafe "ldap.h ldap_first_attribute"
  ldap_first_attribute :: LDAPPtr -> Ptr LDAPMessage -> Ptr (Ptr BerElement) 
                       -> IO CString

foreign import ccall unsafe "ldap.h ldap_next_attribute"
  ldap_next_attribute :: LDAPPtr -> Ptr LDAPMessage -> Ptr BerElement
                       -> IO CString