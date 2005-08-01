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
  do msgid <- checkLE "ldapSearch" ld $
              ldap_search cld cbase (fromIntegral $ fromEnum scope)
                          cfilter cattrs (fromBool attrsonly)
     
  ))))

data LDAPMessage

foreign import ccall unsafe "ldap.h ldap_search"
  ldap_search :: LDAPPtr -> CString -> LDAPInt -> CString -> Ptr CString ->
                 LDAPInt -> IO LDAPInt

foreign import ccall unsafe "ldap.h ldap_first_entry"
  ldap_first_entry :: LDAPPtr -> Ptr LDAPMessage -> IO (Ptr LDAPMessage)
