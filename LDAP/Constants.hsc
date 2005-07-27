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

module LDAP.Constants(module LDAP.Constants)
where
import Foreign.C.Types
import LDAP.Types

#include "ldap.h"

#enum LDAPInt, , LDAP_PORT, LDAPS_PORT, LDAP_API_VERSION, LDAP_VENDOR_NAME, \
      LDAP_OPT_API_INFO, LDAP_OPT_DESC, LDAP_OPT_DEREF, \
      LDAP_OPT_SIZELIMIT, LDAP_OPT_TIMELIMIT, LDAP_OPT_REFERRALS, \
      LDAP_OPT_RESTART, LDAP_OPT_PROTOCOL_VERSION, LDAP_OPT_SERVER_CONTROLS, \
      LDAP_OPT_CLIENT_CONTROLS, LDAP_OPT_API_FEATURE_INFO, \
      LDAP_OPT_HOST_NAME, LDAP_OPT_ERROR_NUMBER, LDAP_OPT_ERROR_STRING, \
      LDAP_OPT_MATCHED_DN, LDAP_OPT_ON, LDAP_OPT_OFF, LDAP_OPT_SUCCESS, \
      LDAP_OPT_ERROR, \
      LDAP_API_INFO_VERSION, LDAP_FEATURE_INFO_VERSION, \
      LDAP_CONTROL_VALUESRETURNFILTER, LDAP_CONTROL_SUBENTRIES, \
      LDAP_CONTROL_NOOP, LDAP_CONTROL_MANAGEDSAIT, LDAP_CONTROL_PROXY_AUTHZ, \
      LDAP_CONTROL_SORTREQUEST, LDAP_CONTROL_SORTRESPONSE, \
      LDAP_CONTROL_VLVREQUEST, LDAP_CONTROL_VLVRESPONSE, \
      LDAP_NOTICE_OF_DISCONNECTION, LDAP_NOTICE_DISCONNECT, \
      LDAP_SUCCESS, LDAP_OPERATIONS_ERROR, LDAP_PROTOCOL_ERROR, \
      LDAP_TIMELIMIT_EXCEEDED, LDAP_SIZELIMIT_EXCEEDED, LDAP_COMPARE_FALSE, \
      LDAP_COMPARE_TRUE, LDAP_AUTH_METHOD_NOT_SUPPORTED, \
      LDAP_STRONG_AUTH_NOT_SUPPORTED, LDAP_STRONG_AUTH_REQUIRED, \
      LDAP_PARTIAL_RESULTS, LDAP_REFERRAL, LDAP_ADMINLIMIT_EXCEEDED, \
      LDAP_UNAVAILABLE_CRITICAL_EXTENSION, LDAP_CONFIDENTIALITY_REQUIRED, \
      LDAP_SASL_BIND_IN_PROGRESS, LDAP_NO_SUCH_ATTRIBUTE, LDAP_UNDEFINED_TYPE,\
      LDAP_INAPPROPRIATE_MATCHING, LDAP_CONSTRAINT_VIOLATION, \
      LDAP_TYPE_OR_VALUE_EXISTS, LDAP_INVALID_SYNTAX, LDAP_NO_SUCH_OBJECT,\
      LDAP_ALIAS_PROBLEM, LDAP_INVALID_DN_SYNTAX, LDAP_IS_LEAF,\
      LDAP_ALIAS_DEREF_PROBLEM, LDAP_PROXY_AUTHZ_FAILURE,\
      LDAP_INAPPROPRIATE_AUTH, LDAP_INVALID_CREDENTIALS, \
      LDAP_INSUFFICIENT_ACCESS, LDAP_BUSY, LDAP_UNAVAILABLE, \
      LDAP_UNWILLING_TO_PERFORM, LDAP_LOOP_DETECT, \
      LDAP_NAMING_VIOLATION, LDAP_OBJECT_CLASS_VIOLATION, \
      LDAP_NOT_ALLOWED_ON_NONLEAF, LDAP_NOT_ALLOWED_ON_RDN,\
      LDAP_ALREADY_EXISTS, LDAP_NO_OBJECT_CLASS_MODS,\
      LDAP_RESULTS_TOO_LARGE, LDAP_AFFECTS_MULTIPLE_DSAS,\
      LDAP_OTHER, LDAP_SERVER_DOWN, LDAP_LOCAL_ERROR, LDAP_ENCODING_ERROR,\
      LDAP_DECODING_ERROR, LDAP_TIMEOUT, LDAP_AUTH_UNKNOWN,\
      LDAP_FILTER_ERROR, LDAP_USER_CANCELLED, LDAP_PARAM_ERROR,\
      LDAP_NO_MEMORY, LDAP_CONNECT_ERROR,\
      LDAP_NOT_SUPPORTED, LDAP_CONTROL_NOT_FOUND, LDAP_NO_RESULTS_RETURNED,\
      LDAP_MORE_RESULTS_TO_RETURN, LDAP_CLIENT_LOOP, \
      LDAP_REFERRAL_LIMIT_EXCEEDED

#enum BERTag, , LDAP_FILTER_AND, LDAP_FILTER_OR, LDAP_FILTER_NOT, \
      LDAP_FILTER_EQUALITY, LDAP_FILTER_SUBSTRINGS, LDAP_FILTER_GE,\
      LDAP_FILTER_LE, LDAP_FILTER_PRESENT, LDAP_FILTER_APPROX,\
      LDAP_FILTER_EXT, LDAP_FILTER_EXT_OID, LDAP_FILTER_EXT_TYPE,\
      LDAP_FILTER_EXT_VALUE,LDAP_FILTER_EXT_DNATTRS, \
      LDAP_SUBSTRING_ANY, LDAP_SUBSTRING_FINAL, LDAP_SUBSTRING_INITIAL

#enum BERInt, , LDAP_SCOPE_DEFAULT, LDAP_SCOPE_BASE, LDAP_SCOPE_ONELEVEL, \
      LDAP_SCOPE_SUBTREE