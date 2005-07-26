{- -*- Mode: haskell; -*-
Haskell CDK Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : CDK.Constants
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

CDK constants for use in your programs

Written by John Goerzen, jgoerzen\@complete.org
-}

module CDK.Constants
    (cLEFT, cRIGHT, cCENTER, cTOP,
     cBOTTOM, cHORIZONTAL, cVERTICAL,
     cNONE, cROW, cCOL)
where

#include "cdk/cdk.h"

cLEFT = #const LEFT
cRIGHT = #const RIGHT
cCENTER = #const CENTER
cTOP = #const TOP
cBOTTOM = #const BOTTOM
cHORIZONTAL = #const HORIZONTAL
cVERTICAL = #const VERTICAL

cNONE = #const NONE
cROW = #const ROW
cCOL = #const COL
