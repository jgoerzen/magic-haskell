{-  -*- Mode: haskell; -*-
Haskell CDK Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : CDK.Types
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Basic types for CDK programs.

Written by John Goerzen, jgoerzen\@complete.org
-}

module CDK.Types(CDKScreen, CDKEntry)
where

import Foreign.Ptr
import Data.Word
import Data.Int

#include <cdk/cdk.h>
#include <ncurses.h>

data CCDKScreen

{- | Main CDK screen type -}
type CDKScreen = Ptr CCDKScreen

data CCDKEntry

{- | CDK Entry -}
type CDKEntry = Ptr CCDKEntry

type Chtype = #type chtype