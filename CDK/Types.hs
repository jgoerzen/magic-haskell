{- 
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

module CDK.Types(CDKScreen)
where

import Foreign.Ptr

type CCDKScreen = ()
type CDKScreen = Ptr CCDKScreen

