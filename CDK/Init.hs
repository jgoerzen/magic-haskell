{- 
Haskell CDK Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : CDK.Init
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Initialization and shutdown for CDK programs

Written by John Goerzen, jgoerzen\@complete.org
-}

module CDK.Init(initCDK)
where

import Foreign.Ptr
import CDK.Types

{- | Initialize curses and CDK.  Returns the new cdkscreen handle. -}
foreign import ccall safe "glue.h initialize_cdk"
  initCDK :: IO CDKscreen
