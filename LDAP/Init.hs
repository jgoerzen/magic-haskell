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

module CDK.Init(initCDK, initCDKColor,
                destroyCDKScreen,
                endCDK)
where

import Foreign.Ptr
import CDK.Types

{- | Initialize curses and CDK.  Returns the new cdkscreen handle. -}
foreign import ccall unsafe "glue.h initialize_cdk"
  initCDK :: IO CDKScreen

{- | 
starts the Cdk color capabilities.  It defines 64 color pairs each
            of  which is accessible using the COLOR_PAIR macro.  If you do not
            have color support, this function call makes no difference.
-}
foreign import ccall unsafe "cdk/cdk.h initCDKColor"
  initCDKColor :: IO ()

{- | Destroys a CDK screen. -}
foreign import ccall unsafe "cdk/cdk.h destroyCDKScreen"
  destroyCDKScreen :: CDKScreen -> IO ()

{- | Cleans up CDK and shuts down curses. -}
foreign import ccall unsafe "cdk/cdk.h endCDK"
  endCDK :: IO ()