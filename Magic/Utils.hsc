{- -*- Mode: haskell; -*-
Haskell magic Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : Magic.Utils
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Utils

Written by John Goerzen, jgoerzen\@complete.org
-}

module Magic.Utils (flaglist2int, fromMagicPtr, withMagicPtr)
where

import Foreign
import Foreign.C.Error
import Foreign.ForeignPtr
import Magic.TypesLL
import Magic.Types
import Data.Bits
import Foreign.C.Types
import Magic.Data

flaglist2int :: [MagicFlag] -> CInt
flaglist2int mfl =
    foldl (\c f -> c .|. (fromIntegral . fromEnum $ f)) 0 mfl

fromMagicPtr :: String -> IO (Ptr CMagic) -> IO Magic
fromMagicPtr caller action =
    do ptr <- throwErrnoIfNull caller action
       newForeignPtr magic_close ptr

withMagicPtr :: Magic -> (Ptr CMagic -> IO a) -> IO a
withMagicPtr m = withForeignPtr m

foreign import ccall unsafe "magic.h &magic_close"
  magic_close :: FunPtr (Ptr CMagic -> IO ())
