{- -*- Mode: haskell; -*-
Haskell magic Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
-}

{- |
   Module     : Magic.Init
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : BSD

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Initialization and shutdown for magic programs

Written by John Goerzen, jgoerzen\@complete.org
-}

module Magic.Init(magicOpen)
where

import Foreign.Ptr
import Foreign.C.String
import Magic.Types
import Foreign.C.Types
import Magic.Utils
import Magic.TypesLL
import Foreign.Marshal.Utils

{- | Initialize the Magic system.
-}
magicOpen :: [MagicFlag] -> IO Magic
magicOpen mfl =
    fromMagicPtr "magicOpen" (magic_open flags)
    where flags = flaglist2int mfl

foreign import ccall unsafe "magic.h magic_open"
  magic_open :: CInt -> IO (Ptr CMagic)
