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

module Magic.Operations()
where

import Foreign.Ptr
import Foreign.C.String
import Magic.Types
import Foreign.C.Types
import Foreign.C.String
import Magic.Utils
import Magic.TypesLL
import Foreign.Marshal.Utils

{- | Calls the Magic system on the specified file. -}
magicFile :: Magic -> FilePath -> IO String
magicFile magic fp =
    withMagicPtr magic (\cmagic ->
    withCString fp (\cfp ->
     do res <- throwErrnoIfNull "magicFile" (magic_file cmagic cfp)
        peekCString res
                    )
                       )

{- | Calls the Magic system on stdin. -}
magicStdin :: Magic -> IO String
magicStdin magic =
    withMagicPtr magic (\cmagic ->
     do res <- throwErrnoIfNull "magicStdin" (magic_stdin cmagic nullPtr)
        peekCString res
                       )

foreign import ccall unsafe "magic.h magic_file"
  magic_file :: Ptr CMagic -> CString -> IO CString
