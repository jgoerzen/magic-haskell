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

module Magic.Operations(-- * Guessing the type
                        magicFile, magicStdin,
                        magicString, magicCString)
where

import Foreign.Ptr
import Foreign.C.String
import Magic.Types
import Foreign.C.Types
import Data.Word
import Foreign.C.String
import Foreign.C.Error
import Magic.Utils
import Magic.TypesLL
import Foreign.Marshal.Utils

{- | Calls the Magic system on the specified file. -}
magicFile :: Magic -> FilePath -> IO String
magicFile magic fp =
    withMagicPtr magic (\cmagic ->
    withCString fp (\cfp ->
     do res <- throwErrorIfNull "magicFile" magic (magic_file cmagic cfp)
        peekCString res
                    )
                       )

{- | Calls the Magic system on stdin. -}
magicStdin :: Magic -> IO String
magicStdin magic =
    withMagicPtr magic (\cmagic ->
     do res <- throwErrorIfNull "magicStdin" magic (magic_file cmagic nullPtr)
        peekCString res
                       )

{- | Calls the Magic system to process the given String.  Please note:
it is not evaluated lazily. -}
magicString :: Magic -> String -> IO String
magicString m s = withCStringLen s (magicCString m)

{- | Lower-level function used to call the Magic system to process a C 
string. -}
magicCString :: Magic -> CStringLen -> IO String
magicCString magic (cstr, len) =
    withMagicPtr magic (\cmagic ->
     do res <- throwErrorIfNull "magicCString" magic (magic_buffer cmagic cstr (fromIntegral len))
        peekCString res
                    )


foreign import ccall unsafe "magic.h magic_file"
  magic_file :: Ptr CMagic -> CString -> IO CString

foreign import ccall unsafe "magic.h magic_buffer"
  magic_buffer :: Ptr CMagic -> CString -> #{type size_t} -> IO CString
