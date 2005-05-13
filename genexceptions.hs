import MissingH.Str
import Data.List

notice = " NOTICE -- THIS FILE IS AUTO-GENERATED -- DO NOT EDIT"

ifdefcode str =
    "#ifdef MS_WINDOWS\n" ++ str ++ "\n#endif\n"

windowsexc = "WindowsError"

genGlue copyright excs =
    do h <- openFile "glue/excglue.h"
       c <- openFile "glue/excglue.h"
       let head = "/* " ++ notice ++ "\n" ++ copyright ++ 
              "*/\n#include <Python.h>"
       hPutStrLn h head
       hPutStrLn c head
       hPutStrLn h $ unlines . map excfunch $ excs
       hPutstrLn h $ ifdefcode $ excfunch windowsexc
       hPutStrLn c $ unlines . map excfuncc $ excs
       hPutStrLn c $ ifdefcode $ excfuncc windowsexc
       hClose h
       hClose c
    where excfunch e =
              "extern PyObject *hspy_" ++ e ++ "(void);"
          excfuncc e = 
              "PyObject *hspy_" ++ e ++ "(void) { return PyExc_" ++ e
              ++ "; }"

genExcTypes copyright excs =
    do h <- openFile "Python/Exceptions/ExcTypes.hsc"
       hPutStrLn h $ "{- " ++ notice ++ "\n" ++ copyright ++
                     "-}"
       hPutStrLn h $ unlines $ 
          ["{- |",
           "   Module     : Python.Exceptions.ExcTypes",
           "   Copyright  : Copyright (C) 2005 John Goerzen",
           "   License    : GNU GPL, version 2 or above",
           "",
           "   Maintainer : John Goerzen,",
           "   Maintainer : jgoerzen@complete.org",
           "   Stability  : provisional",
           "   Portability: portable",
           "",
           "Python low-level exception definitions",
           "",
           "These are definitions of the built-in Python exception objects.  You can",
           "use them with 'MissingPy.Python.Exceptions.doesExceptionMatch' and",
           "'MissingPy.Python.Exceptions.catchSpecificPy'.",
           "",
           "The meanings of these exceptions can be found at",
           "<http://www.python.org/doc/current/lib/module-exceptions.html>.",
           "",
           "Please note that windowsError is available only on Microsoft platforms.",
           "",
           "Written by John Goerzen, jgoerzen\@complete.org",
           "-}",
           "#include <Python.h>",
           "module Python.Exceptions.ExcTypes",
           "("]
       hPutStrLn $ ifdefcode $ (hsname windowsexc) ++ ","
       hPutStrLn $ intersperse ',' . map hsname $ excs
       hPutStrLn ")\nwhere"
       hPutStrLn $ unlines $
          ["import Python.Types",
           "import Python.Objects",
           "import System.IO.Unsafe",
           "import Python.Utils",
           "import Foreign",
           "exctypes_internal_e :: IO (Ptr CPyObject) -> PyObject",
           "exctypes_internal_e f = do p <- f",
           "                           fp <- newForeignPtr_ p",
           "                           return $ PyObject fp"
          ]
       hPutStrLn h $ unlines . map hsfunc $ excs
       hPutStrLn h $ ifdefcode $ hsfunc windowsexc
       hPutStrLn h $ unlines . map cfunc $ excs
       hPutStrLn h $ ifdefcode $ cfunc windowsexc
    where hsname "Exception" = "pyMainException"
          hsname (x:xs) = toUpper x : xs
          hsfunc exc =
              "{-# NOINLINE " ++ hsname exc ++ " #-}\n" ++
              hsname exc ++ " = unsafePerformIO $ exctypes_internal_e " ++
                "c" ++ exc ++ "\n"
          cfunc exc = 
              "foreign import ccall unsafe \"excglue.h hspy_" ++ exc ++ "\"\n" ++
              " c" ++ exc ++ " :: IO (Ptr CPyObject)\n"
              

main = do c <- readFile "exceptionlist"
          copyright <- readfile "COPYRIGHT"
          let excs = sort . map strip . filter (/= "") . lines $ c
          genGlue copyright excs
          genExcTypes copyright excs
