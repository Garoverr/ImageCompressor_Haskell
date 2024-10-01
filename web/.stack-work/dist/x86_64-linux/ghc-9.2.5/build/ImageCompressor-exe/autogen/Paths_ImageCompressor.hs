{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ImageCompressor (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/jsapin/Garoverr/ImageCompressor_Haskell/web/.stack-work/install/x86_64-linux/01f9863decaa82fa33cfdc0d3c58fdb33d18a17c396e856185d30c53828f8817/9.2.5/bin"
libdir     = "/home/jsapin/Garoverr/ImageCompressor_Haskell/web/.stack-work/install/x86_64-linux/01f9863decaa82fa33cfdc0d3c58fdb33d18a17c396e856185d30c53828f8817/9.2.5/lib/x86_64-linux-ghc-9.2.5/ImageCompressor-0.1.0.0-55Qum46IGPn4P6dzZDKJqT-ImageCompressor-exe"
dynlibdir  = "/home/jsapin/Garoverr/ImageCompressor_Haskell/web/.stack-work/install/x86_64-linux/01f9863decaa82fa33cfdc0d3c58fdb33d18a17c396e856185d30c53828f8817/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/jsapin/Garoverr/ImageCompressor_Haskell/web/.stack-work/install/x86_64-linux/01f9863decaa82fa33cfdc0d3c58fdb33d18a17c396e856185d30c53828f8817/9.2.5/share/x86_64-linux-ghc-9.2.5/ImageCompressor-0.1.0.0"
libexecdir = "/home/jsapin/Garoverr/ImageCompressor_Haskell/web/.stack-work/install/x86_64-linux/01f9863decaa82fa33cfdc0d3c58fdb33d18a17c396e856185d30c53828f8817/9.2.5/libexec/x86_64-linux-ghc-9.2.5/ImageCompressor-0.1.0.0"
sysconfdir = "/home/jsapin/Garoverr/ImageCompressor_Haskell/web/.stack-work/install/x86_64-linux/01f9863decaa82fa33cfdc0d3c58fdb33d18a17c396e856185d30c53828f8817/9.2.5/etc"

getBinDir     = catchIO (getEnv "ImageCompressor_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ImageCompressor_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ImageCompressor_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ImageCompressor_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ImageCompressor_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ImageCompressor_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
