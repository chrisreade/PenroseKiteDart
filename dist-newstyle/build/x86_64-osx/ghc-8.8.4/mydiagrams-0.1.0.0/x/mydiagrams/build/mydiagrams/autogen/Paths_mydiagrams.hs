{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mydiagrams (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/chrisreade/.cabal/bin"
libdir     = "/Users/chrisreade/.cabal/lib/x86_64-osx-ghc-8.8.4/mydiagrams-0.1.0.0-inplace-mydiagrams"
dynlibdir  = "/Users/chrisreade/.cabal/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/chrisreade/.cabal/share/x86_64-osx-ghc-8.8.4/mydiagrams-0.1.0.0"
libexecdir = "/Users/chrisreade/.cabal/libexec/x86_64-osx-ghc-8.8.4/mydiagrams-0.1.0.0"
sysconfdir = "/Users/chrisreade/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mydiagrams_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mydiagrams_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mydiagrams_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mydiagrams_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mydiagrams_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mydiagrams_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
