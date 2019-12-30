{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_regex (
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

bindir     = "C:\\web-dev\\haskell-book-solutions\\realWorldHaskell\\8\\regex\\.stack-work\\install\\2ce8b0a3\\bin"
libdir     = "C:\\web-dev\\haskell-book-solutions\\realWorldHaskell\\8\\regex\\.stack-work\\install\\2ce8b0a3\\lib\\x86_64-windows-ghc-8.6.5\\regex-0.1.0.0-EYfpP66wqjdLWvnBXZx1m8-regex"
dynlibdir  = "C:\\web-dev\\haskell-book-solutions\\realWorldHaskell\\8\\regex\\.stack-work\\install\\2ce8b0a3\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\web-dev\\haskell-book-solutions\\realWorldHaskell\\8\\regex\\.stack-work\\install\\2ce8b0a3\\share\\x86_64-windows-ghc-8.6.5\\regex-0.1.0.0"
libexecdir = "C:\\web-dev\\haskell-book-solutions\\realWorldHaskell\\8\\regex\\.stack-work\\install\\2ce8b0a3\\libexec\\x86_64-windows-ghc-8.6.5\\regex-0.1.0.0"
sysconfdir = "C:\\web-dev\\haskell-book-solutions\\realWorldHaskell\\8\\regex\\.stack-work\\install\\2ce8b0a3\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "regex_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "regex_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "regex_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "regex_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "regex_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "regex_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
