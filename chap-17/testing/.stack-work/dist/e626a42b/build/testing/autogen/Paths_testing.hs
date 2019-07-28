{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_testing (
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

bindir     = "C:\\web-dev\\haskell-book-solutions\\chap-17\\testing\\.stack-work\\install\\804f060a\\bin"
libdir     = "C:\\web-dev\\haskell-book-solutions\\chap-17\\testing\\.stack-work\\install\\804f060a\\lib\\x86_64-windows-ghc-8.6.5\\testing-0.1.0.0-1TeLPkaAlZZ5FJsDU2cvZL-testing"
dynlibdir  = "C:\\web-dev\\haskell-book-solutions\\chap-17\\testing\\.stack-work\\install\\804f060a\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\web-dev\\haskell-book-solutions\\chap-17\\testing\\.stack-work\\install\\804f060a\\share\\x86_64-windows-ghc-8.6.5\\testing-0.1.0.0"
libexecdir = "C:\\web-dev\\haskell-book-solutions\\chap-17\\testing\\.stack-work\\install\\804f060a\\libexec\\x86_64-windows-ghc-8.6.5\\testing-0.1.0.0"
sysconfdir = "C:\\web-dev\\haskell-book-solutions\\chap-17\\testing\\.stack-work\\install\\804f060a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "testing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "testing_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "testing_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "testing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "testing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "testing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
