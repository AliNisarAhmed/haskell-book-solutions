{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_url_shortener (
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

bindir     = "C:\\web-dev\\haskell-book-solutions\\chap-19\\url-shortener\\.stack-work\\install\\74ffa373\\bin"
libdir     = "C:\\web-dev\\haskell-book-solutions\\chap-19\\url-shortener\\.stack-work\\install\\74ffa373\\lib\\x86_64-windows-ghc-8.6.5\\url-shortener-0.1.0.0-i1MFmCcsAYDJ4G8ngdySN-url-shortener"
dynlibdir  = "C:\\web-dev\\haskell-book-solutions\\chap-19\\url-shortener\\.stack-work\\install\\74ffa373\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\web-dev\\haskell-book-solutions\\chap-19\\url-shortener\\.stack-work\\install\\74ffa373\\share\\x86_64-windows-ghc-8.6.5\\url-shortener-0.1.0.0"
libexecdir = "C:\\web-dev\\haskell-book-solutions\\chap-19\\url-shortener\\.stack-work\\install\\74ffa373\\libexec\\x86_64-windows-ghc-8.6.5\\url-shortener-0.1.0.0"
sysconfdir = "C:\\web-dev\\haskell-book-solutions\\chap-19\\url-shortener\\.stack-work\\install\\74ffa373\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "url_shortener_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "url_shortener_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "url_shortener_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "url_shortener_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "url_shortener_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "url_shortener_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
