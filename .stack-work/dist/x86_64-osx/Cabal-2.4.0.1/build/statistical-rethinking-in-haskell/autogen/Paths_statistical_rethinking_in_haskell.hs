{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_statistical_rethinking_in_haskell (
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

bindir     = "/Users/shuyuan/repo/daily-haskell/statistical-rethinking-in-haskell/.stack-work/install/x86_64-osx/ad3dcfe821b861f4a08ac056dee3123b1e9d8d6e6797931288d88a93eaa22324/8.6.5/bin"
libdir     = "/Users/shuyuan/repo/daily-haskell/statistical-rethinking-in-haskell/.stack-work/install/x86_64-osx/ad3dcfe821b861f4a08ac056dee3123b1e9d8d6e6797931288d88a93eaa22324/8.6.5/lib/x86_64-osx-ghc-8.6.5/statistical-rethinking-in-haskell-0.1.0.0-DNHLgSKhB8P5ikoMpoLA3B-statistical-rethinking-in-haskell"
dynlibdir  = "/Users/shuyuan/repo/daily-haskell/statistical-rethinking-in-haskell/.stack-work/install/x86_64-osx/ad3dcfe821b861f4a08ac056dee3123b1e9d8d6e6797931288d88a93eaa22324/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/shuyuan/repo/daily-haskell/statistical-rethinking-in-haskell/.stack-work/install/x86_64-osx/ad3dcfe821b861f4a08ac056dee3123b1e9d8d6e6797931288d88a93eaa22324/8.6.5/share/x86_64-osx-ghc-8.6.5/statistical-rethinking-in-haskell-0.1.0.0"
libexecdir = "/Users/shuyuan/repo/daily-haskell/statistical-rethinking-in-haskell/.stack-work/install/x86_64-osx/ad3dcfe821b861f4a08ac056dee3123b1e9d8d6e6797931288d88a93eaa22324/8.6.5/libexec/x86_64-osx-ghc-8.6.5/statistical-rethinking-in-haskell-0.1.0.0"
sysconfdir = "/Users/shuyuan/repo/daily-haskell/statistical-rethinking-in-haskell/.stack-work/install/x86_64-osx/ad3dcfe821b861f4a08ac056dee3123b1e9d8d6e6797931288d88a93eaa22324/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "statistical_rethinking_in_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "statistical_rethinking_in_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "statistical_rethinking_in_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "statistical_rethinking_in_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "statistical_rethinking_in_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "statistical_rethinking_in_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
