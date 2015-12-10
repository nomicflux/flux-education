module Paths_Flux (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/michael/Code/Flux/.cabal-sandbox/bin"
libdir     = "/home/michael/Code/Flux/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/Flux-0.1.0.0"
datadir    = "/home/michael/Code/Flux/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/Flux-0.1.0.0"
libexecdir = "/home/michael/Code/Flux/.cabal-sandbox/libexec"
sysconfdir = "/home/michael/Code/Flux/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Flux_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Flux_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Flux_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Flux_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Flux_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
