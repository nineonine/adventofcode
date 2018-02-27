module Paths_adventofcode (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/nineonine/.cabal/bin"
libdir     = "/Users/nineonine/.cabal/lib/x86_64-osx-ghc-7.10.2/adventofcode-0.1.0.0-3lt1FxCxekk2gEUEyMFYaT"
datadir    = "/Users/nineonine/.cabal/share/x86_64-osx-ghc-7.10.2/adventofcode-0.1.0.0"
libexecdir = "/Users/nineonine/.cabal/libexec"
sysconfdir = "/Users/nineonine/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "adventofcode_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "adventofcode_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "adventofcode_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "adventofcode_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "adventofcode_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
