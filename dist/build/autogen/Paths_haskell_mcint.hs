module Paths_haskell_mcint (
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

bindir     = "/home/nick/.cabal/bin"
libdir     = "/home/nick/.cabal/lib/x86_64-linux-ghc-7.10.3/haskell-mcint-0.1.0.0-9fjD7ftBxkUIzh9X8nnApH"
datadir    = "/home/nick/.cabal/share/x86_64-linux-ghc-7.10.3/haskell-mcint-0.1.0.0"
libexecdir = "/home/nick/.cabal/libexec"
sysconfdir = "/home/nick/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_mcint_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_mcint_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_mcint_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_mcint_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_mcint_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
