module Paths_aca_processor_simulator (
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

bindir     = "/home/fe14/jl14910/linux/Documents/Year_3/Advanced_Computer_Architecture/aca-processor-simulator/.cabal-sandbox/bin"
libdir     = "/home/fe14/jl14910/linux/Documents/Year_3/Advanced_Computer_Architecture/aca-processor-simulator/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/aca-processor-simulator-0.1.0.0"
datadir    = "/home/fe14/jl14910/linux/Documents/Year_3/Advanced_Computer_Architecture/aca-processor-simulator/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/aca-processor-simulator-0.1.0.0"
libexecdir = "/home/fe14/jl14910/linux/Documents/Year_3/Advanced_Computer_Architecture/aca-processor-simulator/.cabal-sandbox/libexec"
sysconfdir = "/home/fe14/jl14910/linux/Documents/Year_3/Advanced_Computer_Architecture/aca-processor-simulator/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aca_processor_simulator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aca_processor_simulator_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "aca_processor_simulator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aca_processor_simulator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aca_processor_simulator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
