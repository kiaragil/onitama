{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_my_project (
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

bindir     = "C:\\Users\\Kiara\\Desktop\\dm552-project(1)\\dm552-project\\.stack-work\\install\\21786603\\bin"
libdir     = "C:\\Users\\Kiara\\Desktop\\dm552-project(1)\\dm552-project\\.stack-work\\install\\21786603\\lib\\x86_64-windows-ghc-8.10.7\\my-project-0.1.0.0-IV6fmSWW5mR9jXwN6DPWBo-my-project-test"
dynlibdir  = "C:\\Users\\Kiara\\Desktop\\dm552-project(1)\\dm552-project\\.stack-work\\install\\21786603\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\Kiara\\Desktop\\dm552-project(1)\\dm552-project\\.stack-work\\install\\21786603\\share\\x86_64-windows-ghc-8.10.7\\my-project-0.1.0.0"
libexecdir = "C:\\Users\\Kiara\\Desktop\\dm552-project(1)\\dm552-project\\.stack-work\\install\\21786603\\libexec\\x86_64-windows-ghc-8.10.7\\my-project-0.1.0.0"
sysconfdir = "C:\\Users\\Kiara\\Desktop\\dm552-project(1)\\dm552-project\\.stack-work\\install\\21786603\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "my_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "my_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "my_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "my_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "my_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "my_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
