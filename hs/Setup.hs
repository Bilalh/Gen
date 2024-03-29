{-# LANGUAGE LambdaCase #-}
import Prelude
import Distribution.Simple
import Distribution.PackageDescription (emptyHookedBuildInfo, PackageDescription(..), HookedBuildInfo )
import System.Process (readProcess, readProcessWithExitCode)
import System.Directory
import System.FilePath 
import System.Exit(ExitCode(..))

import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), absoluteInstallDirs, bindir)
import Distribution.Simple.InstallDirs( datadir)
import Distribution.Simple.Setup ( fromFlag, copyDest, CopyDest(..),BuildFlags )
import Distribution.Simple.Utils
import Distribution.Verbosity

import Control.Monad(forM_)

main = defaultMainWithHooks myHooks
    where myHooks = simpleUserHooks {
          preBuild = myPreBuild
        , postCopy = \ _ flags pkg lbi -> copyScripts pkg lbi (fromFlag $ copyDest flags)
        , postInst = \ _ _     pkg lbi -> copyScripts pkg lbi NoCopyDest
        }

copyScripts :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
copyScripts pkg local copy = do
    let dirs = absoluteInstallDirs pkg local copy
    copyFile "scripts/genLogged.sh" (bindir dirs </> "genLogged.sh")
    createDirectoryIfMissing True (datadir dirs)
    copyDirectory "../toolchain" (datadir dirs </> "toolchain")

myPreBuild :: Args 
           -> BuildFlags
           -> IO HookedBuildInfo
myPreBuild _ _ = do
  putStrLn "Generating dist/build/autogen/Build_autoversion.hs ..."
  createDirectoryIfMissing True "dist/build/autogen/"

  (scode,_,_) <- readProcessWithExitCode "git" ["rev-parse", "--is-inside-work-tree"] ""
  case scode of
    ExitFailure{} -> do
      doesFileExist "Build_autoversion.hs" >>= \case
           True  -> renameFile "Build_autoversion.hs" "dist/build/autogen/Build_autoversion.hs"
           False -> do
             error $  "Error: Not inside a git repo and ./Build_autoversion.hs not found\n"
			       ++ "Use ./scripts/make_autoversion.sh to create ./Build_autoversion.hs"
				   ++ "when inside a git repo, so successive builds do not require git"
      
    ExitSuccess -> do
      makeVersionInfo

  return emptyHookedBuildInfo


makeVersionInfo :: IO ()
makeVersionInfo = do
  desc <- readProcess "git" ["log", "-1", "--format='%H (%cD)'"] ""

  writeFile "dist/build/autogen/Build_autoversion.hs.tmp" $ unlines
      [ "module Build_autoversion where"
      , "import Prelude(String)"
      , "autoVersion :: String"
      , "autoVersion = " ++ show (init desc)
      ]

  doesFileExist "dist/build/autogen/Build_autoversion.hs" >>= \case
    False -> renameFile
                         "dist/build/autogen/Build_autoversion.hs.tmp"
                         "dist/build/autogen/Build_autoversion.hs"
    True  -> do
      (code,_,_) <- readProcessWithExitCode "diff" [
                     "dist/build/autogen/Build_autoversion.hs",
                     "dist/build/autogen/Build_autoversion.hs.tmp"]
                    ""
      case code of
        ExitSuccess -> return ()
        _           -> renameFile
                         "dist/build/autogen/Build_autoversion.hs.tmp"
                         "dist/build/autogen/Build_autoversion.hs"


copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory srcDir destDir = do
  info normal ("copy directory '" ++ srcDir ++ "' to '" ++ destDir ++ "'.")
  srcFiles <- getDirectoryContentsRecursive srcDir
  let files = [ (srcDir, f) | f <- srcFiles
                            , takeBaseName f `notElem` ["tmtags", "tmtagsHistory"]
                            , (takeBaseName. takeDirectory ) f `notElem` ["__pycache__"] ]
  installMaybeExecutableFiles normal destDir files
	  