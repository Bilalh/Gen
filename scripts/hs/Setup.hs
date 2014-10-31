import Distribution.Simple
import Distribution.PackageDescription (emptyHookedBuildInfo, PackageDescription(..) )
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)
import System.Directory ( copyFile )
import System.FilePath ( (</>) )


import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..), absoluteInstallDirs, bindir )
import Distribution.Simple.Setup ( fromFlag, copyDest, CopyDest(..) )

main = defaultMainWithHooks myHooks
    where myHooks = simpleUserHooks {
          preBuild = myPreBuild
        , postCopy = \ _ flags pkg lbi -> copyScripts pkg lbi (fromFlag $ copyDest flags)
        , postInst = \ _ _     pkg lbi -> copyScripts pkg lbi NoCopyDest
        }

copyScripts :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
copyScripts pkg local copy = do
    let dirs = absoluteInstallDirs pkg local copy
    copyFile "scripts/testSampleWrap.sh" (bindir dirs </> "testSampleWrap.sh")


myPreBuild _ _ = do
    putStrLn "Generating dist/build/autogen/Build_autoversion.hs ..."
    createDirectoryIfMissing True "dist/build/autogen/"

    desc <- readProcess "git" ["log", "-1", "--format='%H (%cD)'"] ""
    now  <- readProcess "date" ["+%s"] ""

    writeFile "dist/build/autogen/Build_autoversion.hs" $ unlines
        [ "module Build_autoversion where"
        , "import Data.DateTime"
        , "autoVersion :: String"
        , "autoVersion = " ++ show (init desc)
        , "buildDateTime :: DateTime"
        , "buildDateTime = fromSeconds " ++ now
        , "buildDateRFC2822 :: String"
        , "buildDateRFC2822 = formatDateTime \"%a, %d %b %Y %T %z\" buildDateTime"
        ]
    return emptyHookedBuildInfo
