import Distribution.Simple
import Distribution.PackageDescription (emptyHookedBuildInfo)
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

main = defaultMainWithHooks myHooks
  where myHooks = simpleUserHooks { preBuild = myPreBuild }

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
