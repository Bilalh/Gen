module Gen.IO.Toolchain (
    module X
  , toolchain
  , kindsList
  , statusesList
  , getToolchainDir
  , writeModelDef
  , runCommand
  , saveBinariesCsv
  , copyMetaToSpecDir
  , logSpec
  , doMeta
  , getRunTime
  )where

import Conjure.Language.Definition (Model)
import Conjure.UI.IO               (writeModel)
import Data.Data
import Gen.IO.Formats
import Gen.IO.ToolchainData        as X
import Gen.Imports
import Paths_essence_gen           (getDataDir)
import System.Directory            (copyFile)
import System.Environment          (lookupEnv)
import System.Exit                 (ExitCode, exitSuccess)
import System.FilePath             ((<.>), dropFileName)
import System.IO                   (IOMode (..), withFile)
import System.Process              (StdStream (..), createProcess, proc,
                                    showCommandForUser, std_err, std_out, env,
                                    waitForProcess)
import System.Posix.Env(getEnvironment)
import System.Environment(getExecutablePath)

import qualified Data.Map as M

logSpec :: MonadIO m => Spec -> m ()
logSpec sp = do
  liftIO $ putStrLn . renderSmall . nest 4 . vcat $ ["Processing", pretty sp]

writeModelDef :: MonadIO m => FilePath -> Model -> m FilePath
writeModelDef dir spec = do
    liftIO $ createDirectoryIfMissing True  dir

    let name = (dir </> "spec" <.> ".essence")
    writeModel (Just name) spec
    return (dir </> "spec.essence")

toolchain :: MonadIO m => ToolchainData -> m (ExitCode, ToolchainResult)
toolchain ToolchainData{..} = do
  toolchainDir <- liftIO $ getToolchainDir binariesDirectory

  let script = toolchainDir </> "toolchain.py"
  let args = [ essencePath
             , "--outdir",  outputDirectory
             , "--timeout", show toolchainTime
             , "--num_cores", (show cores)
             ] ++ refineTypeArgs refineType ++ oldConjureArgs oldConjure
               ++ argsMay "--seed"    (fmap show seed)
               ++ argsMay "--bin_dir" binariesDirectory
               ++ argsMay "--param"   essenceParam
               ++ argsMay "--choices" choicesPath


  liftIO . putStrLn $ "Running: " ++ showCommandForUser script args
  when dryRun $ liftIO exitSuccess

  code    <- runCommand script args (outputArg toolchainOutput outputDirectory)
  refineF <- readFromJSONMay $ outputDirectory </> "refine_essence.json"
  solveF  <- readFromJSONMay $ outputDirectory </> "solve_eprime.json"
  liftIO . putStrLn $ "Finished: " ++ showCommandForUser script args

  return $ case (refineF, solveF) of
             (Just r, Just s)  -> (code, SolveResult (r,s))
             (Just r, Nothing) -> (code, RefineResult r)
             (r, s)            -> error . show . vcat $
                                    [
                                      nn "toolchain error on" essencePath
                                    , nn "outputDirectory"  outputDirectory
                                    , nn "rs" (groom ( r,s) )
                                    , nn "cmd" (showCommandForUser script args)
                                    ]


outputArg :: ToolchainOutput -> FilePath -> Maybe String
outputArg ToolchainScreen_ _ = Nothing
outputArg ToolchainFile_   d = Just (d </> "toolchain.log")
outputArg ToolchainNull_   _ = Just "/dev/null"


refineTypeArgs :: RefineType -> [String]
refineTypeArgs Refine_Only      = ["--refine_only"]
refineTypeArgs Refine_All       = ["--refine_all", "--refine_only"]
refineTypeArgs Refine_Solve     = []
refineTypeArgs Refine_Solve_All = ["--refine_all"]

argsMay :: String -> Maybe String -> [String]
argsMay _    Nothing   = []
argsMay flag (Just s)  = [flag,s]

oldConjureArgs :: Bool -> [String]
oldConjureArgs True  = []
oldConjureArgs False = ["--new_conjure"]


kindsList :: [String]
kindsList = do
  let names = dataTypeConstrs . dataTypeOf $ (error "kindI" :: KindI)
  map show names

statusesList :: [String]
statusesList = do
  let names = dataTypeConstrs . dataTypeOf $ (error "StatusI" :: StatusI)
  map show names


getToolchainDir :: (MonadFail m, MonadIO m) => Maybe FilePath -> m FilePath
getToolchainDir binDir = liftIO $ lookupEnv "REPO_GEN" >>= \case
                  Nothing -> do
                    case binDir of
                      Nothing -> useNextToApp >>= \case
                                 Just tc -> return tc
                                 Nothing -> useDataDir

                      Just bp -> doesDirectoryExist (bp </> "toolchain") >>= \case
                                    True  -> do
                                      return (bp </> "toolchain")
                                    False -> fail . vcat $
                                       [" Can't find toolchain directory in" <+> pretty bp
                                       , "set REPO_GEN to gen/ repo" ]
                  Just fp -> do
                    doesDirectoryExist (fp </> "toolchain") >>= \case
                       True -> do
                         return $ fp </> "toolchain"
                       False -> do
                         fail . vcat $ [" Can't find toolchain directory in REPO_GEN "
                                       , "set REPO_GEN to gen/ repo"]

  where

  useNextToApp = do
    p <- getExecutablePath
    let tc = dropFileName p </> "toolchain"
    doesDirectoryExist tc >>= \case
      True   -> return $ Just tc
      False  -> return Nothing


  useDataDir = do
    fp <- getDataDir
    doesDirectoryExist (fp </> "toolchain") >>= \case
      True -> do
        return $ fp </> "toolchain"
      False -> do
         fail . vcat $ [" Can't find toolchain directory next to gen binary or in data dir"
                       , "place toolchain/ next to gen binary or set REPO_GEN to gen/ repo"]

runCommand :: MonadIO m => FilePath -> [String] -> Maybe String -> m ExitCode
runCommand = runCommand' Nothing

runCommand' :: MonadIO m => Maybe [(String,String) ] ->
                            FilePath -> [String] -> Maybe String -> m ExitCode
runCommand' env cmd args (Just fout)  = do
  envUse <- doEnv env
  liftIO $ withFile fout  WriteMode  $ \hout  -> do
    (_, _, _, ph) <- createProcess (proc cmd args)
                     { std_out  = UseHandle hout
                     , std_err  = UseHandle hout
                     , env      = envUse
                     }
    waitForProcess ph

runCommand' env cmd args Nothing  = do
  envUse <- doEnv env
  liftIO $ do
    (_, _, _, ph) <- createProcess (proc cmd args){env = envUse}
    waitForProcess ph

doEnv :: MonadIO m
      => Maybe [(String, String)] -> m (Maybe [(String, String)])
doEnv Nothing  = return Nothing
doEnv (Just newNew) = do
  curEnv <- liftIO $ getEnvironment
  -- liftIO $ print $ "PATH" `lookup`  curEnv
  let merged = M.toList $ (M.fromList newNew)  `M.union` (M.fromList curEnv)
  -- liftIO $ print $ "PATH" `lookup`  merged
  return $ Just $  merged


saveBinariesCsv :: FilePath -> Maybe FilePath -> IO ()
saveBinariesCsv fp bin_dir= do
  toolchainDir <- getToolchainDir Nothing
  env_use <- bin_env bin_dir
  let cmd = toolchainDir </> "save_binaries_csv.sh"
  void $ runCommand' env_use  cmd [fp] Nothing

  where
    bin_env Nothing  = return Nothing
    bin_env (Just x) = do
      liftIO $ lookupEnv ("PATH" :: String) >>= \case
                Nothing -> liftIO $ error "No PATH variable"
                Just p -> do
                    let newPath = x ++ ":" ++ p
                    return $ Just [("PATH", newPath)]


doMeta :: FilePath -> Bool -> Maybe FilePath -> IO ()
doMeta out no_csv_copy bin_dir = do
  createDirectoryIfMissing True out

  case no_csv_copy of
    True  -> return ()
    False -> saveBinariesCsv out bin_dir

  case bin_dir of
    Nothing -> return ()
    Just bp -> doesFileExist (bp </> "meta.json") >>= \case
               False -> return ()
               True  -> copyFile (bp </> "meta.json") (out </> "meta.json")

copyMetaToSpecDir :: FilePath -> FilePath -> IO ()
copyMetaToSpecDir base_out spec_out = do
  createDirectoryIfMissing True spec_out

  doesFileExist (base_out </> "meta.json") >>= \case
           False -> return ()
           True  -> copyFile (base_out </> "meta.json") (spec_out </> "meta-ran.json")

  doesFileExist (base_out </> "versions.csv") >>= \case
           False -> return ()
           True  -> copyFile (base_out </> "versions.csv") (spec_out </> "versions-ran.csv")

getRunTime :: ToolchainResult -> Double
getRunTime (RefineResult x)      = time_taken_ x
getRunTime (SolveResult (x1,x2)) = time_taken_ x1 + time_taken_ x2
