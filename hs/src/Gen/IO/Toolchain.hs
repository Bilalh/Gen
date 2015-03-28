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
  )where

import Conjure.Language.Definition (Model)
import Conjure.UI.IO               (writeModel)
import Data.Data
import Gen.IO.Formats
import Gen.IO.ToolchainData        as X
import Gen.Prelude
import Paths_essence_gen           (getDataDir)
import System.Directory            (copyFile)
import System.Environment          (lookupEnv)
import System.Exit                 (ExitCode,exitSuccess)
import System.FilePath             ((<.>))
import System.IO                   (IOMode (..), withFile)
import System.Process              (StdStream (..), createProcess, proc,
                                    showCommandForUser, std_err, std_out,
                                    waitForProcess)

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
                      Nothing -> useDataDir
                      Just bp -> doesDirectoryExist (bp </> "toolchain") >>= \case
                                    True  -> do
                                      return (bp </> "toolchain")
                                    False -> fail . vcat $
                                       [" Can't find toolchain directory in" <+> pretty bp
                                       , "set REPO_GEN to gen/" ]
                  Just fp -> do
                    doesDirectoryExist (fp </> "toolchain") >>= \case
                       True -> do
                         return $ fp </> "toolchain"
                       False -> do
                         fail . vcat $ [" Can't find toolchain directory in data dir"
                                       , "set REPO_GEN to gen/"]

  where
  useDataDir = do
    fp <- getDataDir
    doesDirectoryExist (fp </> "toolchain") >>= \case
      True -> do
        return $ fp </> "toolchain"
      False -> do
        fail . vcat $ [" Can't find toolchain directory in data dir"
                      , "set REPO_GEN to gen/" ]


runCommand :: MonadIO m => FilePath -> [String] -> Maybe String -> m ExitCode
runCommand cmd args (Just fout)  =
  liftIO $ withFile fout  WriteMode  $ \hout  -> do
    (_, _, _, ph) <- createProcess (proc cmd args)
                     { std_out  = UseHandle hout
                     , std_err  = UseHandle hout
                     }
    waitForProcess ph

runCommand cmd args Nothing  =
  liftIO $ do
    (_, _, _, ph) <- createProcess (proc cmd args)
    waitForProcess ph


saveBinariesCsv :: FilePath -> IO ()
saveBinariesCsv fp= do
  toolchainDir <- getToolchainDir Nothing
  let cmd = toolchainDir </> "save_binaries_csv.sh"
  void $ runCommand cmd [fp] Nothing


doMeta :: FilePath -> Bool -> Maybe FilePath -> IO ()
doMeta out no_csv_copy bin_dir = do
  createDirectoryIfMissing True out

  case no_csv_copy of
    True  -> return ()
    False -> saveBinariesCsv out

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
