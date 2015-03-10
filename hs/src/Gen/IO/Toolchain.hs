module Gen.IO.Toolchain (
    module X
  , toolchain
  , kindsList
  , statusesList
  , getToolchainDir
  , writeModelDef
  , runCommand
  , saveBinariesCsv
  )where

import Conjure.Language.Definition (Model)
import Conjure.UI.IO               (writeModel)
import Data.Data
import Gen.IO.Formats
import Gen.IO.ToolchainData        as X
import Gen.Prelude
import Paths_essence_gen           (getDataDir)
import System.Environment          (lookupEnv)
import System.Exit                 (ExitCode)
import System.FilePath             ((<.>))
import System.IO                   (IOMode (..), withFile)
import System.Process              (StdStream (..), createProcess, proc,
                                    showCommandForUser, std_err, std_out, waitForProcess)


writeModelDef :: MonadIO m => FilePath -> Model -> m FilePath
writeModelDef dir spec = do
    liftIO $ putStrLn . renderSmall . nest 4 . vcat $ ["Processing", pretty spec]
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
  code    <- runCommand script args (outputArg toolchainOutput outputDirectory)
  refineF <- readFromJSONMay $ outputDirectory </> "refine_essence.json"
  solveF  <- readFromJSONMay $ outputDirectory </> "solve_eprime.json"
  liftIO . putStrLn $ "Finished: " ++ showCommandForUser script args

  return $ case (refineF, solveF) of
             (Just r, Just s)  -> (code, SolveResult (r,s))
             (Just r, Nothing) -> (code, RefineResult r)
             (r, s)            -> error . show $ (r,s)


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
