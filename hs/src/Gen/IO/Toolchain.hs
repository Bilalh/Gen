module Gen.IO.Toolchain (
    module X
  , toolchain
  , kindsList
  , statusesList
  , getToolchainDir
  , writeModelDef
  , foo
  )where

import Data.Data
import Data.List (foldl1)
import Gen.IO.Formats
import Gen.IO.ToolchainData as X
import Gen.Prelude
import Paths_essence_gen (getDataDir)
import System.Environment (lookupEnv, setEnv)
import System.Process (rawSystem)
import Conjure.Language.Definition(Model)
import Conjure.UI.IO(writeModel)
import System.FilePath((<.>))
import System.Process(createProcess, waitForProcess, proc
                     , StdStream(..), std_out, std_err)
import System.IO(withFile,IOMode(..))


writeModelDef :: MonadIO m => FilePath -> Model -> m FilePath
writeModelDef dir spec = do
    liftIO $ putStrLn . renderSmall . nest 4 . vcat $ ["Running", pretty spec]
    liftIO $ createDirectoryIfMissing True  dir

    let name = (dir </> "spec" <.> ".essence")
    writeModel (Just name) spec
    return (dir </> "spec.essence")

toolchain :: MonadIO m => ToolchainData -> m ToolchainResult
toolchain ToolchainData{..} = do
  toolchainDir <- liftIO $ getToolchainDir binariesDirectory

  let script = toolchainDir </> "toolchain_wrap.sh"
  let args = [ essencePath
             , "--outdir",  outputDirectory
             , "--timeout", show totalTime
             , "--num_cores", (show cores)
             ] ++ refineTypeArgs refineType ++ oldConjureArgs oldConjure
               ++ argsMay "--seed"    (fmap show seed)
               ++ argsMay "--bin_dir" binariesDirectory
               ++ argsMay "--param"   essenceParam


  liftIO . putStrLn $ "cmd: " ++ script ++ " " ++ foldl1 (\a b -> a ++ " " ++ b) args

  _       <- liftIO $ rawSystem script args
  refineF <- readFromJSONMay $ outputDirectory </> "refine_essence.json"
  solveF  <- readFromJSONMay $ outputDirectory </> "solve_eprime.json"

  return $ case (refineF, solveF) of
             (Just r, Just s)  -> SolveResult (r,s)
             (Just r, Nothing) -> RefineResult r
             (r, s)            -> error . show $ (r,s)



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


getToolchainDir :: Maybe FilePath -> IO FilePath
getToolchainDir binDir = lookupEnv "PARAM_GEN_SCRIPTS" >>= \case
                  Nothing -> do
                    case binDir of
                      Nothing -> useDataDir
                      Just bp -> doesDirectoryExist (bp </> "toolchain") >>= \case
                                    True  -> do
                                      setEnv "PARAM_GEN_SCRIPTS" bp
                                      return (bp </> "toolchain")
                                    False -> useDataDir
                  Just fp -> do
                    doesDirectoryExist (fp </> "toolchain") >>= \case
                       True -> do
                         return $ fp </> "toolchain"
                       False -> do
                         error . show . vcat $ [" Can't find toolchain directory in data dir"
                                               , "set PARAM_GEN_SCRIPTS to instancegen/scripts"]

  where
  useDataDir = do
    fp <- getDataDir
    doesDirectoryExist (fp </> "toolchain") >>= \case
      True -> do
        setEnv "PARAM_GEN_SCRIPTS" fp
        return $ fp </> "toolchain"
      False -> do
        error . show . vcat $ [" Can't find toolchain directory in data dir"
                              , "set PARAM_GEN_SCRIPTS to instancegen/scripts"]


foo cmd fout ferr =
    withFile fout  WriteMode  $ \hout  ->
    withFile ferr WriteMode $ \herr -> do
        (_, _, _, ph) <- createProcess (proc cmd [])
            { std_out  = UseHandle hout
            , std_err = UseHandle herr
            }
        waitForProcess ph
