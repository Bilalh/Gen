{-# LANGUAGE RecordWildCards #-}
module Gen.IO.Toolchain (
    module X
  , toolchain
  , kindsList
  , statusesList
  , getToolchainDir
  , writeModelDef
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


writeModelDef :: MonadIO m => FilePath -> Model -> m FilePath
writeModelDef dir spec = do
    liftIO $ putStrLn . renderSmall . nest 4 . vcat $ ["Running", pretty spec]
    liftIO $ createDirectoryIfMissing True  dir

    let name = (dir </> "spec" <.> ".essence")
    writeModel (Just name) spec
    return (dir </> "spec.essence")

toolchain :: MonadIO m => ToolchainData -> m ToolchainResult
toolchain ToolchainData{..} = do
  toolchainDir <- liftIO getToolchainDir

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

getToolchainDir :: IO FilePath
getToolchainDir = lookupEnv "PARAM_GEN_SCRIPTS" >>= \case
                  Nothing -> do
                    fp <- getDataDir
                    setEnv "PARAM_GEN_SCRIPTS" fp
                    return $ fp </> "toolchain"
                  Just fp -> return $ fp </> "toolchain"

kindsList :: [String]
kindsList = do
  let names = dataTypeConstrs . dataTypeOf $ (error "kindI" :: KindI)
  map show names

statusesList :: [String]
statusesList = do
  let names = dataTypeConstrs . dataTypeOf $ (error "StatusI" :: StatusI)
  map show names
