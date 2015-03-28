{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.IO.ToolchainRecheck (
    toolchainRecheck
  , RecheckData(..)
  )where

import Data.Data
import Gen.IO.Formats
import Gen.IO.Toolchain
import Gen.Prelude
import System.Exit      (ExitCode, exitSuccess)
import System.Process   (showCommandForUser)



toolchainRecheck :: MonadIO m => RecheckData -> m (ExitCode, ToolchainResult)
toolchainRecheck RecheckData{..} = do
  toolchainDir <- liftIO $ getToolchainDir binariesDirectory

  let script = toolchainDir </> "toolchain_recheck.py"
  let args = [ essencePath
             , outputDirectory
             , "--num_cores", (show cores)
             ] ++ oldConjureArgs oldConjure
               ++ argsMay "--bin_dir" binariesDirectory


  liftIO . putStrLn $ "Running: " ++ showCommandForUser script args
  when dryRun $ liftIO exitSuccess

  code   <- runCommand script args (outputArg toolchainOutput outputDirectory)
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


argsMay :: String -> Maybe String -> [String]
argsMay _    Nothing   = []
argsMay flag (Just s)  = [flag,s]

oldConjureArgs :: Bool -> [String]
oldConjureArgs True  = []
oldConjureArgs False = ["--new_conjure"]


data RecheckData = RecheckData
    {
      essencePath        :: FilePath
    , outputDirectory    :: FilePath

    , cores              :: Int
    , binariesDirectory  :: Maybe FilePath
    , oldConjure         :: Bool
    , toolchainOutput    :: ToolchainOutput
    , dryRun             :: Bool
    }
  deriving (Show, Eq, Generic, Typeable, Data)

instance Default RecheckData where
    def = RecheckData
          {
            essencePath       = $never
          , outputDirectory   = $never
          , cores             = $never
          , binariesDirectory = Nothing
          , oldConjure        = False
          , toolchainOutput   = def
          , dryRun            = False
          }
