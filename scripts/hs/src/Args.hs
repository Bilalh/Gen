{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Args(parseArgs) where

import Data
import System.Console.CmdArgs.Implicit

data Args = Args{base_directory  :: Maybe FilePath
                 , total_time    :: Maybe Float
                 , per_spec_time :: Maybe Int
                 , rseed         :: Maybe Int
                }  deriving (Show, Data, Typeable)


argsDef :: Args
argsDef  = Args
             { base_directory = def &= help "Base Directory"
             , total_time     = def &= help "Total time to use"
             , per_spec_time  = def &= help "Total time to spend on each spec"
             , rseed          = def &= help "Seed to Use"
             }
         &= summary "Generate Essence Spec for testing "


parseArgs :: IO GenGlobal
parseArgs = do
   Args{..} <- cmdArgs argsDef

   let base_directory1 = f base_directory "base-directory"
   let total_time1     = f total_time     "total-time"
   let per_spec_time1  = f per_spec_time  "per-spec-time"
   let rseed1          = f rseed          "rseed"

   return $ GenGlobal{gBase = base_directory1, gSeed = rseed1
                     , gTotalTime=total_time1, gSpecTime=per_spec_time1
                     , gErrors=[],gErrors_no_use=[],gErrors_timeout=[]}

    where
    f Nothing n = error $ "--" ++ n ++ " needs to be specifed"
    f (Just v) _   = v

