{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module TestGen.Args(parseArgs, TArgs(..)) where


import Build_autoversion(buildDateRFC2822,autoVersion)
import System.Console.CmdArgs.Implicit

data TArgs_ = TArgs_{base_directory  :: Maybe FilePath
                 , total_time    :: Maybe Int
                 , per_spec_time :: Maybe Int
                 , rseed         :: Maybe Int
                 , cores         :: Maybe Int
                }  deriving (Show, Data, Typeable)

data TArgs  = TArgs{baseDirectory_  :: FilePath
                 , totalTime_       :: Int
                 , perSpecTime_     :: Int
                 , rseed_           :: Int
                 , cores_           :: Int
                }  deriving (Show, Data, Typeable)

argsDef :: TArgs_
argsDef  = TArgs_
             { base_directory = def &= help "Base Directory"
             , total_time     = def &= help "Total time to use"
             , per_spec_time  = def &= help "Total time to spend on each spec"
             , rseed          = def &= help "Seed to Use"
             , cores          = def &= help "Cores to use"
             }
         &= summary (unlines ["TestSample Version 1.0"
                             , "Git version: " ++ autoVersion
                             , "Build date: "  ++ buildDateRFC2822
                             ])
         &= helpArg [name "h"]

parseArgs :: IO TArgs
parseArgs = do
    TArgs_{..} <- cmdArgs argsDef

    let baseDirectory_ = f base_directory "base-directory"
    let totalTime_     = f total_time     "total-time"
    let perSpecTime_   = f per_spec_time  "per-spec-time"
    let rseed_         = f rseed          "rseed"
    let cores_         = f cores          "cores"


    return $ TArgs{baseDirectory_,totalTime_, perSpecTime_,rseed_,cores_}

    where
    f Nothing n = error $ "--" ++ n ++ " needs to be specifed"
    f (Just v) _   = v
