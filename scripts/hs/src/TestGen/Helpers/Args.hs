{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs?
module TestGen.Helpers.Args(parseArgs, TArgs(..)) where


import Build_autoversion(buildDateRFC2822,autoVersion)
import System.Console.CmdArgs.Implicit

data TArgs_ = TArgs_{base_directory  :: Maybe FilePath
                 , total_time     :: Maybe Int
                 , per_spec_time  :: Maybe Int
                 , rseed          :: Maybe Int
                 , size           :: Maybe Int
                 , cores          :: Maybe Int
                 , typecheck_only :: Maybe Int
                 , run_tool_chain :: Bool
                 , new_conjure    :: Bool
                }  deriving (Show, Data, Typeable)

data TArgs  = TArgs{baseDirectory_  :: FilePath
                 , totalTime_       :: Int
                 , perSpecTime_     :: Int
                 , rseed_           :: Maybe Int
                 , size_            :: Int
                 , cores_           :: Int
                 , typecheckOnly_   :: Maybe Int
                 , runToolchain_    :: Bool
                 , newConjure_      :: Bool
                }  deriving (Show, Data, Typeable)

argsDef :: TArgs_
argsDef  = TArgs_
             { base_directory = def &= help "Base Directory"
             , total_time     = def &= help "Total time to use" 
                                    &= name "t"
             , per_spec_time  = def &= help "Total time to spend on each spec"
             , rseed          = def &= help "Seed to Use"
             , size           = def &= help "Max depth (5 should be large enough) "
             , cores          = def &= help "Cores to use"
             , typecheck_only = def &= help "Only typechecks the generated specs" 
                                    &= name "y"
             , run_tool_chain = def &= help "Run Conjure(refine) + SR + Minion + translate + validate + consistently checking"
                                    &= name "a"
             , new_conjure    = def &= help "Use new conjure, must be called conjureNew"
                                    &= name "n"
             }
         &= summary (unlines ["TestSample, the generator of failing test cases"
                             , "Git version: " ++ autoVersion
                             , "Build date: "  ++ buildDateRFC2822
                             ])
         &= helpArg [name "h"]
         &= program "testSample"

parseArgs :: IO TArgs
parseArgs = do
    TArgs_{..} <- cmdArgs argsDef

    let baseDirectory_  = f base_directory "base-directory"
    let totalTime_      = f total_time     "total-time"
    let perSpecTime_    = f per_spec_time  "per-spec-time"
    let rseed_          = rseed
    let size_           = f size "size"
    let cores_          = f cores          "cores"
    let typecheckOnly_  = typecheck_only 
    let runToolchain_   = run_tool_chain
    let newConjure_     = new_conjure

    return $ TArgs{baseDirectory_,totalTime_, perSpecTime_,rseed_,cores_
                  , typecheckOnly_, size_, runToolchain_, newConjure_}

    where
    f Nothing n = error $ "--" ++ n ++ " needs to be specified"
    f (Just v) _   = v
