{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs?
module TestGen.Reduce.Args(parseArgs) where

import Build_autoversion(buildDateRFC2822,autoVersion)
import System.Console.CmdArgs.Implicit

import qualified TestGen.Prelude as P
import TestGen.Reduce.Data
import TestGen.Helpers.Runner(KindI, StatusI)


data RArgs_ = RArgs_
                 { spec_directory   :: Maybe FilePath
                 , output_directory :: Maybe FilePath
                 , per_spec_time    :: Maybe Int
                 , error_kind       :: Maybe KindI
                 , error_status     :: Maybe StatusI
                 , cores            :: Maybe Int
                 , new_conjure      :: Bool
                 , seed             :: Maybe Int
                }  deriving (Show, Data, Typeable)


argsDef :: RArgs_
argsDef  = RArgs_
             { spec_directory   = def &= help "{R} Directory of spec.essence"
                                      &= name "b"
             , output_directory = def &= help "{R} Output directory"
                                      &= name "o"
             , per_spec_time    = def &= help "{R} Total time to spend on each spec"
                                      &= name "p"
             , error_kind       = def &= help "{R} The Error kind e.g RefineRandom_"
                                      &= explicit
                                      &= name "kind"
             , error_status     = def &= help "{R} The Error status e.g. ParseError_"
                                      &= explicit
                                      &= name "status"
             , seed             = def &= help "{R} Seed to Use"
                                      &= explicit
                                      &= name "seed"
             , cores            = def &= help "{R} Cores to use"
                                      &= name "c"
             , new_conjure      = def &= help "Use new conjure, must be called conjureNew"
                                      &= name "n"
             }

         &= summary (unlines ["TestReduce, the test case simplifier"
                             , "Git version: " ++ autoVersion
                             , "Build date: "  ++ buildDateRFC2822
                             , "{R} required"
                             ])
         &= helpArg [name "h"]
         &= program "testSample"

parseArgs :: IO RState
parseArgs = do
    a@RArgs_{..} <- cmdArgs argsDef
    print a
    let oErrKind_   = f error_kind "error_kind"
        oErrStatus_ = f error_status "error_status"
        oErrEprime_ = Nothing
        cores_      = f cores "cores"
        newConjure_ = new_conjure
        outputDir_  = f output_directory "output_directory"
        specDir_    = f spec_directory "spec_directory"
        rgen_        = mkrGen (f seed "seed")


    let res = P.def{oErrKind_, oErrStatus_, oErrEprime_, cores_
                   ,newConjure_, outputDir_, specDir_, rgen_}
    return res


    where
    f Nothing n = error $ "--" ++ n ++ " needs to be specified"
    f (Just v) _   = v
