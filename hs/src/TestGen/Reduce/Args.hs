{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs?
module TestGen.Reduce.Args(parseArgs) where

import Build_autoversion(autoVersion)
import System.Console.CmdArgs.Implicit as I

import qualified TestGen.Prelude as P
import TestGen.Reduce.Data
import TestGen.Helpers.Runner(KindI(..), StatusI(..))

import Data.Data

import System.Exit(exitSuccess)

data RArgs_ = RArgs_
                 { spec_directory   :: FilePath
                 , output_directory :: Maybe FilePath
                 , per_spec_time    :: Maybe Int
                 , error_kind       :: KindI
                 , error_status     :: StatusI
                 , cores            :: Maybe Int
                 , new_conjure      :: Bool
                 , seed             :: Maybe Int
                 , list_kinds       :: Bool
                 , list_statuses    :: Bool
                }  deriving (Show, Data, Typeable)


argsDef :: RArgs_
argsDef  = RArgs_
             { spec_directory  =
                   def      &= typ "SpecE_directory"
                            &= argPos 0
             , output_directory =
                 def        &= help "{R} Output directory"
                            &= typDir
                            &= name "o"
             , per_spec_time    =
                 def        &= help "{R} Total time to spend on each spec"
                            &= name "p"
             , error_kind       =
                 KindAny_   &= help "The error Kind e.g. RefineRandom_"
                            &= explicit
                            &= name "kind"
             , error_status     =
                 StatusAny_ &= help "The error Status e.g. ParseError_"
                            &= explicit
                            &= name "status"
             , seed             =
                 def        &= help "{R} Seed to use"
                            &= explicit
                            &= name "seed"
             , cores            =
                 def        &= help "{R} Cores to use"
                            &= name "c"
             , new_conjure      =
                 def        &= help "Use new conjure, must be called conjureNew"
                            &= name "n"
             , list_kinds       =
                 def        &= help "Just list the kinds then exit"
                            &= explicit
                            &= name "list-kinds"
             , list_statuses    =
                 def        &= help "Just list the statuses then exit"
                            &= explicit
                            &= name "list-statuses"
             }

         &= summary (unlines ["testReduce, the test case simplifier"
                             , "Git version: " ++ autoVersion
                             -- , "Build date: "  ++ buildDateRFC2822
                             , "{R} required"
                             , "The input directory must have a file called spec.specE"
                             ])
         &= helpArg [name "h"]
         &= program "testReduce"

parseArgs :: IO RState
parseArgs = do
    a@RArgs_{..} <- cmdArgs argsDef
    putStrLn "Command line options passed"
    print a
    putStrLn ""

    l1 <- doKinds list_kinds
    l2 <- doStatuses list_statuses
    if l1 || l2 then
        exitSuccess
    else do
      let oErrKind_   = error_kind
          oErrStatus_ = error_status
          oErrEprime_ = Nothing
          cores_      = f cores "cores"
          newConjure_ = new_conjure
          outputDir_  = f output_directory "output_directory"
          specDir_    = spec_directory
          rgen_       = mkrGen (f seed "seed")
          specTime_   = f per_spec_time "per_spec_time"


      let res = P.def{oErrKind_, oErrStatus_, oErrEprime_, cores_
                     ,newConjure_, outputDir_, specDir_, rgen_,specTime_}
      return res


    where
    f Nothing n = error $ "--" ++ n ++ " needs to be specified"
    f (Just v) _ = v


    doKinds False = return False
    doKinds True  = do
      let names = dataTypeConstrs . dataTypeOf $ (undefined :: KindI)
      putStrLn "Kinds:"
      mapM_ (putStrLn . show) names
      putStrLn ""
      return True


    doStatuses False = return False
    doStatuses True  = do
      putStrLn "Statuses:"
      let names = dataTypeConstrs . dataTypeOf $ (undefined :: StatusI)
      mapM_ (putStrLn . show) names
      putStrLn ""
      return True
