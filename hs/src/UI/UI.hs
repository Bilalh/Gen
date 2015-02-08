{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs
module UI.UI where

import TestGen.Prelude

import TestGen.Helpers.Runner(KindI(..), StatusI(..))

import System.Console.CmdArgs hiding ( Default(..) )


data UI
  =
    Essence
      { output_directory :: FilePath
      , limit_time       :: Maybe Int
      }

  | Instance
      { output_directory :: FilePath
      , limit_time       :: Maybe Int
      }

  | Reduce
      { spec_directory :: FilePath
      , error_kind     :: KindI
      , error_status   :: StatusI

      , list_kinds    :: Bool
      , list_statuses :: Bool

      , output_directory :: FilePath
      , per_spec_time    :: Int
      , old_conjure      :: Bool
      , cores            :: Int
      , seed             :: Int
      , limit_time       :: Maybe Int
      }
  | Link
      { directories :: [FilePath]
      , limit_time  :: Maybe Int
      }
  | Meta
      { directories :: [FilePath]
      , limit_time  :: Maybe Int
      }
  | SpecEE
      { directories :: [FilePath]
      , limit_time  :: Maybe Int
      }
  deriving (Show, Data, Typeable)

ui :: UI
ui  = modes

  [ Reduce
     { spec_directory   = def        &= typDir
                                     &= argPos 0
     , error_status     = StatusAny_ &= name "status"
                                     &= groupname "Reduction"
                                     &= explicit
                                     &= help "The error Kind e.g. RefineRandom_"
     , error_kind       = KindAny_   &= name "kind"
                                     &= groupname "Reduction"
                                     &= explicit
                                     &= help "The error Status e.g. ParseError_"
     , list_statuses    = False      &= name "list-statuses"
                                     &= groupname "Reduction"
                                     &= explicit
                                     &= help "Just list the statuses then exit"
     , list_kinds       = False      &= name "list-kinds"
                                     &= groupname "Reduction"
                                     &= explicit
                                     &= help "Just list the kinds then exit"
     , output_directory = def        &= typDir
                                     &= name "output-directory"
                                     &= name "o"
                                     &= groupname "Stats"
                                     &= explicit
                                     &= help "Output directory "
     , per_spec_time    = def        &= name "per-spec-time"
                                     &= name "p"
                                     &= groupname "Stats"
                                     &= explicit
                                     &= help "Time per Spec"
     , cores            = def        &= name "cores"
                                     &= name "c"
                                     &= groupname "Stats"
                                     &= explicit
                                     &= help "Number of cores to Use"
     , seed             = def        &= name "seed"
                                     &= groupname "Stats"
                                     &= explicit
                                     &= help "Random Seed to use"
     , old_conjure      = False      &= name "old-conjure"
                                     &= groupname "Stats"
                                     &= explicit
                                     &= help "Use old conjure"
     , limit_time       = Nothing    &= name "limit-time"
                                     &= groupname "Stats"
                                     &= explicit
                                     &= help "Time limit in seconds of CPU time of this program"

     } &= explicit
       &= name "reduce"
       &= help "Reduces a spec.specE file"

  , Link
    {
      directories = def &= typDir
                        &= name "directory"
                        &= name "d"
                        &= explicit
                        &= help "Directories containing spec.meta.json files "
    , limit_time  = def &= name "limit-time"
                        &= groupname "Other"
                        &= explicit
                        &= help "Time limit in seconds of CPU time of this program"

    } &= explicit
      &= name "link"
      &= help "Classify the specs by creating symlinks (using .meta.json files)"

  , Meta
    {
      directories = def &= typDir
                        &= name "directory"
                        &= name "d"
                        &= explicit
                        &= help "Directories containing spec.specE files "
    , limit_time  = def &= name "limit-time"
                        &= groupname "Other"
                        &= explicit
                        &= help "Time limit in seconds of CPU time of this program"

    } &= explicit
      &= name "meta"
      &= help "Create .meta.json files for each .specE file"

  , SpecEE
    {
      directories = def &= typDir
                        &= name "directory"
                        &= name "d"
                        &= explicit
                        &= help "Directories containing spec.essence files "
    , limit_time  = def &= name "limit-time"
                        &= groupname "Other"
                        &= explicit
                        &= help "Time limit in seconds of CPU time of this program"

    } &= explicit
      &= name "specE"
      &= help "Create .specE files for each .essence file (not complete)"

  ] &= program "gen"
    &= summary "Gen the test case generator"
    &= helpArg [name "h"]
