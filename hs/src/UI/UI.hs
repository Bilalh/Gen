{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs
module UI.UI where

import TestGen.Prelude

import TestGen.Helpers.Runner(KindI(..), StatusI(..))

import System.Console.CmdArgs hiding ( Default(..) )

import Build_autoversion(autoVersion)

data ModeChoice =
          TypeCheck
        | Refine
        | Solve
  deriving (Show, Data, Typeable,Eq)

instance Default ModeChoice where
    def = Solve

data UI
  = Essence
      { output_directory :: FilePath
      , _mode            :: ModeChoice

      , total_time         :: Int
      , per_spec_time      :: Int
      , _size              :: Int  -- should not be greater then 5
      , _cores             :: Int
      , _seed              :: Maybe Int
      , delete_passing     :: Bool
      , total_is_real_time :: Bool

      , binaries_directory :: Maybe FilePath
      , old_conjure        :: Bool
      , limit_time         :: Maybe Int
      }

  | Instance
      { output_directory   :: FilePath

      , binaries_directory :: Maybe FilePath
      , old_conjure        :: Bool
      , limit_time         :: Maybe Int
      }

  | Reduce
      { spec_directory :: FilePath
      , error_kind     :: KindI
      , error_status   :: StatusI

      , list_kinds    :: Bool
      , list_statuses :: Bool

      , output_directory   :: FilePath
      , per_spec_time      :: Int
      , _cores             :: Int
      , _seed              :: Maybe Int

      , binaries_directory :: Maybe FilePath
      , old_conjure        :: Bool
      , limit_time         :: Maybe Int
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
      , print_specs :: Bool
      , limit_time  :: Maybe Int
      }
  deriving (Show, Data, Typeable)

ui :: UI
ui  = modes

  [ Essence
     { output_directory   = def     &= typ "OUT-DIR"
                                    &= argPos 0
     , _mode              = def     &= name "mode"
                                    &= typ "mode"
                                    &= groupname "Generation"
                                    &= explicit
                                    &= help "Mode to use, one of typecheck, refine, solve (default) "
     , total_time         = def     &= name "total-time"
                                    &= name "t"
                                    &= groupname "Required"
                                    &= explicit
                                    &= help "Total time for running specs"
     , per_spec_time      = def     &= name "per-spec-time"
                                    &= name "p"
                                    &= groupname "Required"
                                    &= explicit
                                    &= help "Time per Spec"
     , _size              = 4       &= name "size"
                                    &= groupname "Generation"
                                    &= help "Max depth of an expression, 5 should be more then enough"
                                    &= explicit
     , _cores             = def     &= name "cores"
                                    &= name "c"
                                    &= groupname "Required"
                                    &= explicit
                                    &= help "Number of cores to use"
     , _seed              = def     &= name "seed"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Random Seed to use"
     , delete_passing     = False   &= name "delete_passing"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Delete non failing test cases as soon as they have been generated"
     , total_is_real_time = False   &= name "total-is-real-time"
                                    &= name "@"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "The total time is real time, not cpu time "

     , binaries_directory = Nothing &= name "bin-dir"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Directory to prepend to the $PATH before running progams."
     , old_conjure        = False   &= name "old-conjure"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Use old conjure"
     , limit_time         = Nothing &= name "limit-time"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Time limit in seconds of CPU time of this program"

     } &= explicit
       &= name "essence"
       &= help "Generates essence test cases"

  , Reduce
     { spec_directory   = def        &= typ "SPEC-DIR"
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
                                     &= groupname "Required"
                                     &= explicit
                                     &= help "Output directory "
     , per_spec_time    = def        &= name "per-spec-time"
                                     &= name "p"
                                     &= groupname "Required"
                                     &= explicit
                                     &= help "Time per Spec"
     , _cores           = def        &= name "cores"
                                     &= name "c"
                                     &= groupname "Required"
                                     &= explicit
                                     &= help "Number of cores to Use"
     , _seed            = def        &= name "seed"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Random Seed to use"
     , binaries_directory = Nothing  &= name "bin-dir"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Directory to prepend the $PATH before running progams."
     , old_conjure      = False      &= name "old-conjure"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Use old conjure"
     , limit_time       = Nothing    &= name "limit-time"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Time limit in seconds of CPU time of this program"

     } &= explicit
       &= name "reduce"
       &= help "Reduces a .spec.json file"

  , Link
    {
      directories = def &= typDir
                        &= name "directory"
                        &= name "d"
                        &= explicit
                        &= help "Directories containing spec.meta.json files "
    , limit_time  = def &= name "limit-time"
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
                        &= explicit
                        &= help "Time limit in seconds of CPU time of this program"

    } &= explicit
      &= name "meta"
      &= help "Create .meta.json files for each .specE file recursively"

  , SpecEE
    {
      directories = def   &= typDir
                          &= name "directory"
                          &= name "d"
                          &= explicit
                          &= help "Directories containing spec.essence files "
    , limit_time  = def   &= name "limit-time"
                          &= explicit
                          &= help "Time limit in seconds of CPU time of this program"
    , print_specs = False &= name "print-specs"
                          &= name "v"
                          &= explicit
                          &= help "Print the the spec before and after conversion"

    } &= explicit
      &= name "json"
      &= help "Create .spec.json files for each .essence file recursively"

  ] &= program "gen"
    &= summary (unlines ["Gen v2.1",  "Git version: " ++ autoVersion])
    &= helpArg [name "h"]
