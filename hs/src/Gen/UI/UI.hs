{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-} -- stup id cmdargs
module Gen.UI.UI where

import Build_autoversion      (autoVersion)
import Gen.IO.Toolchain       (KindI (..), RefineType (..), StatusI (..))
import Gen.Prelude
import System.Console.CmdArgs hiding (Default (..))


data UI
  = Essence
    {
      total_time         :: Int
    , per_spec_time      :: Int
    , _mode              :: ModeChoice
    ,  output_directory  :: Maybe FilePath
    , _size              :: Int  -- should not be greater then 5
    , _cores             :: Int
    , _seed              :: Maybe Int
    , delete_passing     :: Bool
    , total_is_real_time :: Bool

    , toolchain_ouput    :: ToolchainOutput
    , binaries_directory :: Maybe FilePath
    , old_conjure        :: Bool
    , limit_time         :: Maybe Int
    , no_csv             :: Bool
    , given_dir          :: Maybe FilePath
    }

  | Instance
    { output_directory   :: Maybe FilePath

    , binaries_directory :: Maybe FilePath
    , old_conjure        :: Bool
    , limit_time         :: Maybe Int
    }

  | Reduce
    {
      per_spec_time      :: Int
    , spec_directory     :: FilePath
    , error_kind         :: KindI
    , error_status       :: StatusI
    , error_choices      :: Maybe FilePath

    , list_kinds         :: Bool
    , list_statuses      :: Bool

    , output_directory   :: Maybe FilePath
    , _cores             :: Int
    , _seed              :: Maybe Int

    , delete_passing     :: Bool
    , delete_steps       :: Bool
    , toolchain_ouput    :: ToolchainOutput
    , binaries_directory :: Maybe FilePath
    , limit_time         :: Maybe Int
    , no_csv             :: Bool
    , db_directory       :: Maybe FilePath
    , db_only_passing    :: Bool
    }

  | Generalise
    {
      per_spec_time      :: Int
    , spec_directory     :: FilePath
    , error_kind         :: KindI
    , error_status       :: StatusI
    , error_choices      :: Maybe FilePath

    , list_kinds         :: Bool
    , list_statuses      :: Bool

    , output_directory   :: Maybe FilePath
    , _cores             :: Int
    , _seed              :: Maybe Int

    , delete_passing     :: Bool
    , toolchain_ouput    :: ToolchainOutput
    , binaries_directory :: Maybe FilePath
    , limit_time         :: Maybe Int
    , no_csv             :: Bool
    , db_directory       :: Maybe FilePath
    }
  | Link
    { directories :: [FilePath]
    , limit_time  :: Maybe Int
    }
  | SpecEE
    { directories :: [FilePath]
    , print_specs :: Bool
    , meta_only   :: Bool
    , limit_time  :: Maybe Int
    }
  | Script_Toolchain
    {
      essence_path       :: FilePath
    , total_time         :: Int

    , essence_param      :: Maybe FilePath
    , choices_path       :: Maybe FilePath

    , refine_type        :: RefineType
    , output_directory   :: Maybe FilePath

    , _cores             :: Int
    , _seed              :: Maybe Int

    , toolchain_ouput    :: ToolchainOutput
    , binaries_directory :: Maybe FilePath
    , old_conjure        :: Bool
    , limit_time         :: Maybe Int
    , dry_run            :: Bool
    }
  | Script_ToolchainRecheck
    {
      essence_path       :: FilePath
    , _cores             :: Int
    , output_directory   :: Maybe FilePath

    , toolchain_ouput    :: ToolchainOutput
    , binaries_directory :: Maybe FilePath
    , old_conjure        :: Bool
    , limit_time         :: Maybe Int
    , dry_run            :: Bool
    }
  deriving (Show, Data, Typeable)

data ModeChoice =
          TypeCheck
        | Refine
        | Solve
  deriving (Show, Data, Typeable,Eq)

instance Default ModeChoice where
    def = Solve



ui :: UI
ui  = modes

  [ Essence
     {
       output_directory  = def      &= typDir
                                    &= name "output-directory"
                                    &= name "o"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Output directory default is %F_%H-%M_%s e.g. 2015-03-23_01-04_1427072681"
     , _mode              = def     &= name "mode"
                                    &= typ "mode"
                                    &= groupname "Generation"
                                    &= explicit
                                    &= help "Mode to use, one of typecheck, refine, solve (default) "
     , total_time         = def     &= name "total-time"
                                    &= name "t"
                                    &= groupname "Required"
                                    &= explicit
                                    &= help "Total time for running specs. If --given is used this can not be specified"
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
     , delete_passing     = False   &= name "delete-passing"
                                    &= name "D"
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
                                    &= typDir
                                    &= explicit
                                    &= help "Directory to prepend to the $PATH before running progams."
     , old_conjure        = False   &= name "old-conjure"
                                    &= groupname "Other"
                                    &= typDir
                                    &= explicit
                                    &= help "Use old conjure"
     , limit_time         = Nothing &= name "limit-time"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Time limit in seconds of CPU time of this program"
     , no_csv             = False   &= name "no-save-csv"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Don't save version of the tools used, The script if used requires bash"
     , given_dir         = Nothing  &= typDir
                                    &= name "given"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Instead of generating specs, act like the .spec.json files in the given dir were the generated specs, -t/--total-time can not be used with this option"
     , toolchain_ouput    = enum
                            [
                              ToolchainScreen_ &= name "show-toolchain-output"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Show toolchain output (default)"
                            , ToolchainFile_   &= name "redirect-toolchain-output"
                                               &= name "F"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Redirect toolchain output to file"
                            , ToolchainNull_   &= name "nul-toolchain-output"
                                               &= name "N"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Discard toolchain output"
                            ]

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
     , error_choices    = Nothing    &= typFile
                                     &= name "choices"
                                     &= groupname "Reduction"
                                     &= explicit
                                     &= help "Use log-following if given otherwise refine all models"
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
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Output directory default is %F_%H-%M_%s e.g. 2015-03-23_01-04_1427072681"
     , per_spec_time    = def        &= name "per-spec-time"
                                     &= name "p"
                                     &= groupname "Required"
                                     &= explicit
                                     &= help "Time per Spec"
     , _cores           = 1          &= name "cores"
                                     &= name "c"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Number of cores to Use"
     , _seed            = def        &= name "seed"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Random Seed to use"
     , binaries_directory = Nothing  &= name "bin-dir"
                                     &= groupname "Other"
                                     &= typDir
                                     &= explicit
                                     &= help "Directory to prepend the $PATH before running progams."
     , db_directory       = Nothing  &= name "db-dir"
                                     &= groupname "Other"
                                     &= typDir
                                     &= explicit
                                     &= help "Cache the result of running a spec. Useful for development"
     , db_only_passing    = False    &= name "db-only-passing"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Only store the which specs have no errors"
     , limit_time       = Nothing    &= name "limit-time"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Time limit in seconds of CPU time of this program"
     , no_csv              = False   &= name "no-save-csv"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Don't save version of the tools used, The script if used requires bash"
     , delete_passing     = False    &= name "delete-passing"
                                     &= name "D"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Delete non failing test cases as soon as they have been generated"
     , delete_steps       = False    &= name "delete-steps"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Delete intermediate test case generated which have the same error"
     , toolchain_ouput    = enum
                            [
                              ToolchainScreen_ &= name "show-toolchain-output"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Show toolchain output (default)"
                            , ToolchainFile_   &= name "redirect-toolchain-output"
                                               &= name "F"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Redirect toolchain output to file"
                            , ToolchainNull_   &= name "null-toolchain-output"
                                               &= name "N"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Discard toolchain output"
                            ]
     } &= explicit
       &= name "reduce"
       &= help "Reduces a .spec.json file"

  , Generalise
     { spec_directory   = def        &= typ "SPEC-DIR"
                                     &= argPos 0
     , error_status     = StatusAny_ &= name "status"
                                     &= groupname "Generalisation"
                                     &= explicit
                                     &= help "The error Kind e.g. RefineRandom_"
     , error_kind       = KindAny_   &= name "kind"
                                     &= groupname "Generalisation"
                                     &= explicit
                                     &= help "The error Status e.g. ParseError_"
     , error_choices    = Nothing    &= typFile
                                     &= name "choices"
                                     &= groupname "Generalisation"
                                     &= explicit
                                     &= help "Use log-following if given otherwise refine all models"
     , list_statuses    = False      &= name "list-statuses"
                                     &= groupname "Generalisation"
                                     &= explicit
                                     &= help "Just list the statuses then exit"
     , list_kinds       = False      &= name "list-kinds"
                                     &= groupname "Generalisation"
                                     &= explicit
                                     &= help "Just list the kinds then exit"
     , output_directory = def        &= typDir
                                     &= name "output-directory"
                                     &= name "o"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Output directory default is %F_%H-%M_%s e.g. 2015-03-23_01-04_1427072681"
     , per_spec_time    = def        &= name "per-spec-time"
                                     &= name "p"
                                     &= groupname "Required"
                                     &= explicit
                                     &= help "Time per Spec"
     , _cores           = 1          &= name "cores"
                                     &= name "c"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Number of cores to Use"
     , _seed            = def        &= name "seed"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Random Seed to use"
     , binaries_directory = Nothing  &= name "bin-dir"
                                     &= groupname "Other"
                                     &= typDir
                                     &= explicit
                                     &= help "Directory to prepend the $PATH before running progams."
     , db_directory       = Nothing  &= name "db-dir"
                                     &= groupname "Other"
                                     &= typDir
                                     &= explicit
                                     &= help "Cache the result of running a spec. Useful for development"
     , limit_time       = Nothing    &= name "limit-time"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Time limit in seconds of CPU time of this program"
     , no_csv              = False   &= name "no-save-csv"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Don't save version of the tools used, The script if used requires bash"
     , delete_passing     = False    &= name "delete-passing"
                                     &= name "D"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Delete non failing test cases as soon as they have been generated"
     , toolchain_ouput    = enum
                            [
                              ToolchainScreen_ &= name "show-toolchain-output"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Show toolchain output (default)"
                            , ToolchainFile_   &= name "redirect-toolchain-output"
                                               &= name "F"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Redirect toolchain output to file"
                            , ToolchainNull_   &= name "null-toolchain-output"
                                               &= name "N"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Discard toolchain output"
                            ]
     } &= explicit
       &= name "generalise"
       &= help "Generalises a .spec.json file"


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
     , meta_only   = False &= name "meta-only"
                           &= explicit
                           &= help "Only create the .meta.json file from previously created .spec.json"

     } &= explicit
       &= name "json"
       &= help "Create .spec.json and .meta.json files for each .essence file recursively"

  , Script_Toolchain
     { essence_path       = def     &= typ "essence"
                                    &= argPos 0
     , output_directory   = def     &= typDir
                                    &= name "output-directory"
                                    &= name "o"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Output directory default is %F_%H-%M_%s e.g. 2015-03-23_01-04_1427072681"
     , total_time         = def     &= name "total-time"
                                    &= name "t"
                                    &= groupname "Required"
                                    &= explicit
                                    &= help "Total time for running the toolchain"
     , _cores             = def     &= name "cores"
                                    &= name "c"
                                    &= groupname "Required"
                                    &= explicit
                                    &= help "Number of cores to Use"

     , essence_param      = def     &= typFile
                                    &= name "essence-param"
                                    &= groupname "Input"
                                    &= explicit
                                    &= help "Essence param to use"
     , refine_type        = enum
                            [
                              Refine_Solve     &= name "refine-solve"
                                               &= explicit
                                               &= groupname "Control"
                                               &= help "Refine, solve and validate the spec (default)"
                            , Refine_Only      &= name "refine-only"
                                               &= explicit
                                               &= groupname "Control"
                                               &= help "Only refine the spec"
                            , Refine_All       &= name "refine-all"
                                               &= explicit
                                               &= groupname "Control"
                                               &= help "Only generate all model of the spec"
                            , Refine_Solve_All &= name "refine-solve-all"
                                               &= explicit
                                               &= groupname "Control"
                                               &= help "Refine, solve and validate the spec while genrating all models"
                            ]
     , dry_run            = False   &= name "dry-run"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Just output the command that would be executed"
     , choices_path      = Nothing  &= name "choices"
                                    &= explicit
                                    &= typ "FILE"
                                    &= groupname "Input"
                                    &= help "Use an .eprime/.json file to try to pick the given choices as far as possible"

     , _seed              = def     &= name "seed"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Random Seed to use"
     , binaries_directory = Nothing &= name "bin-dir"
                                    &= groupname "Other"
                                    &= typDir
                                    &= explicit
                                    &= help "Directory to prepend the $PATH before running progams."
     , old_conjure        = False   &= name "old-conjure"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Use old conjure"
     , limit_time         = Nothing &= name "limit-time"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Time limit in seconds of CPU time of this program"
     , toolchain_ouput    = enum
                            [
                              ToolchainScreen_ &= name "show-toolchain-output"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Show toolchain output (default)"
                            , ToolchainFile_   &= name "redirect-toolchain-output"
                                               &= name "F"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Redirect toolchain output to file"
                            , ToolchainNull_   &= name "null-toolchain-output"
                                               &= name "N"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Discard toolchain output"
                            ]
     } &= explicit
       &= name "script-toolchain"
       &= help "Run the toolchain on an essence spec"
  , Script_ToolchainRecheck
     { essence_path       = def     &= typDir
                                    &= argPos 0
     , output_directory   = def     &= typDir
                                    &= name "output-directory"
                                    &= name "o"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Output directory default is %F_%H-%M_%s e.g. 2015-03-23_01-04_1427072681"
     , _cores             = def     &= name "cores"
                                    &= name "c"
                                    &= groupname "Required"
                                    &= explicit
                                    &= help "Number of cores to Use"
     , binaries_directory = Nothing &= name "bin-dir"
                                    &= groupname "Other"
                                    &= typDir
                                    &= explicit
                                    &= help "Directory to prepend the $PATH before running progams."
     , old_conjure        = False   &= name "old-conjure"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Use old conjure"
     , limit_time         = Nothing &= name "limit-time"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Time limit in seconds of CPU time of this program"
     , dry_run            = False   &= name "dry-run"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Just output the command that would be executed"
     , toolchain_ouput    = enum
                            [
                              ToolchainScreen_ &= name "show-toolchain-output"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Show toolchain output (default)"
                            , ToolchainFile_   &= name "redirect-toolchain-output"
                                               &= name "F"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Redirect toolchain output to file"
                            , ToolchainNull_   &= name "null-toolchain-output"
                                               &= name "N"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Discard toolchain output"
                            ]
     } &= explicit
       &= name "script-recheck"
       &= help "Reruns the toolchain with previous used settings"


  ] &= program "gen"
    &= summary (unlines ["Gen v3.2",  "Git version: " ++ autoVersion])
    &= helpArg [name "h"]
