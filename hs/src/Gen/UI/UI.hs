{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-} -- stup id cmdargs
module Gen.UI.UI where

import Build_autoversion      (autoVersion)
import Gen.IO.Toolchain       (KindI (..), RefineType (..), StatusI (..), ToolchainOutput(..))
import Gen.Imports
import System.Console.CmdArgs hiding (Default (..))
import Gen.Essence.UIData

data UI
  = Essence
    {
      total_time         :: Int
    , per_spec_time      :: Int
    , _mode              :: EssenceMode
    , _gen_type          :: GenType
    , keep_passing       :: Bool
    , output_directory   :: Maybe FilePath
    , domain_depth       :: Int
    , expression_depth   :: Int
    , _cores             :: Maybe Int
    , _seed              :: Maybe Int
    , total_is_cpu_time  :: Bool

    , toolchain_ouput    :: ToolchainOutput
    , binaries_directory :: Maybe FilePath
    , old_conjure        :: Bool
    , limit_time         :: Maybe Int
    , no_csv             :: Bool
    , given_dir          :: Maybe FilePath
    , reduce_as_well     :: Maybe Int
    , _weightings        :: Maybe FilePath
    , db_directory       :: Maybe FilePath
    , log_level          :: LogLevel
    , delete_immediately :: Maybe FilePath
    , list_kinds         :: Bool
    , list_statuses      :: Bool
    , strict_checking    :: Bool
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

    , total_time_may     :: Maybe Int
    , total_is_cpu_time  :: Bool

    , output_directory   :: Maybe FilePath
    , _cores             :: Maybe Int
    , _seed              :: Maybe Int

    , keep_passing       :: Bool
    , delete_steps       :: Bool
    , delete_others      :: Bool
    , toolchain_ouput    :: ToolchainOutput
    , binaries_directory :: Maybe FilePath
    , limit_time         :: Maybe Int
    , always_compact     :: Bool

    , no_csv             :: Bool
    , db_directory       :: Maybe FilePath
    , db_passing_in      :: Maybe FilePath
    , db_only_passing    :: Bool
    , from_essence       :: Bool
    , no_check           :: Bool
    , log_level          :: LogLevel
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
    , _cores             :: Maybe Int
    , _seed              :: Maybe Int

    , keep_passing       :: Bool
    , toolchain_ouput    :: ToolchainOutput
    , binaries_directory :: Maybe FilePath
    , limit_time         :: Maybe Int
    , no_csv             :: Bool
    , db_directory       :: Maybe FilePath
    , from_essence       :: Bool
    , log_level          :: LogLevel
    }

  | Instance_Undirected
    { essence_path       :: FilePath
    , per_model_time     :: Int
    , iterations         :: Int
    , mode               :: String
    , _cores             :: Maybe Int
    , output_directory   :: Maybe FilePath
    , limit_time         :: Maybe Int
    , log_level          :: LogLevel
    , _seed              :: Maybe Int
    , pre_solutions      :: Maybe FilePath
    , given_dir          :: Maybe FilePath
    , param_gen_time     :: Int
    }

  | Instance_Nsample
    { essence_path       :: FilePath
    , per_model_time     :: Int
    , iterations         :: Int
    , mode               :: String
    , _cores             :: Maybe Int
    , output_directory   :: Maybe FilePath
    , limit_time         :: Maybe Int
    , log_level          :: LogLevel
    , _seed              :: Maybe Int
    , influence_radius   :: Int
    , pre_solutions      :: Maybe FilePath
    , given_dir          :: Maybe FilePath
    , param_gen_time     :: Int
    }

  | Instance_NoRacing
    { essence_path       :: FilePath
    , iterations         :: Int
    , output_directory   :: Maybe FilePath
    , limit_time         :: Maybe Int
    , log_level          :: LogLevel
    , _seed              :: Maybe Int
    , param_gen_time     :: Int
    }

  | Instance_AllSolutions
    { essence_path       :: FilePath
    , output_directory   :: Maybe FilePath
    , limit_time         :: Maybe Int
    , log_level          :: LogLevel
    }

  | Instance_Summary
    { input_directory    :: FilePath
    , limit_time         :: Maybe Int
    , log_level          :: LogLevel
    }
  | SpecEE
    { directories     :: [FilePath]
    , print_specs     :: Bool
    , meta_only       :: Bool
    , limit_time      :: Maybe Int
    , verboseOpt      :: Bool
    , strict_checking :: Bool
    }
  | Link
    { directory    :: FilePath
    , reduced_only :: Bool
    , limit_time   :: Maybe Int
    }
  | Solver
    {
      essence_path   :: FilePath
    , solution_path  :: Maybe FilePath
    , print_solution :: Bool
    , limit_time     :: Maybe Int
    }
  | Weights
    { default_weights  :: Bool
    , all_weights      :: Bool
    , eprime_like      :: Bool
    , by_type          :: Maybe Int
    , output_directory :: Maybe FilePath

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

    , _cores             :: Maybe Int
    , _seed              :: Maybe Int

    , toolchain_ouput    :: ToolchainOutput
    , binaries_directory :: Maybe FilePath
    , old_conjure        :: Bool
    , limit_time         :: Maybe Int
    , dry_run            :: Bool
    }
  | Script_UpdateChoices
    {
      choices_in_  :: FilePath
    , choices_out_ :: FilePath
    , limit_time   :: Maybe Int
    }
  | Script_CreateDBHashes
    {
      directory    :: FilePath
    , limit_time   :: Maybe Int
    }
  | Script_RemoveDups
    {
      dups_      :: [FilePath]
    , dups_kind  :: DupKind
    , limit_time :: Maybe Int
    }
  | Script_SMAC
    {
       s_output_directory  :: FilePath
    ,  s_eprime            :: String
    ,  s_instance_specific :: String
    ,  s_cutoff_time       :: Double
    ,  s_cutoff_length     :: Double
    ,  s_seed              :: Int
    ,  s_param_arr         :: [String]
    ,  limit_time          :: Maybe Int
    ,  log_level           :: LogLevel
    }
  deriving (Show, Data, Typeable)


data DupKind = DupSolve | DupRefine
  deriving (Show, Data, Typeable)

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
     , _gen_type          = def     &= name "gen-type"
                                    &= typ "type"
                                    &= groupname "Generation"
                                    &= explicit
                                    &= help "The generator to use, only SecondGen is allowed"
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
     , domain_depth       = 3       &= name "domain-depth"
                                    &= name "d"
                                    &= groupname "Generation"
                                    &= help "Max depth of an Domain (default 3) "
                                    &= explicit
     , expression_depth   = 4       &= name "expression-depth"
                                    &= name "e"
                                    &= groupname "Generation"
                                    &= help "Max depth of an Expression (default 4)"
                                    &= explicit
     , _cores             = Nothing &= name "cores"
                                    &= name "c"
                                    &= groupname "Required"
                                    &= explicit
                                    &= help "Number of cores to use, required unless CORES is set"
     , keep_passing       = False   &= name "keep-passing"
                                    &= groupname "Filters"
                                    &= explicit
                                    &= help "Keep non-failing test cases"
     , _seed              = def     &= name "seed"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Random Seed to use"
     , total_is_cpu_time = False    &= name "total-is-cpu-time"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "The total time is cpu time, not real time "

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
                                    &= groupname "Filters"
                                    &= explicit
                                    &= help "Instead of generating specs, act like the .spec.json files in the given dir were the generated specs, -t/--total-time can not be used with this option"
     , reduce_as_well    = Nothing  &= name "reduce-as-well"
                                    &= groupname "Generation"
                                    &= explicit
                                    &= help "Reduce the test case found sequentially"
     , _weightings       = Nothing  &= typFile
                                    &= name "w"
                                    &= name "weightings"
                                    &= groupname "Generation"
                                    &= explicit
                                    &= help "Weighting json key value pairs e.g. {\"TypeSet\":100}"
     , delete_immediately= Nothing  &= typFile
                                    &= name "X"
                                    &= name "delete-immediately"
                                    &= groupname "Filters"
                                    &= explicit
                                    &= help "Delete the specs that match the specifed kinds and statuses. The default is [[\"KindAny_\",\"NumberToLarge_\"], [\"KindAny_\",\"Timeout_\"] ] "
     , db_directory      = Nothing  &= name "db-dir"
                                    &= groupname "Other"
                                    &= typDir
                                    &= explicit
                                    &= help "Cache the result of running a spec. Useful for development"
     , log_level         = LogDebug &= name "log-level"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Logging level, default LogDebug"
     , list_statuses    = False     &= name "list-statuses"
                                    &= groupname "Filters"
                                    &= explicit
                                    &= help "Just list the statuses then exit"
     , list_kinds       = False     &= name "list-kinds"
                                    &= groupname "Filters"
                                    &= explicit
                                    &= help "Just list the kinds then exit"
     , strict_checking  = False     &= name "strict-type-checking"
                                    &= groupname "Other"
                                    &= explicit
                                    &= help "Crash when a type incorrect spec is generated/given"
     , toolchain_ouput    = enum
                            [
                              ToolchainScreen_ &= name "show-toolchain-output"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Show toolchain output (default)"
                            , ToolchainFile_   &= name "redirect-toolchain-output"
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
     , always_compact    = False     &= name "always-compact"
                                     &= groupname "Reduction"
                                     &= explicit
                                     &= help "Use compact if no choices are specified, cores should be set to 1 in this case. Also useful for specs that take a long time to produce multiple models"
     , list_kinds       = False      &= name "list-kinds"
                                     &= groupname "Reduction"
                                     &= explicit
                                     &= help "Just list the kinds then exit"
     , total_time_may    = Nothing   &= name "total-time"
                                     &= name "t"
                                     &= groupname "Timing"
                                     &= explicit
                                     &= help "Total time for running specs, if specifed, Otherwise specs are reduced until a fixed point is reached"
     , total_is_cpu_time = False     &= name "total-is-cpu-time"
                                     &= groupname "Timing"
                                     &= explicit
                                     &= help "The total time is cpu time, not real time "
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
     , _cores           = Nothing    &= name "cores"
                                     &= name "c"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Number of cores to use, defualts to 1 unless CORES is set"
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
     , db_passing_in      = Nothing  &= name "db-passing-in"
                                     &= groupname "Other"
                                     &= typFile
                                     &= explicit
                                     &= help "Addition specs marked as passing,  Useful for development"

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
     , keep_passing       = False    &= name "keep-passing"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Keep non-failing test"
     , delete_steps       = False    &= name "delete-steps"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Delete intermediate test case generated which have the same error"
     , delete_others      = False    &= name "delete-others"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Delete test cases found during reductionwhich have a different error"
     , from_essence       = False    &= name "from-essence"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Convert spec.essence to json (spec.spec.json), automatically for convenience"
     , no_check           = False    &= name "no-check"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Don't check if the error still occurs by running the give specification"
     , log_level          = LogDebug &= name "log-level"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Logging level, default LogDebug"
     , toolchain_ouput    = enum
                            [
                              ToolchainScreen_ &= name "show-toolchain-output"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Show toolchain output (default)"
                            , ToolchainFile_   &= name "redirect-toolchain-output"
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
     , _cores           = Nothing    &= name "cores"
                                     &= name "c"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Number of cores to use, defualts to 1 unless CORES is set"
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
     , keep_passing     = False      &= name "keep-passing"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Keep non-failing test"
     , from_essence       = False    &= name "from-essence"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Convert spec.essence to json (spec.spec.json), automatically for convenience"
     , log_level         = LogDebug  &= name "log-level"
                                     &= groupname "Other"
                                     &= explicit
                                     &= help "Logging level, default LogDebug"
     , toolchain_ouput    = enum
                            [
                              ToolchainScreen_ &= name "show-toolchain-output"
                                               &= explicit
                                               &= groupname "Output"
                                               &= help "Show toolchain output (default)"
                            , ToolchainFile_   &= name "redirect-toolchain-output"
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


  , Instance_Undirected
     { essence_path     = def     &= typ "essence"
                                  &= argPos 0
     , per_model_time   = def     &= name "per-model-time"
                                  &= name "p"
                                  &= groupname "Required"
                                  &= explicit
                                  &= help "Time per model"
     , iterations       = def     &= name "iterations"
                                  &= name "i"
                                  &= groupname "Required"
                                  &= explicit
                                  &= help "Number of races"
     , mode             = def     &= name "mode"
                                  &= name "m"
                                  &= groupname "Required"
                                  &= help "The suffix of the models directory"
     , _cores           = Nothing &= name "cores"
                                  &= name "c"
                                  &= groupname "Required"
                                  &= explicit
                                  &= help "Number of cores to use, required unless CORES is set"
     , output_directory = def     &= typDir
                                  &= name "output-directory"
                                  &= name "o"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Output directory default is %F_%H-%M_%s e.g. 2015-03-23_01-04_1427072681"
     , limit_time       = Nothing &= name "limit-time"
                                  &= explicit
                                  &= help "Time limit in seconds of CPU time of this program"
                                  &= groupname "Other"
                                  &= explicit
     , log_level       = LogDebug &= name "log-level"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Logging level, default LogDebug"
     , _seed            = def     &= name "seed"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Random Seed to use"
     , pre_solutions    = def     &= typDir
                                  &= name "generated-solutions"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Sample only from these solution generated by `gen instance-allsols`"
     , given_dir        = Nothing &= typDir
                                  &= name "given"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Instead of generating params, act like the .param files in the given dir were the generated specs, the number of .param files much mathc the value give to -i"
    , param_gen_time    = 300     &= name "param-generation-time"
                                  &= name "g"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Maximum time given to generate each instance, default 300 seconds"
     } &= name "instance-undirected"
       &= help "Generate discriminating instance for the given essence specification using a baseline method"

  , Instance_Nsample
     { essence_path     = def     &= typ "essence"
                                  &= argPos 0
     , per_model_time   = def     &= name "per-model-time"
                                  &= name "p"
                                  &= groupname "Required"
                                  &= explicit
                                  &= help "Time per model"
     , iterations       = def     &= name "iterations"
                                  &= name "i"
                                  &= groupname "Required"
                                  &= explicit
                                  &= help "Number of races"
     , influence_radius = def     &= name "influence_radius"
                                  &= name "f"
                                  &= groupname "Required"
                                  &= explicit
                                  &= help "Number of races"
     , mode             = def     &= name "mode"
                                  &= name "m"
                                  &= groupname "Required"
                                  &= help "The suffix of the models directory"
     , _cores           = Nothing &= name "cores"
                                  &= name "c"
                                  &= groupname "Required"
                                  &= explicit
                                  &= help "Number of cores to use, required unless CORES is set"
     , output_directory = def     &= typDir
                                  &= name "output-directory"
                                  &= name "o"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Output directory default is %F_%H-%M_%s e.g. 2015-03-23_01-04_1427072681"
     , limit_time       = Nothing &= name "limit-time"
                                  &= explicit
                                  &= help "Time limit in seconds of CPU time of this program"
                                  &= groupname "Other"
                                  &= explicit
     , log_level       = LogDebug &= name "log-level"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Logging level, default LogDebug"
     , _seed              = def   &= name "seed"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Random Seed to use"
     , pre_solutions    =  def    &= typDir
                                  &= name "generated-solutions"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Sample only from these solution generated by `gen instance-allsols`"
     , given_dir        = Nothing &= typDir
                                  &= name "given"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Instead of generating params, act like the .param files in the given dir were the generated specs, the number of .param files much mathc the value give to -i"
    , param_gen_time    = 300     &= name "param-generation-time"
                                  &= name "g"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Maximum time given to generate each instance, default 300 seconds"
     } &= name "instance-nsample"
       &= help "Generate discriminating instance for the given essence specification using the nsample method"

  , Instance_NoRacing
     { essence_path     = def     &= typ "essence"
                                  &= argPos 0
     , iterations       = def     &= name "iterations"
                                  &= name "i"
                                  &= groupname "Required"
                                  &= explicit
                                  &= help "Number of instances to generate"
     , output_directory = def     &= typDir
                                  &= name "output-directory"
                                  &= name "o"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Output directory default is %F_%H-%M_%s e.g. 2015-03-23_01-04_1427072681"
     , limit_time       = Nothing &= name "limit-time"
                                  &= explicit
                                  &= help "Time limit in seconds of CPU time of this program"
                                  &= groupname "Other"
                                  &= explicit
     , log_level       = LogDebug &= name "log-level"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Logging level, default LogDebug"
     , _seed            = def     &= name "seed"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Random Seed to use"
    , param_gen_time    = 300     &= name "param-generation-time"
                                  &= name "g"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Maximum time given to generate each instance, default 300 seconds"
     } &= name "instance-noRacing"
       &= help "Only Generate instances i.e. no racing. Results will be in the _params sub-directory of -o"


  , Instance_AllSolutions
     { essence_path     = def     &= typ "essence"
                                  &= argPos 0
     , output_directory = def     &= typDir
                                  &= name "output-directory"
                                  &= name "o"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Output directory default is %F_%H-%M_%s e.g. 2015-03-23_01-04_1427072681"
     , limit_time       = Nothing &= name "limit-time"
                                  &= explicit
                                  &= help "Time limit in seconds of CPU time of this program"
                                  &= groupname "Other"
                                  &= explicit
     , log_level       = LogDebug &= name "log-level"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Logging level, default LogDebug"
     } &= name "instance-allsols"
       &= help "Generate the *script* and data required to create all solutions. This will usually requires TB(s) of space"

  , Instance_Summary
     { input_directory = def      &= typDir
                                  &= name "output-directory"
                                  &= name "o"
                                  &= groupname "Required"
                                  &= explicit
                                  &= help "Output directory"
     , limit_time       = Nothing &= name "limit-time"
                                  &= explicit
                                  &= help "Time limit in seconds of CPU time of this program"
                                  &= groupname "Other"
                                  &= explicit
     , log_level       = LogDebug &= name "log-level"
                                  &= groupname "Other"
                                  &= explicit
                                  &= help "Logging level, default LogDebug"
     } &= name "instance-summary"
       &= help "Generate a summary of the results, placed in the summary sub-directory."


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
                           &= name "s"
                           &= explicit
                           &= help "Print the the spec before and after conversion"
     , meta_only   = False &= name "meta-only"
                           &= explicit
                           &= help "Only create the .meta.json file from previously created .spec.json"
     , verboseOpt  = False &= name "verbose"
                           &= explicit
                           &= help "Show more infomation, and info on error if any"

     , strict_checking = False &= name "strict-type-checking"
                               &= explicit
                               &= help "Crash when a type incorrect spec is generated/given"

     } &= explicit
       &= name "json"
       &= help "Create .spec.json and .meta.json files for each .essence file recursively"


  , Link
     {
       directory    = "Errors" &= typDir
                               &= argPos 0
     , reduced_only = False    &= name "reduced_only"
                               &= name "r"
                               &= explicit
                               &= help "Link only reduced specs"
     , limit_time   = def      &= name "limit-time"
                               &= explicit
                               &= help "Time limit in seconds of CPU time of this program"

     } &= explicit
       &= name "link"
       &= help "Classify a dir of specs by creating symlinks (using .meta.json files)"


  , Solver
     { essence_path   = def     &= typ "essence"
                                &= argPos 0
     , solution_path  = Nothing &= typ "solution"
                                &= name "output-solution"
                                &= name "o"
                                &= explicit
                                &= help "Where to place writes solution, defaults to essence's path with last extension change"
     , print_solution = False   &= name "print-solution"
                                &= name "s"
                                &= explicit
                                &= help "Print the solution as well"
     , limit_time     = Nothing &= name "limit-time"
                                &= explicit
                                &= help "Time limit in seconds of CPU time of this program"
     } &= explicit
       &= name "solve"
       &= help "Solve a .essence file, and write the first solution to file if there is one"


  , Weights
     {
       default_weights  = False &= name "default-weights"
                                &= groupname "Weights"
                                &= help "Output the default weighting"
                                &= explicit
     , all_weights      = False &= name "all-weights"
                                &= groupname "Weights"
                                &= help "Output the all weighting"
                                &= explicit
     , eprime_like      = False &= name "eprimeish"
                                &= groupname "Weights"
                                &= help "Output a weighting file for eprime like specs"
                                &= explicit
     , by_type          = def   &= name "by-type"
                                &= groupname "Weights"
                                &= help "Output weighting files with n types"
                                &= explicit
     , output_directory  = def  &= typDir
                                &= name "output-directory"
                                &= name "o"
                                &= groupname "Other"
                                &= explicit
                                &= help "Output directory default is weights"
     , limit_time  = def        &= name "limit-time"
                                &= explicit
                                &= help "Time limit in seconds of CPU time of this program"
     } &= explicit
       &= name "weights"
       &= help "Create weighting file for use with gen essence --weightings"


  , Script_CreateDBHashes
     {
       directory  = def     &= typDir
                            &= argPos 0
     , limit_time = Nothing &= name "limit-time"
                            &= explicit
                            &= help "Time limit in seconds of CPU time of this program"

     } &= explicit
       &= name "script-createDbHashes"
       &= help "Make a db.json using the hashes of spec.spec.json files. This can be given to `gen essence --db-dir` so that same spec is not generated again"

  , Script_RemoveDups
     {
       dups_       = def    &= typDir
                            &= explicit
                            &= name "d"
                            &= name "dup"
                            &= groupname "Required"
                            &= help "Directories to check"
     , dups_kind  = enum [
                  DupRefine &= name "refine"
                            &= explicit
                            &= groupname "Error Kind (defulat refine)"
                            &= help "Refinement Errors"
                , DupSolve  &= name "solve"
                            &= explicit
                            &= groupname "Error Kind (defulat refine)"
                            &= help "All other Errors"
                ]
     , limit_time  = Nothing &= name "limit-time"
                             &= groupname "Other"
                             &= explicit
                             &= help "Time limit in seconds of CPU time of this program"

     } &= explicit
       &= name "script-removeDups"
       &= help "Remove Duplicate Errors"


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
     , _cores             = Nothing &= name "cores"
                                    &= name "c"
                                    &= groupname "Required"
                                    &= explicit
                                    &= help "Number of cores to use, required unless CORES is set"

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


  , Script_SMAC
     { s_output_directory   = def      &= typ "output_directory"
                                       &= argPos 0
     ,  s_eprime            = def      &= typ "eprime"
                                       &= argPos 1
     ,  s_instance_specific = def      &= typ "instance_specific"
                                       &= argPos 2
     ,  s_cutoff_time       = def      &= typ "cutoff_time"
                                       &= argPos 3
     ,  s_cutoff_length     = def      &= typ "cutoff_length"
                                       &= argPos 4
     ,  s_seed              = def      &= typ "seed"
                                       &= argPos 5
     ,  s_param_arr         = def      &= typ "param_arr"
                                       &= args
     , limit_time           = Nothing  &= name "limit-time"
                                       &= explicit
                                       &= help "Time limit in seconds of CPU time of this program"
                                       &= groupname "Other"
                                       &= explicit
     , log_level            = LogDebug &= name "log-level"
                                       &= groupname "Other"
                                       &= explicit
                                       &= help "Logging level, default LogDebug"
     } &= name "script-smac-process"
       &= help "wrapper script for the SMAC param tunner "


  , Script_UpdateChoices
     {
       choices_in_  = def     &= typ "IN-JSON"
                              &= argPos 0
      ,choices_out_ = def     &= typ "OUT-JSON"
                              &= argPos 1
     , limit_time   = Nothing &= name "limit-time"
                              &= groupname "Other"
                              &= explicit
                              &= help "Time limit in seconds of CPU time of this program"

     } &= explicit
       &= name "script-updateChoices"
       &= help "Convert AnsweredRepr to AnsweredReprStored from IN to OUT"

  ] &= program "gen"
    &= summary (unlines ["Gen 0.9.1",  "Git version: " ++ autoVersion])
    &= helpArg [name "h"]
