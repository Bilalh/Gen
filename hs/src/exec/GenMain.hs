{-# LANGUAGE LambdaCase, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs?
module Main where

import Data.Time                  (formatTime, getCurrentTime)
import Data.Time.Format           (defaultTimeLocale)
import Gen.Arbitrary.Data
import Gen.Classify.AddMeta       (metaMain)
import Gen.Classify.AddMeta       (addMeta)
import Gen.Classify.AddSpecE      (addSpecJson, specEMain)
import Gen.Classify.Sorter        (getRecursiveContents, sorterMain')
import Gen.Classify.UpdateChoices (updateChoices)
import Gen.Essence.Generate       (generateEssence)
import Gen.Essence.UIData
import Gen.Generalise.Generalise  (generaliseMain)
import Gen.Imports
import Gen.IO.Formats             (readFromJSON)
import Gen.IO.RunResult           (giveDb, writeDB_)
import Gen.IO.Term
import Gen.IO.Toolchain           (KindI (..), StatusI (..), ToolchainOutput (..),
                                   doMeta, kindsList, statusesList)
import Gen.Reduce.Data            (RState (..), mkrGen)
import Gen.Reduce.FormatResults   (formatResults)
import Gen.Reduce.Reduce          (reduceMain)
import Gen.Solver.Solver          (SolverArgs (..), solverMain)
import Gen.UI.UI
import System.Console.CmdArgs     (cmdArgs)
import System.CPUTime             (getCPUTime)
import System.Directory           (getCurrentDirectory, setCurrentDirectory)
import System.Environment         (lookupEnv, withArgs)
import System.Exit                (exitFailure, exitSuccess, exitWith)
import System.FilePath            (replaceExtension, takeExtensions)
import System.Timeout             (timeout)
import Text.Printf                (printf)

import qualified Data.Set                as S
import qualified Gen.Essence.UIData      as EC
import qualified Gen.Essence.Weightings  as Weights
import qualified Gen.Generalise.Data     as E
import qualified Gen.IO.Toolchain        as Toolchain
import qualified Gen.IO.ToolchainRecheck as Recheck
import qualified Gen.Reduce.Data         as R



main :: IO ()
main = do
  getArgs >>= \case
    [] -> do
       args <- helpArg
       void $ withArgs [args] (cmdArgs ui)
    [x] | x `elem` [ "essence", "reduce", "link", "meta", "json", "generalise", "solve", "weights"
                   , "script-toolchain", "script-recheck", "script-updateChoices"] -> do
       args <- helpArg
       void $ withArgs [x, args] (cmdArgs ui)

    ["reduce", "--list-kinds"] -> do
        mapM_ (putStrLn) kindsList
        exitSuccess
    ["reduce", "--list-statuses"] -> do
        mapM_ (putStrLn) statusesList
        exitSuccess

    ["generalise", "--list-kinds"] -> do
        mapM_ (putStrLn) kindsList
        exitSuccess
    ["generalise", "--list-statuses"] -> do
        mapM_ (putStrLn) statusesList
        exitSuccess

    ["--toolchain-path"] -> do
         dir <- Toolchain.getToolchainDir Nothing
         putStrLn dir

    [_, "--toolchain-path"] -> do
         dir <- Toolchain.getToolchainDir Nothing
         putStrLn dir

    xs -> do
      newArgs <- replaceOldHelpArg xs
      input <- withArgs newArgs (cmdArgs ui)

      let workload = do
            putStrLn . show . vcat $ ["Command line options: ", pretty (groom input)]
            mainWithArgs input

      limiter (getLimit input) workload

   where


     getLimit (Essence{_mode=EC.TypeCheck_, given_dir=Just{}}) =
         error "--given and --mode typecheck can not be used together"
     getLimit x | Just i <- limit_time x, i <=0  = error "--limit-time must be > then 0"
     getLimit (Essence{_mode=EC.TypeCheck_, limit_time=Nothing, total_time=t}) = Just t
     getLimit input = limit_time input


limiter :: Maybe Int -> IO () -> IO ()
limiter Nothing f = f
limiter (Just sec) f  =  do
  putStrLn $ "Running with a timelimit of " ++ show sec ++ " seconds."
  res <- timeout (sec * 1000000) f
  case res of
    Nothing -> do
      cputime <- getCPUTime
      let
        -- cputime is returned in pico-seconds. arbitrary precision integer.
        -- divide by 10^9 first. use arbitrary precision integer arithmetic.
        -- do the last 10^3 division via double to get 3 significant digits
        -- after the integer part
        cputimeInSeconds :: Double
        cputimeInSeconds = fromInteger (cputime `div` 1000000000) / 1000
      putStrLn $ printf "Timed out. Total CPU time is %.3f seconds." cputimeInSeconds
    Just () -> return ()


mainWithArgs :: UI -> IO ()
mainWithArgs u@Essence{..} = do

  let ls = case given_dir of
             Nothing  -> [ aerr "-t|--total-time" (total_time == 0)]
             Just{} -> []

  fileErr <- sequence
            [
              dirExistsMay "--givens"  given_dir
            , dirExistsMay "--db_dir"  db_directory
            , dirExistsMay "--bin-dir" binaries_directory
            , fileExistsMay "--weightings/-w" _weightings
            ]

  let errors = catMaybes $
        [ aerr "-p|--per-spec-time"     (per_spec_time == 0)
        , aerr "--domain-depth > 0"     (domain_depth < 0)
        , aerr "--expression-depth >= 0" (expression_depth < 0)
        , aerr "--given and -t/--total-time can not be used together" (isJust given_dir && total_time /= 0)
        ]  ++ fileErr ++ ls


  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  out        <- giveOutputDirectory output_directory
  seed_      <- giveSeed _seed
  cores      <- giveCores u
  givenSpecs <- giveSpec given_dir

  ws <- case _weightings of
          Nothing -> def
          Just fp -> do
            readFromJSON fp

  let notUseful = S.fromList $ (Savilerow_, NumberToLarge_) : (KindAny_, NumberToLarge_) :
        [ (x,Timeout_)
        | x <- [ RefineCompact_, RefineAll_, RefineRandom_, RefineParam_, KindAny_] ]

  let config = EC.EssenceConfig
               { outputDirectory_ = out
               , mode_            = _mode
               , totalTime_       = total_time
               , perSpecTime_     = per_spec_time
               , cores_           = cores
               , seed_            = seed_

               , domainDepth_     = domain_depth
               , expressionDepth_ = expression_depth

               , totalIsRealTime    = not total_is_cpu_time
               , deletePassing_     = not keep_passing
               , binariesDirectory_ = binaries_directory
               , oldConjure_        = old_conjure
               , toolchainOutput_   = toolchain_ouput
               , notUseful          = notUseful
               , givenSpecs_        = givenSpecs
               , genType_           = _gen_type
               , reduceAsWell_      = reduce_as_well
               , dbDirectory_       = db_directory
               , logLevel           = log_level
               }



  doMeta out no_csv binaries_directory
  generateEssence ws config


mainWithArgs Instance{..} = do
  error . show . vcat $ ["gen instance not done yet" ]

mainWithArgs u@Reduce{..} = do

  if list_kinds then do
        mapM_ (putStrLn) kindsList
        exitSuccess
  else
      return ()

  if list_statuses then do
        mapM_ (putStrLn) statusesList
        exitSuccess
  else
      return ()

  fileErr <- catMaybes <$> sequence
            [
              dirExists     "spec_directory" spec_directory
            , fileExistsMay "choices" error_choices
            , dirExistsMay "--bin-dir" binaries_directory
            , case from_essence of
                True -> return Nothing
                False -> fileExists "spec.spec.json is required unless --from-essence is specifed" (spec_directory </> "spec.spec.json")
            ]


  let errors = catMaybes
        [ aerr "spec-directory" (null spec_directory)
        , aerr "-p|--per-spec-time >0" (per_spec_time == 0)
        , aerr "-t|--total-time > 0 if specified" (total_time_may == Just 0)
        , aerr "-@/--total-is-cpu-time requires -t/--total-time to be specified"
                   (total_is_cpu_time && total_time_may == Nothing)
        ] ++ fileErr

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  case from_essence of
    False -> return ()
    True  ->  addSpecJson False (spec_directory </> "spec.essence")
          >>  addMeta (spec_directory </> "spec.spec.json")

  seed_ <- giveSeed _seed
  db    <- giveDb db_directory db_passing_in
  out   <- giveOutputDirectory output_directory
  cores <- giveCores u

  let args = def{rconfig=
                 R.RConfig
                 { oErrKind_            = error_kind
                 ,oErrStatus_          = error_status
                 ,oErrChoices_         = error_choices
                 ,outputDir_           = out
                 ,specDir_             = spec_directory
                 ,specTime_            = per_spec_time
                 ,resultsDB_dir        = db_directory
                 ,totalIsRealTime_     = not total_is_cpu_time
                 ,R.cores_             = cores
                 ,R.binariesDirectory_ = binaries_directory
                 ,R.toolchainOutput_   = toolchain_ouput
                 ,R.deletePassing_     = not keep_passing
                 }
                ,rgen_                = mkrGen (seed_)
                ,resultsDB_           = db
                ,mostReducedChoices_  = error_choices
                ,timeLeft_            = total_time_may
                }

  doMeta out no_csv binaries_directory

  state <- reduceMain (not no_check) args
  writeDB_ db_only_passing db_directory (resultsDB_  state)
  void $ formatResults (delete_steps) state

mainWithArgs Generalise{..} = do

  if list_kinds then do
        mapM_ (putStrLn) kindsList
        exitSuccess
  else
      return ()

  if list_statuses then do
        mapM_ (putStrLn) statusesList
        exitSuccess
  else
      return ()

  fileErr <- catMaybes <$> sequence
            [
              dirExists     "spec_directory" spec_directory
            , fileExistsMay "choices" error_choices
            , dirExistsMay "--bin-dir" binaries_directory
            , case from_essence of
                True -> return Nothing
                False -> fileExists "spec.spec.json is required unless --from-essence is specifed" (spec_directory </> "spec.spec.json")
            ]

  let errors = catMaybes
        [ aerr "spec-directory" (null spec_directory)
        , aerr "-p|--per-spec-time" (per_spec_time == 0)
        ] ++ fileErr

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  case from_essence of
    False -> return ()
    True  ->  addSpecJson False (spec_directory </> "spec.essence")
          >>  addMeta (spec_directory </> "spec.spec.json")

  seed_ <- giveSeed _seed
  db    <- giveDb db_directory Nothing
  out   <- giveOutputDirectory output_directory
  cores <- giveCores ui

  let args :: E.GState =
             def{E.rconfig=
                 R.RConfig
                 { oErrKind_            = error_kind
                 ,oErrStatus_          = error_status
                 ,oErrChoices_         = error_choices
                 ,outputDir_           = out
                 ,specDir_             = spec_directory
                 ,specTime_            = per_spec_time
                 ,resultsDB_dir        = db_directory
                 ,totalIsRealTime_     = True
                 ,R.cores_             = cores
                 ,R.binariesDirectory_ = binaries_directory
                 ,R.toolchainOutput_   = toolchain_ouput
                 ,R.deletePassing_     = not keep_passing
                 }
                ,E.resultsDB_          = db
                ,E.choicesToUse_       = error_choices
                ,E.rgen_               = mkrGen (seed_)
                }

  doMeta out no_csv binaries_directory

  state <- generaliseMain args
  writeDB_ False db_directory (E.resultsDB_  state)

mainWithArgs Solver{..} = do

  fileErr <- catMaybes <$> sequence
            [
              fileExists    "essence path" essence_path
            ]

  case fileErr of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  outPath <- case solution_path of
               Just p  -> return p
               Nothing -> do
                 return $ replaceExtension essence_path ".solution"

  solverMain SolverArgs{
                   essencePath   = essence_path
                 , printSolution = print_solution
                 , solutionPath  = outPath
                 }

mainWithArgs Link{..} = do
  errors <- catMaybes <$> (mapM (dirExists "-d") directories )

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  sorterMain' directories

mainWithArgs Weights{..} = do

  let errors = catMaybes
        [
          aerr "--by-type must >=1" (fromMaybe False . fmap (<=0) $ by_type )
        ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  let out = fromMaybe "weights" output_directory

  let save = Weights.save out

  case by_type of
    Nothing -> return ()
    Just i  -> do
      save $ (Weights.byType i)

  when default_weights $ save Weights.defaults
  when eprime_like     $ save Weights.eprimeish
  when all_weights     $ save Weights.every


mainWithArgs SpecEE{..} = do
  errors <- catMaybes <$> (mapM (dirExists "-d") directories )

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  if meta_only then
      return ()
  else do
    putStrLn "Creating .spec.json files"
    specEMain verboseOpt print_specs directories

  putStrLn "Creating .meta.json files"
  metaMain verboseOpt directories


mainWithArgs u@Script_Toolchain{..} = do

  fileErr <- catMaybes <$> sequence
            [
              fileExists    "essence path" essence_path
            , fileExistsMay "--essence-param" essence_param
            , fileExistsMay "--choices" choices_path
            , dirExistsMay "--bin-dir" binaries_directory
            ]

  let errors = catMaybes
        [ aerr "-t|--total-time" (total_time == 0)
        ] ++ fileErr


  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  out <- giveOutputDirectory output_directory
  cores <- giveCores u

  (code,_) <- Toolchain.toolchain Toolchain.ToolchainData
           {
             Toolchain.essencePath       = essence_path
           , Toolchain.outputDirectory   = out
           , Toolchain.toolchainTime     = total_time
           , Toolchain.essenceParam      = essence_param
           , Toolchain.refineType        = f refine_type choices_path
           , Toolchain.cores             = cores
           , Toolchain.seed              = _seed
           , Toolchain.binariesDirectory = binaries_directory
           , Toolchain.oldConjure        = old_conjure
           , Toolchain.toolchainOutput   = toolchain_ouput
           , Toolchain.choicesPath       = choices_path
           , dryRun                      = dry_run
           }
  exitWith code

  where
    f :: Toolchain.RefineType -> Maybe FilePath -> Toolchain.RefineType
    f r Nothing = r
    f Toolchain.Refine_All _       = error "--choices make no sense with --refine-all"
    f Toolchain.Refine_Solve_All _ = error "--choices make no sense with --refine-solve-all"
    f r _ = r

mainWithArgs Script_ToolchainRecheck{..} = do
  fileErr <- catMaybes <$> sequence
            [
              fileExists    "essence path" essence_path
            , dirExistsMay "--bin-dir" binaries_directory
            ]


  case fileErr of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  out <- giveOutputDirectory output_directory
  cores <- giveCores ui

  (code,_) <- Recheck.toolchainRecheck Recheck.RecheckData
           {
             Recheck.essencePath       = essence_path
           , Recheck.outputDirectory   = out
           , Recheck.cores             = cores
           , Recheck.binariesDirectory = binaries_directory
           , Recheck.oldConjure        = old_conjure
           , Recheck.toolchainOutput   = toolchain_ouput
           , Recheck.dryRun            = dry_run
           }
  exitWith code

mainWithArgs Script_UpdateChoices{..} = do

  errors <- catMaybes <$> sequence
            [
              fileExists    "First argument " choices_in_
            ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  updateChoices choices_in_ choices_out_


aerr :: String -> Bool -> Maybe String
aerr n b | b = Just $ "Error: " ++ n
aerr _ _     = Nothing

dirExistsMay :: String -> Maybe FilePath -> IO (Maybe String)
dirExistsMay _ Nothing  = return Nothing
dirExistsMay s (Just x) = dirExists s x

dirExists :: String -> FilePath -> IO (Maybe String)
dirExists s x = do
  b <- doesDirectoryExist x
  return $ aerr (s ++ " ~ Directory does not exist: " ++ x ) (not b)

fileExistsMay :: String -> Maybe FilePath -> IO (Maybe String)
fileExistsMay _ Nothing  = return Nothing
fileExistsMay s (Just x) = fileExists s x

fileExists ::String -> FilePath -> IO (Maybe String)
fileExists s x = do
  b <- doesFileExist x
  return $ aerr (s ++ " ~ File does not exist: " ++ x ) (not b)



giveSeed :: Maybe Int -> IO Int
giveSeed (Just s)  = return s
giveSeed Nothing = randomRIO (0,2^(31 :: Int)-1)

giveOutputDirectory :: Maybe FilePath -> IO FilePath
giveOutputDirectory (Just fp) = return fp
giveOutputDirectory Nothing   = do
  t <- getCurrentTime
  return $ formatTime defaultTimeLocale "%F_%H-%M_%s" t


giveSpec :: Maybe FilePath -> IO (Maybe [FilePath])
giveSpec Nothing  = return Nothing
giveSpec (Just path)  = do
  files <- getRecursiveContents path
  case [ f | f <- files, (takeExtensions $ f) == ".spec.json" ] of
    [] -> error . show . vcat  $[ "No .spec.json files in " <+>  pretty path
                                , "Did you run ` gen json -d" <+> pretty path <+> "`?" ]
    xs -> return $ Just xs

giveCores :: UI -> IO Int
giveCores u = do
  lookupEnv "CORES" >>= \case
    Nothing  -> ci_only u
    Just ""  -> ci_only u
    Just str ->
     case readMay str of
       Nothing -> error $ "CORES set, but unable to be parsed: " ++ str
       Just cores_env  -> do
         let cores_ci = _cores u
         case (cores_env,  cores_ci) of
           (0,Just 0)   -> error $ "CORES and -c/--cores are invaild (both 0)"
           (0,Nothing)  -> error $ "-c/--cores is required unless CORES is set"
           (i, Nothing) -> return i
           (i,Just j) | i == j -> return j
           (i,Just j) -> error . show  $ nn "CORES:" i
                                      <+> nn "-c/--cores:" j
                                      <+> "--> not consistent"




  where
    ci_only :: UI -> IO Int
    ci_only Reduce{} =  do
      case _cores u of
        Nothing -> return 1
        Just i  -> return i

    ci_only _ =  do
      case _cores u of
        Nothing -> error $ "-c/--cores is required unless CORES is set"
        Just i  -> return i


_essenceDebug :: IO ()
_essenceDebug = do
    let ec = Essence
             { output_directory   = Just "__/solve"
             , _mode              = EC.Solve_

             , total_time         = 60
             , per_spec_time      = 10
             , domain_depth       = 3
             , expression_depth   = 4
             , _cores             = Just 1
             , _seed              = Just 44

             , keep_passing       = True
             , binaries_directory = Nothing
             , old_conjure        = False
             , limit_time         = Nothing
             , total_is_cpu_time  = False
             , toolchain_ouput    = ToolchainNull_
             , no_csv             = False
             , given_dir          = Nothing
             , _gen_type          = SecondGen
             , reduce_as_well     = Nothing
             , _weightings        = Nothing
             , db_directory       = Nothing
             , log_level          = LogDebug
             }
    limiter (limit_time ec) (mainWithArgs ec)

_givenDebug :: IO ()
_givenDebug = do
    let ec = Essence
             { output_directory   = Just "/Users/bilalh/Desktop/Results/_notable/zz"
             , _mode              = EC.Refine_

             , total_time         = 0
             , per_spec_time      = 30
             , domain_depth       = 3
             , expression_depth   = 4
             , _cores             = Just 1
             , _seed              = Just 44

             , keep_passing       = False
             , binaries_directory = Nothing
             , old_conjure        = False
             , limit_time         = Nothing
             , total_is_cpu_time  = False
             , toolchain_ouput    = ToolchainNull_
             , no_csv             = True
             , given_dir          = Just "/Users/bilalh/Desktop/Results/_notable/_new/2015-05-11_01-33_1431308031/_errors/RefineCompact_/RuleApplication_/1431310206_12"
             , _gen_type          = def
             , reduce_as_well     = Just 60
             , _weightings        = Nothing
             , db_directory       = Nothing
             , log_level          = LogDebug
             }
    limiter (limit_time ec) (mainWithArgs ec)


_reduceDebug :: IO ()
_reduceDebug = do
  cur <- getCurrentDirectory
  if "__/reduceDebug" `isInfixOf` cur then
      return ()
  else do
    createDirectoryIfMissing True "__/reduceDebug"
    setCurrentDirectory "__/reduceDebug"

  let ec =
       Reduce{per_spec_time = 30, spec_directory = "/Users/bilalh/Desktop/Results/_notable/_new/with_crash/res/base/2015-05-26_02-08_1432602516/_errors/RefineCompact_/RuleApplication_/1432602517_54_r-.model000000.choices/1432602529_2039",
              error_kind = RefineCompact_, error_status = RuleApplication_,
              error_choices = Just "/Users/bilalh/Desktop/Results/_notable/_new/with_crash/res/base/2015-05-26_02-08_1432602516/_errors/RefineCompact_/RuleApplication_/1432602517_54_r-.model000000.choices/1432602529_2039/follow.choices.json", list_kinds = False,
              list_statuses = False, total_time_may = Nothing,
              total_is_cpu_time = False, output_directory = Nothing,
              _cores = Just 1, _seed = Nothing, keep_passing = False,
              delete_steps = True, toolchain_ouput = ToolchainNull_,
              binaries_directory = Nothing, limit_time = Nothing, no_csv = False,
              db_directory = Just "db", db_passing_in = Nothing,
              db_only_passing = False, from_essence = False,no_check=True}
  limiter (limit_time ec) (mainWithArgs ec)
