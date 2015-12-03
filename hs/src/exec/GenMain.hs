{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs?
module Main where

import Data.Time                    (formatTime, getCurrentTime)
import Data.Time.Format             (defaultTimeLocale)
import Gen.Classify.AddMeta         (metaMain)
import Gen.Classify.AddMeta         (addMeta)
import Gen.Classify.AddSpecE        (addSpecJson, specEMain)
import Gen.Classify.CreateDbHashes  (createDbHashesMain)
import Gen.Classify.Sorter          (getRecursiveContents, sorterMain)
import Gen.Classify.UpdateChoices   (updateChoices)
import Gen.Essence.Generate         (generateEssence)
import Gen.Essence.UIData
import Gen.Generalise.Generalise    (generaliseMain)
import Gen.Imports
import Gen.Instance.AllSolutions    (createAllSolutionScript, readSolutionCounts)
import Gen.Instance.Data
import Gen.Instance.Nsample         (Nsample (..))
import Gen.Instance.Point           (giveParam, readPoint)
import Gen.Instance.Results.Results (showResults)
import Gen.Instance.UI              (instances_no_racing, makeProvider, runMethod)
import Gen.Instance.Undirected      (Undirected (..))
import Gen.IO.Dups                  (deleteDups2, refineDups, solveDups)
import Gen.IO.FindCompact           (findCompact)
import Gen.IO.Formats               (readFromJSON)
import Gen.IO.RunResult             (giveDb, writeDB_)
import Gen.IO.SmacProcess           (smacProcess)
import Gen.IO.Term
import Gen.IO.Toolchain             (KindI (..), StatusI (..), ToolchainOutput (..),
                                     doMeta, kindsList, statusesList)
import Gen.Reduce.Data              (RState (..))
import Gen.Reduce.FormatResults     (formatResults)
import Gen.Reduce.Random            (runRndGen)
import Gen.Reduce.Reduce            (reduceMain)
import Gen.Solver.Solver            (SolverArgs (..), solverMain)
import Gen.UI.UI
import System.Console.CmdArgs       (cmdArgs)
import System.CPUTime               (getCPUTime)
import System.Directory             (getCurrentDirectory, makeAbsolute,
                                     setCurrentDirectory)
import System.Environment           (lookupEnv, setEnv, withArgs)
import System.Exit                  (exitFailure, exitSuccess, exitWith)
import System.FilePath              (replaceExtension, replaceFileName, takeBaseName,
                                     takeExtension, takeExtensions)
import System.Timeout               (timeout)
import Text.Printf                  (printf)

import qualified Data.Set               as S
import qualified Gen.Essence.UIData     as EC
import qualified Gen.Essence.Weightings as Weights
import qualified Gen.Generalise.Data    as E
import qualified Gen.IO.Toolchain       as Toolchain
import qualified Gen.Reduce.Data        as R


main :: IO ()
main = do
  getArgs >>= \case
    [] -> do
       args <- helpArg
       void $ withArgs [args] (cmdArgs ui)
    [x] | x `elem` [ "essence", "reduce", "link", "meta", "json", "generalise", "solve"
                   , "weights" , "script-toolchain", "script-removeDups"
                   , "instance-nsample", "instance-undirected", "instance-summary"
                   , "script-createDbHashes", "script-updateChoices"
                   , "instance-allsols", "instance-noRacing", "script-smac-process"] -> do
       args <- helpArg
       void $ withArgs [x, args] (cmdArgs ui)

    [_, "--list-kinds"] -> do
        mapM_ (putStrLn) kindsList
        exitSuccess
    [_, "--list-statuses"] -> do
        mapM_ (putStrLn) statusesList
        exitSuccess

    ["--list-kinds"] -> do
        mapM_ (putStrLn) kindsList
        exitSuccess
    ["--list-statuses"] -> do
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

  let ls = case given_dir of
             Nothing  -> [ aerr "-t|--total-time" (total_time == 0)]
             Just{} -> []

  fileErr <- sequence
            [
              dirExistsMay "--givens"  given_dir
            , dirExistsMay "--db_dir"  db_directory
            , dirExistsMay "--bin-dir" binaries_directory
            , fileExistsMay "--delete-immediately/-X" delete_immediately
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

  notUseful <- case delete_immediately of
          Nothing -> return $ S.fromList $  [(KindAny_, NumberToLarge_), (KindAny_,Timeout_)]
          Just fp -> do
            vals <- readFromJSON fp
            when ( (KindAny_,StatusAny_) `S.member` vals ) $
                 error "Error (--delete_immediately/-X): Specifying (KindAny_,StatusAny_) would delete every generated specification."
            return vals

  nullParamGen toolchain_ouput

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
               , strictTypeChecking = strict_checking
               }



  doMeta out no_csv binaries_directory
  generateEssence ws config

mainWithArgs u@Instance_Undirected{..} = do

  cores  <- giveCores u
  common <- instanceCommon cores Instance_Common{..}

  seed_  <- giveSeed _seed
  runMethod seed_ log_level (Method common Undirected)

mainWithArgs u@Instance_Nsample{..} = do

  cores  <- giveCores u
  common <- instanceCommon cores Instance_Common{..}

  let errors = catMaybes
        [ aerr "-f|--influence-radius >0" (influence_radius <= 0)
        ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  let ns = Nsample{mInfluence_radius=influence_radius}


  seed_  <- giveSeed _seed
  runMethod seed_ log_level (Method common ns)

mainWithArgs Instance_NoRacing{..} = do
  essence <- makeAbsolute essence_path
  fileErr <- catMaybes <$> sequence
    [
      fileExists   "essence" essence
    ]

  let argErr = catMaybes
        [ aerr "-i|--iterations >0" (iterations == 0)
        ] ++ fileErr

  let errors = argErr ++ fileErr
  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure
  out   <- giveOutputDirectory output_directory >>= makeAbsolute
  createDirectoryIfMissing True out

  seed_  <- giveSeed _seed
  instances_no_racing essence_path iterations param_gen_time out seed_ log_level


mainWithArgs Instance_AllSolutions{..} = do
  essence <- makeAbsolute essence_path
  let info_path   = replaceFileName essence "info.json"
  fileErr <- catMaybes <$> sequence
    [
      fileExists   "essence" essence
    , fileExists   "info.json needs to be next to the essence" info_path
    ]

  case fileErr of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  out  <- makeAbsolute =<< case output_directory of
            Just i  -> return i
            Nothing -> return $ "allsols@" ++ takeBaseName essence_path

  i :: VarInfo <- readFromJSON info_path
  p <- ignoreLogs $ makeProvider essence_path i

  runLoggerPipeIO log_level $
    createAllSolutionScript p i essence out


mainWithArgs Instance_Summary{..} = do
  fileErr <- catMaybes <$> sequence [
               dirExists "-o/--output-directory" input_directory
             , fileExists "Results.db missing" (input_directory </> "results.db")
             ]

  case fileErr of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  showResults input_directory

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

  mayParam <- giveParam spec_directory

  nullParamGen toolchain_ouput

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
                 ,alwaysCompact_       = always_compact
                 }
                ,resultsDB_           = db
                ,mostReducedChoices_  = error_choices
                ,timeLeft_            = total_time_may
                ,param_               = mayParam
                }

  doMeta out no_csv binaries_directory

  state <- runLoggerPipeIO log_level $ runRndGen seed_ $ reduceMain (not no_check) args
  writeDB_ db_only_passing db_directory (resultsDB_  state)
  void $ formatResults delete_steps delete_others state

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

  nullParamGen toolchain_ouput
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
                 ,alwaysCompact_       = False
                 }
                ,E.resultsDB_          = db
                ,E.choicesToUse_       = error_choices
                }

  doMeta out no_csv binaries_directory

  state <-  runLoggerPipeIO log_level $ runRndGen seed_ $ generaliseMain args
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
  errors <- catMaybes <$> (mapM (dirExists "-d") [directory]  )
  l <- maybeToList <$> ( dirExistsErr "Delete to recreate"  "link" )

  case errors ++ l  of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  sorterMain reduced_only [directory]

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

  nullParamGen toolchain_ouput
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

mainWithArgs Script_UpdateChoices{..} = do

  errors <- catMaybes <$> sequence
            [
              fileExists    "First argument " choices_in_
            ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  updateChoices choices_in_ choices_out_



mainWithArgs Script_CreateDBHashes{..} = do

  errors <- catMaybes <$> sequence
            [
              dirExists "First argument " directory
            ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  createDbHashesMain directory (directory </> "hashes")


mainWithArgs Script_RemoveDups{..} = do

  errors <- catMaybes <$> (mapM (dirExists "-d") dups_)

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  dups <- case dups_kind of
    DupRefine -> refineDups dups_
    DupSolve  -> solveDups  dups_
  putStrLn "Result"
  putStrLn $ show $ map pretty  dups
  deleteDups2 dups

mainWithArgs Script_SMAC{..} = do

  errors <- return []

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  runLoggerPipeIO log_level $
    smacProcess
      s_output_directory s_eprime s_instance_specific s_cutoff_time
      s_cutoff_length s_seed s_param_arr



instanceCommon :: Int -> Instance_Common -> IO MCommon
instanceCommon cores Instance_Common{..} = do
  essence <- makeAbsolute essence_path
  let info_path   = replaceFileName essence "info.json"
  let models_path = replaceFileName essence (takeBaseName essence_path ++ "_" ++ mode)
  fileErr <- catMaybes <$> sequence
    [
      fileExists   "essence" essence
    , fileExists   "info.json needs to be next to the essence" info_path
    , dirExists    "Model dir missing" models_path
    , dirExistsMay "--generated-solutions" pre_solutions
    , dirExistsMay "--given" given_dir
    ]

  let argErr = catMaybes
        [ aerr "-p|--per-model-time >0" (per_model_time == 0)
        , aerr "-i|--iterations >0" (iterations == 0)
        , aerr "-m/--mode not empty" (null mode)
        ] ++ fileErr

  let errors = argErr ++ fileErr
  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure
  out   <- giveOutputDirectory output_directory >>= makeAbsolute

  -- Look for compact in  <models_dir>-compact &  <essence_name>_df-compact
  let compactPaths = [ (models_path ++ "-compact")
                     , replaceFileName essence (takeBaseName essence_path ++ "_" ++ "df-compact")
                     ]
  cExists <- filterM doesDirectoryExist compactPaths

  compactFirst <- case cExists of
    [] -> do
      print . pretty $  "no compact found, at"  <+> vcat (map pretty compactPaths)
      return Nothing
    (path:_)  -> do
      print . pretty $ "checking for compact in: " <+> pretty path
      cs <- getAllFilesWithSuffix ".eprime" path
      catMaybes <$> mapM ((flip findCompact) models_path) cs  >>= \case
        []   -> return Nothing
        [x]  -> do
          print . pretty $ "compact_twin is" <+> pretty x
          return $ Just $ (takeBaseName x)
        (x:_) -> do
          print . pretty $ "Picking first compact_twin" <+> pretty x
          return $ Just $ (takeBaseName x)

  pre <- case pre_solutions of
           Nothing -> return Nothing
           Just fp -> do
             counts <- readSolutionCounts (fp </> "all_sols" </> "solutions.counts")
             return $ Just (fp, counts)

  i :: VarInfo <- readFromJSON info_path
  putStrLn $ "VarInfo: " ++ (groom i)
  p <- ignoreLogs $ makeProvider essence_path i
  putStrLn $ "Provider: " ++ (groom p)

  mPointsGiven <- case given_dir of
    Nothing -> return $ Nothing
    Just x  -> do
      fps <- filesWithSuffix x ".param"
      if length fps /= iterations then
          docError [ "Length mismatch"
                   , nn "iterations" iterations
                   , nn "given_dir"  (length fps)
                   ]
      else do
        ps <- mapM readPoint fps
        putStrLn . show . vcat $ [ nn "length given points" (length ps)
                                 , "first 5 points" <+> (vcat $ map pretty . take 5 $ ps)
                                 ]
        return $ Just ps

  let   common          = MCommon{
        mEssencePath    = essence
      , mOutputDir      = out
      , mModelTimeout   = per_model_time
      , mVarInfo        = i
      , mPreGenerate    = pre
      , mIterations     = iterations
      , mMode           = mode
      , mModelsDir      = models_path
      , mGivensProvider = p
      , mPoints         = []
      , mCores          = cores
      , mCompactName    = compactFirst
      , mSubCpu         = 0
      , mPointsGiven    = mPointsGiven
      , mParamGenTime   = param_gen_time
      }

  return common


filesWithSuffix :: FilePath -> String -> IO [FilePath]
filesWithSuffix dir ext= do
    fps <- getDirectoryContents dir `catchError` const (return [])
    return $ [  dir </> fp  | fp <- fps, takeExtension fp == ext  ]


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

dirExistsErr :: String -> FilePath -> IO (Maybe String)
dirExistsErr s x = do
  b <- doesDirectoryExist x
  return $ aerr (s ++ " ~ Directory Exist: " ++ x ) (b)


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

-- discard the output of instance gen if we want to discard the toolchains output
nullParamGen :: ToolchainOutput ->  IO ()
nullParamGen tc =
  if tc == ToolchainNull_ then
    setEnv "NULL_runPadded" "true"
  else
    return ()

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
             , delete_immediately = Nothing
             , list_kinds         = False
             , list_statuses      = False
             , strict_checking    = False
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
             , delete_immediately = Nothing
             , list_kinds         = False
             , list_statuses      = False
             , strict_checking    = False
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
       Reduce{per_spec_time = 30
      , from_essence = True
      , spec_directory = "/Users/bilalh/tmp/SR/2015-11-27_19-21_1448652093/"
      , error_choices = Nothing
      , always_compact=True
      , error_kind = Savilerow_, error_status = ParseError_
      , list_kinds = False,
        list_statuses = False, total_time_may = Nothing,
        total_is_cpu_time = False, output_directory = Nothing,
        _cores = Just 2, _seed = Nothing, keep_passing = False,
        delete_steps = True, delete_others = False, toolchain_ouput = ToolchainNull_,
        binaries_directory = Nothing, limit_time = Nothing, no_csv = False,
        db_directory = Nothing, db_passing_in = Nothing,
        db_only_passing = False,no_check=False, log_level=LogDebug}
  limiter (limit_time ec) (mainWithArgs ec)

_instanceDebug :: IO ()
_instanceDebug = do
  let ec = Instance_Undirected{essence_path = "/Users/bilalh/CS/essence-refinements/_current/prob028-BIBD/prob028-BIBD.essence",
                    per_model_time = 30, iterations = 1, mode = "df-every-givens-all",
                    _cores = Nothing, output_directory = Nothing, limit_time = Nothing,
                    log_level = LogDebug, _seed = Nothing, pre_solutions = Nothing, given_dir=Nothing, param_gen_time   = 300}
  limiter (limit_time ec) (mainWithArgs ec)


_smacProcessDebug :: FilePath -> UI ->  IO ()
_smacProcessDebug dir ec = do
  doesDirectoryExist dir  >>= \case
    True  -> do
      ys <- getDirectoryContents dir
      forM_ ys $ \fp -> do
        when (fp `notElem` [".", "..", "settings.csv"]) $
          doesDirectoryExist (dir </> fp) >>= \case
              True  -> removeDirectoryRecursive (dir </>  fp)
              False -> removeFile (dir </> fp)
    False -> return ()

  createDirectoryIfMissing True dir
  setCurrentDirectory dir
  limiter (limit_time ec) (mainWithArgs ec)

_smac1, _smac2 :: IO ()

_smac1 = _smacProcessDebug dir ec
  where
    dir = "/Users/bilalh/aaa/results/prob055-efpa/smac/sample-64_rndsols%1%10000/"
    ec = Script_SMAC{s_output_directory = ".", s_eprime = "empty",
                     s_instance_specific = "0", s_cutoff_time = 64.0,
                     s_cutoff_length = 2147483647, s_seed = -1,
                     s_param_arr =
                     ["-numCodeWords", "'50'", "-dist", "'50'", "-lam", "'50'",
                      "-numChars", "'50'"],  limit_time = Nothing, log_level = LogDebug}

_smac2 = _smacProcessDebug dir ec
  where
    dir = "/Users/bilalh/aaa/results/prob034-warehouse/smac/sample-64_rndsols%1%10000/"
    ec = Script_SMAC{s_output_directory = ".", s_eprime = "empty",
            s_instance_specific = "0", s_cutoff_time = 64.0,
            s_cutoff_length = 2147483647, s_seed = -1,
            s_param_arr =
              ["-cost%FT_TV%900", "'15'", "-cost%FT_T2%808", "'15'",
               "-cost%FT_T2%809", "'15'", "-cost%FT_T2%806", "'15'",
               "-cost%FT_T2%807", "'15'", "-cost%FT_T2%800", "'15'",
               "-cost%FT_T2%801", "'15'", "-cost%FT_T2%804", "'15'",
               "-cost%FT_T2%805", "'15'", "-cost%FT_T2%802", "'15'",
               "-cost%FT_T2%803", "'15'", "-cost%FT_T1%197", "'15'",
               "-cost%FT_T1%198", "'15'", "-cost%FT_T1%199", "'15'",
               "-cost%FT_T1%179", "'15'", "-cost%FT_T1%175", "'15'",
               "-cost%FT_T1%176", "'15'", "-cost%FT_T1%177", "'15'",
               "-cost%FT_T1%178", "'15'", "-cost%FT_TV%088", "'15'",
               "-cost%FT_TV%089", "'15'", "-cost%FT_TV%086", "'15'",
               "-cost%FT_TV%087", "'15'", "-cost%FT_TV%084", "'15'",
               "-cost%FT_TV%085", "'15'", "-cost%FT_TV%082", "'15'",
               "-cost%FT_TV%083", "'15'", "-cost%FT_TV%091", "'15'",
               "-cost%FT_TV%092", "'15'", "-cost%FT_TV%090", "'15'",
               "-cost%FT_T1%182", "'15'", "-cost%FT_T1%183", "'15'",
               "-cost%FT_T1%184", "'15'", "-cost%FT_T1%185", "'15'",
               "-cost%FT_T1%180", "'15'", "-cost%FT_T1%181", "'15'",
               "-cost%FT_T1%186", "'15'", "-cost%FT_T1%187", "'15'",
               "-cost%FT_T1%188", "'15'", "-cost%FT_T1%189", "'15'",
               "-cost%FT_TV%099", "'15'", "-cost%FT_TV%097", "'15'",
               "-cost%FT_TV%098", "'15'", "-cost%FT_TV%095", "'15'",
               "-cost%FT_TV%096", "'15'", "-cost%FT_TV%093", "'15'",
               "-cost%FT_TV%094", "'15'", "-cost%FT_T1%193", "'15'",
               "-cost%FT_T1%194", "'15'", "-cost%FT_T1%195", "'15'",
               "-cost%FT_T1%196", "'15'", "-cost%FT_T1%190", "'15'",
               "-cost%FT_T1%191", "'15'", "-cost%FT_T1%192", "'15'",
               "-cost%FT_TV%503", "'15'", "-cost%FT_TV%504", "'15'",
               "-cost%FT_TV%501", "'15'", "-cost%FT_TV%502", "'15'",
               "-cost%FT_TV%500", "'15'", "-cost%FT_T2%870", "'15'",
               "-cost%FT_T2%873", "'15'", "-cost%FT_T2%874", "'15'",
               "-cost%FT_T2%871", "'15'", "-cost%FT_T2%872", "'15'",
               "-cost%FT_T2%877", "'15'", "-cost%FT_T2%878", "'15'",
               "-cost%FT_T2%875", "'15'", "-cost%FT_TV%509", "'15'",
               "-cost%FT_T2%876", "'15'", "-cost%FT_TV%507", "'15'",
               "-cost%FT_TV%508", "'15'", "-cost%FT_T2%879", "'15'",
               "-cost%FT_TV%505", "'15'", "-cost%FT_TV%506", "'15'",
               "-cost%FT_TV%514", "'15'", "-cost%FT_TV%515", "'15'",
               "-cost%FT_TV%512", "'15'", "-cost%FT_T1%600", "'15'",
               "-cost%FT_TV%513", "'15'", "-cost%FT_TV%510", "'15'",
               "-cost%FT_TV%511", "'15'", "-cost%FT_T1%605", "'15'",
               "-cost%FT_T1%606", "'15'", "-cost%FT_T1%607", "'15'",
               "-cost%FT_T1%608", "'15'", "-cost%FT_T1%601", "'15'",
               "-cost%FT_T1%602", "'15'", "-cost%FT_T1%603", "'15'",
               "-cost%FT_T1%604", "'15'", "-cost%FT_T2%880", "'15'",
               "-cost%FT_T2%881", "'15'", "-cost%FT_T1%609", "'15'",
               "-cost%FT_T2%400", "'15'", "-cost%FT_T2%884", "'15'",
               "-cost%FT_T2%401", "'15'", "-cost%FT_T2%885", "'15'",
               "-cost%FT_T2%882", "'15'", "-cost%FT_T2%883", "'15'",
               "-cost%FT_T2%404", "'15'", "-cost%FT_T2%888", "'15'",
               "-cost%FT_T2%405", "'15'", "-cost%FT_T2%889", "'15'",
               "-cost%FT_T2%402", "'15'", "-cost%FT_T2%886", "'15'",
               "-cost%FT_T2%403", "'15'", "-cost%FT_T2%887", "'15'",
               "-cost%FT_T2%408", "'15'", "-cost%FT_TV%518", "'15'",
               "-cost%FT_T2%409", "'15'", "-cost%FT_TV%519", "'15'",
               "-cost%FT_T2%406", "'15'", "-cost%FT_TV%516", "'15'",
               "-cost%FT_T2%407", "'15'", "-cost%FT_TV%517", "'15'",
               "-cost%FT_T2%851", "'15'", "-cost%FT_T2%852", "'15'",
               "-cost%FT_T2%850", "'15'", "-cost%FT_T2%855", "'15'",
               "-cost%FT_T2%856", "'15'", "-cost%FT_T2%853", "'15'",
               "-cost%FT_T2%854", "'15'", "-cost%FT_T2%859", "'15'",
               "-cost%FT_T2%857", "'15'", "-cost%FT_T2%858", "'15'",
               "-cost%FT_T2%862", "'15'", "-cost%FT_T2%863", "'15'",
               "-cost%FT_T2%860", "'15'", "-cost%FT_T2%861", "'15'",
               "-cost%FT_T2%866", "'15'", "-cost%FT_T2%867", "'15'",
               "-cost%FT_T2%864", "'15'", "-cost%FT_T2%865", "'15'",
               "-cost%FT_T2%868", "'15'", "-cost%FT_T2%869", "'15'",
               "-cost%FT_T2%839", "'15'", "-cost%FT_T2%830", "'15'",
               "-cost%FT_T2%833", "'15'", "-cost%FT_T2%834", "'15'",
               "-cost%FT_T2%831", "'15'", "-cost%FT_T2%832", "'15'",
               "-cost%FT_T2%837", "'15'", "-cost%FT_T2%838", "'15'",
               "-cost%FT_T2%835", "'15'", "-cost%FT_T2%836", "'15'",
               "-cost%FT_T2%840", "'15'", "-cost%FT_T2%841", "'15'",
               "-cost%FT_T2%844", "'15'", "-cost%FT_T2%845", "'15'",
               "-cost%FT_T2%842", "'15'", "-cost%FT_T2%843", "'15'",
               "-cost%FT_T2%848", "'15'", "-cost%FT_T2%849", "'15'",
               "-cost%FT_T2%846", "'15'", "-cost%FT_T2%847", "'15'",
               "-cost%FT_T2%819", "'15'", "-cost%FT_T2%817", "'15'",
               "-cost%FT_T2%818", "'15'", "-cost%FT_T2%811", "'15'",
               "-cost%FT_T2%812", "'15'", "-cost%FT_T2%810", "'15'",
               "-cost%FT_T2%815", "'15'", "-cost%FT_T2%816", "'15'",
               "-cost%FT_T2%813", "'15'", "-cost%FT_T2%814", "'15'",
               "-cost%FT_T2%828", "'15'", "-cost%FT_T2%829", "'15'",
               "-cost%FT_T2%822", "'15'", "-cost%FT_T2%823", "'15'",
               "-cost%FT_T2%820", "'15'", "-cost%FT_T2%821", "'15'",
               "-cost%FT_T2%826", "'15'", "-cost%FT_T2%827", "'15'",
               "-cost%FT_T2%824", "'15'", "-cost%FT_T2%825", "'15'",
               "-cost%FT_T1%553", "'15'", "-cost%FT_TV%107", "'15'",
               "-cost%FT_T1%554", "'15'", "-cost%FT_TV%108", "'15'",
               "-cost%FT_T1%555", "'15'", "-cost%FT_TV%105", "'15'",
               "-cost%FT_TV%589", "'15'", "-cost%FT_T1%556", "'15'",
               "-cost%FT_TV%106", "'15'", "-cost%FT_TV%103", "'15'",
               "-cost%FT_TV%587", "'15'", "-cost%FT_T1%550", "'15'",
               "-cost%FT_TV%104", "'15'", "-cost%FT_TV%588", "'15'",
               "-cost%FT_T1%551", "'15'", "-cost%FT_TV%101", "'15'",
               "-cost%FT_TV%585", "'15'", "-cost%FT_T1%552", "'15'",
               "-cost%FT_TV%102", "'15'", "-cost%FT_TV%586", "'15'",
               "-cost%FT_TV%583", "'15'", "-cost%FT_TV%100", "'15'",
               "-cost%FT_TV%584", "'15'", "-cost%FT_TV%581", "'15'",
               "-cost%FT_TV%582", "'15'", "-cost%FT_T1%557", "'15'",
               "-cost%FT_T1%558", "'15'", "-cost%FT_T2%360", "'15'",
               "-cost%FT_TV%580", "'15'", "-cost%FT_T1%559", "'15'",
               "-cost%FT_T2%352", "'15'", "-cost%FT_T2%353", "'15'",
               "-cost%FT_T2%350", "'15'", "-cost%FT_T2%351", "'15'",
               "-cost%FT_T2%356", "'15'", "-cost%FT_T2%357", "'15'",
               "-cost%FT_T2%354", "'15'", "-cost%FT_T2%355", "'15'",
               "-cost%FT_T2%358", "'15'", "-cost%FT_T2%359", "'15'",
               "-cost%FT_TV%109", "'15'", "-cost%FT_T1%564", "'15'",
               "-cost%FT_TV%118", "'15'", "-cost%FT_T1%565", "'15'",
               "-cost%FT_TV%119", "'15'", "-cost%FT_T1%566", "'15'",
               "-cost%FT_TV%116", "'15'", "-cost%FT_T1%567", "'15'",
               "-cost%FT_TV%117", "'15'", "-cost%FT_T1%560", "'15'",
               "-cost%FT_TV%114", "'15'", "-cost%FT_TV%598", "'15'",
               "-cost%FT_T1%561", "'15'", "-cost%FT_TV%115", "'15'",
               "-cost%FT_TV%599", "'15'", "-cost%FT_T1%562", "'15'",
               "-cost%FT_TV%112", "'15'", "-cost%FT_TV%596", "'15'",
               "-cost%FT_T1%563", "'15'", "-cost%FT_TV%113", "'15'",
               "-cost%FT_TV%597", "'15'", "-cost%FT_TV%110", "'15'",
               "-cost%FT_TV%594", "'15'", "-cost%FT_TV%111", "'15'",
               "-cost%FT_TV%595", "'15'", "-cost%FT_TV%592", "'15'",
               "-cost%FT_TV%593", "'15'", "-cost%FT_T1%568", "'15'",
               "-cost%FT_T2%370", "'15'", "-cost%FT_TV%590", "'15'",
               "-cost%FT_T1%569", "'15'", "-cost%FT_T2%371", "'15'",
               "-cost%FT_TV%591", "'15'", "-cost%FT_T2%363", "'15'",
               "-cost%FT_T2%364", "'15'", "-cost%FT_T2%361", "'15'",
               "-cost%FT_T2%362", "'15'", "-cost%FT_T2%367", "'15'",
               "-cost%FT_T2%368", "'15'", "-cost%FT_T2%365", "'15'",
               "-cost%FT_T2%366", "'15'", "-cost%FT_T2%369", "'15'",
               "-cost%FT_T1%570", "'15'", "-n_upper", "'15'", "-cost%FT_T1%531",
               "'15'", "-cost%FT_TV%569", "'15'", "-cost%FT_T1%532", "'15'",
               "-cost%FT_T1%533", "'15'", "-cost%FT_TV%567", "'15'",
               "-cost%FT_T1%534", "'15'", "-cost%FT_TV%568", "'15'",
               "-cost%FT_TV%565", "'15'", "-cost%FT_TV%566", "'15'",
               "-cost%FT_TV%563", "'15'", "-cost%FT_T1%530", "'15'",
               "-cost%FT_TV%564", "'15'", "-cost%FT_T1%539", "'15'",
               "-cost%FT_TV%561", "'15'", "-cost%FT_TV%562", "'15'",
               "-cost%FT_TV%560", "'15'", "-cost%FT_T1%535", "'15'",
               "-cost%FT_T1%536", "'15'", "-cost%FT_T1%537", "'15'",
               "-cost%FT_T1%538", "'15'", "-cost%FT_T2%330", "'15'",
               "-cost%FT_T2%331", "'15'", "-cost%FT_T2%334", "'15'",
               "-cost%FT_T2%335", "'15'", "-cost%FT_T2%332", "'15'",
               "-cost%FT_T2%333", "'15'", "-cost%FT_T2%338", "'15'",
               "-cost%FT_T2%339", "'15'", "-cost%FT_T2%336", "'15'",
               "-cost%FT_T2%337", "'15'", "-cost%FT_T1%542", "'15'",
               "-cost%FT_T1%543", "'15'", "-cost%FT_T1%544", "'15'",
               "-cost%FT_TV%578", "'15'", "-cost%FT_T1%545", "'15'",
               "-cost%FT_TV%579", "'15'", "-cost%FT_TV%576", "'15'",
               "-cost%FT_TV%577", "'15'", "-cost%FT_T1%540", "'15'",
               "-cost%FT_TV%574", "'15'", "-cost%FT_T1%541", "'15'",
               "-cost%FT_TV%575", "'15'", "-cost%FT_TV%572", "'15'",
               "-cost%FT_TV%573", "'15'", "-cost%FT_TV%570", "'15'",
               "-cost%FT_TV%571", "'15'", "-cost%FT_T1%546", "'15'",
               "-cost%FT_T1%547", "'15'", "-cost%FT_T1%548", "'15'",
               "-cost%FT_T1%549", "'15'", "-cost%FT_T2%341", "'15'",
               "-cost%FT_T2%342", "'15'", "-cost%FT_T2%340", "'15'",
               "-cost%FT_T2%345", "'15'", "-cost%FT_T2%346", "'15'",
               "-cost%FT_T2%343", "'15'", "-cost%FT_T2%344", "'15'",
               "-cost%FT_T2%349", "'15'", "-cost%FT_T2%347", "'15'",
               "-cost%FT_T2%348", "'15'", "-cost%FT_TV%547", "'15'",
               "-cost%FT_T1%510", "'15'", "-cost%FT_TV%548", "'15'",
               "-cost%FT_T1%511", "'15'", "-cost%FT_TV%545", "'15'",
               "-cost%FT_T1%512", "'15'", "-cost%FT_TV%546", "'15'",
               "-cost%FT_TV%543", "'15'", "-cost%FT_TV%544", "'15'",
               "-cost%FT_TV%541", "'15'", "-cost%FT_TV%542", "'15'",
               "-cost%FT_T1%517", "'15'", "-cost%FT_T1%518", "'15'",
               "-cost%FT_TV%540", "'15'", "-cost%FT_T1%519", "'15'",
               "-cost%FT_T1%513", "'15'", "-cost%FT_T1%514", "'15'",
               "-cost%FT_T1%515", "'15'", "-cost%FT_T1%516", "'15'",
               "-cost%FT_T2%792", "'15'", "-cost%FT_T2%793", "'15'",
               "-cost%FT_T2%790", "'15'", "-cost%FT_T2%791", "'15'",
               "-cost%FT_T2%312", "'15'", "-cost%FT_T2%796", "'15'",
               "-cost%FT_T2%313", "'15'", "-cost%FT_T2%797", "'15'",
               "-cost%FT_T2%310", "'15'", "-cost%FT_T2%794", "'15'",
               "-cost%FT_T2%311", "'15'", "-cost%FT_T2%795", "'15'",
               "-cost%FT_T2%316", "'15'", "-cost%FT_T2%317", "'15'",
               "-cost%FT_T2%314", "'15'", "-cost%FT_T2%798", "'15'",
               "-cost%FT_T2%315", "'15'", "-cost%FT_T2%799", "'15'",
               "-cost%FT_T2%318", "'15'", "-cost%FT_TV%549", "'15'",
               "-cost%FT_T2%319", "'15'", "-cost%FT_T1%520", "'15'",
               "-cost%FT_TV%558", "'15'", "-cost%FT_T1%521", "'15'",
               "-cost%FT_TV%559", "'15'", "-cost%FT_T1%522", "'15'",
               "-cost%FT_TV%556", "'15'", "-cost%FT_T1%523", "'15'",
               "-cost%FT_TV%557", "'15'", "-cost%FT_TV%554", "'15'",
               "-cost%FT_TV%555", "'15'", "-cost%FT_TV%552", "'15'",
               "-cost%FT_TV%553", "'15'", "-cost%FT_T1%528", "'15'",
               "-cost%FT_TV%550", "'15'", "-cost%FT_T1%529", "'15'",
               "-cost%FT_TV%551", "'15'", "-cost%FT_T1%524", "'15'",
               "-cost%FT_T1%525", "'15'", "-cost%FT_T1%526", "'15'",
               "-cost%FT_T1%527", "'15'", "-cost%FT_T2%320", "'15'",
               "-cost%FT_T2%323", "'15'", "-cost%FT_T2%324", "'15'",
               "-cost%FT_T2%321", "'15'", "-cost%FT_T2%322", "'15'",
               "-cost%FT_T2%327", "'15'", "-cost%FT_T2%328", "'15'",
               "-cost%FT_T2%325", "'15'", "-cost%FT_T2%326", "'15'",
               "-cost%FT_T2%329", "'15'", "-cost%FT_TV%525", "'15'",
               "-cost%FT_TV%526", "'15'", "-cost%FT_TV%523", "'15'",
               "-cost%FT_TV%524", "'15'", "-cost%FT_TV%521", "'15'",
               "-cost%FT_TV%522", "'15'", "-cost%FT_TV%520", "'15'",
               "-cost%FT_T2%770", "'15'", "-cost%FT_T2%771", "'15'",
               "-cost%FT_T2%774", "'15'", "-cost%FT_T2%775", "'15'",
               "-cost%FT_T2%772", "'15'", "-cost%FT_T2%773", "'15'",
               "-cost%FT_T2%778", "'15'", "-cost%FT_T2%779", "'15'",
               "-cost%FT_T2%776", "'15'", "-cost%FT_T2%777", "'15'",
               "-cost%FT_TV%529", "'15'", "-cost%FT_TV%527", "'15'",
               "-cost%FT_TV%528", "'15'", "-cost%FT_TV%536", "'15'",
               "-cost%FT_TV%537", "'15'", "-cost%FT_T1%500", "'15'",
               "-cost%FT_TV%534", "'15'", "-cost%FT_T1%501", "'15'",
               "-cost%FT_TV%535", "'15'", "-cost%FT_TV%532", "'15'",
               "-cost%FT_TV%533", "'15'", "-cost%FT_TV%530", "'15'",
               "-cost%FT_TV%531", "'15'", "-cost%FT_T1%506", "'15'",
               "-cost%FT_T1%507", "'15'", "-cost%FT_T1%508", "'15'",
               "-cost%FT_T1%509", "'15'", "-cost%FT_T1%502", "'15'",
               "-cost%FT_T1%503", "'15'", "-cost%FT_T1%504", "'15'",
               "-cost%FT_T1%505", "'15'", "-cost%FT_T2%781", "'15'",
               "-cost%FT_T2%782", "'15'", "-cost%FT_T2%780", "'15'",
               "-cost%FT_T2%301", "'15'", "-cost%FT_T2%785", "'15'",
               "-cost%FT_T2%302", "'15'", "-cost%FT_T2%786", "'15'",
               "-cost%FT_T2%783", "'15'", "-cost%FT_T2%300", "'15'",
               "-cost%FT_T2%784", "'15'", "-cost%FT_T2%305", "'15'",
               "-cost%FT_T2%789", "'15'", "-cost%FT_T2%306", "'15'",
               "-cost%FT_T2%303", "'15'", "-cost%FT_T2%787", "'15'",
               "-cost%FT_T2%304", "'15'", "-cost%FT_T2%788", "'15'",
               "-cost%FT_T2%309", "'15'", "-cost%FT_T2%307", "'15'",
               "-cost%FT_TV%538", "'15'", "-cost%FT_T2%308", "'15'",
               "-cost%FT_TV%539", "'15'", "-cost%FT_T1%157", "'15'",
               "-cost%FT_T1%158", "'15'", "-cost%FT_T1%159", "'15'",
               "-cost%FT_T1%153", "'15'", "-cost%FT_T1%154", "'15'",
               "-cost%FT_T1%155", "'15'", "-cost%FT_TV%189", "'15'",
               "-cost%FT_T1%156", "'15'", "-cost%FT_TV%187", "'15'",
               "-cost%FT_TV%188", "'15'", "-cost%FT_TV%185", "'15'",
               "-cost%FT_TV%186", "'15'", "-cost%FT_TV%183", "'15'",
               "-cost%FT_TV%184", "'15'", "-cost%FT_TV%181", "'15'",
               "-cost%FT_TV%182", "'15'", "-cost%FT_TV%190", "'15'",
               "-cost%FT_TV%191", "'15'", "-cost%FT_T1%160", "'15'",
               "-cost%FT_T1%161", "'15'", "-cost%FT_T1%162", "'15'",
               "-cost%FT_T1%163", "'15'", "-cost%FT_T1%168", "'15'",
               "-cost%FT_T1%169", "'15'", "-cost%FT_T1%164", "'15'",
               "-cost%FT_T1%165", "'15'", "-cost%FT_T1%166", "'15'",
               "-cost%FT_T1%167", "'15'", "-cost%FT_TV%198", "'15'",
               "-cost%FT_TV%199", "'15'", "-cost%FT_TV%196", "'15'",
               "-cost%FT_TV%197", "'15'", "-cost%FT_TV%194", "'15'",
               "-cost%FT_TV%195", "'15'", "-cost%FT_TV%192", "'15'",
               "-cost%FT_TV%193", "'15'", "-cost%FT_T1%171", "'15'",
               "-cost%FT_T1%172", "'15'", "-cost%FT_T1%173", "'15'",
               "-cost%FT_T1%174", "'15'", "-cost%FT_T1%170", "'15'",
               "-cost%FT_T1%135", "'15'", "-cost%FT_T1%136", "'15'",
               "-cost%FT_T1%137", "'15'", "-cost%FT_T1%138", "'15'",
               "-cost%FT_T1%131", "'15'", "-cost%FT_TV%169", "'15'",
               "-cost%FT_T1%132", "'15'", "-cost%FT_T1%133", "'15'",
               "-cost%FT_TV%167", "'15'", "-cost%FT_T1%134", "'15'",
               "-cost%FT_TV%168", "'15'", "-cost%FT_TV%165", "'15'",
               "-cost%FT_TV%166", "'15'", "-cost%FT_TV%163", "'15'",
               "-cost%FT_TV%164", "'15'", "-cost%FT_T1%139", "'15'",
               "-cost%FT_TV%161", "'15'", "-cost%FT_TV%162", "'15'",
               "-cost%FT_TV%160", "'15'", "-cost%FT_T1%140", "'15'",
               "-cost%FT_T1%141", "'15'", "-cost%FT_T1%146", "'15'",
               "-cost%FT_T1%147", "'15'", "-cost%FT_T1%148", "'15'",
               "-cost%FT_T1%149", "'15'", "-cost%FT_T1%142", "'15'",
               "-cost%FT_T1%143", "'15'", "-cost%FT_T1%144", "'15'",
               "-cost%FT_TV%178", "'15'", "-cost%FT_T1%145", "'15'",
               "-cost%FT_TV%179", "'15'", "-cost%FT_TV%176", "'15'",
               "-cost%FT_TV%177", "'15'", "-cost%FT_TV%174", "'15'",
               "-cost%FT_TV%175", "'15'", "-cost%FT_TV%172", "'15'",
               "-cost%FT_TV%173", "'15'", "-cost%FT_TV%170", "'15'",
               "-cost%FT_TV%171", "'15'", "-cost%FT_TV%180", "'15'",
               "-cost%FT_T1%150", "'15'", "-cost%FT_T1%151", "'15'",
               "-cost%FT_T1%152", "'15'", "-cost%FT_T1%113", "'15'",
               "-cost%FT_T1%597", "'15'", "-cost%FT_T1%114", "'15'",
               "-cost%FT_T1%598", "'15'", "-cost%FT_T1%115", "'15'",
               "-cost%FT_T1%599", "'15'", "-cost%FT_TV%149", "'15'",
               "-cost%FT_T1%116", "'15'", "-cost%FT_T1%593", "'15'",
               "-cost%FT_TV%147", "'15'", "-cost%FT_T1%110", "'15'",
               "-cost%FT_T1%594", "'15'", "-cost%FT_TV%148", "'15'",
               "-cost%FT_T1%111", "'15'", "-cost%FT_T1%595", "'15'",
               "-cost%FT_TV%145", "'15'", "-cost%FT_T1%112", "'15'",
               "-cost%FT_T1%596", "'15'", "-cost%FT_TV%146", "'15'",
               "-cost%FT_TV%143", "'15'", "-cost%FT_TV%144", "'15'",
               "-cost%FT_TV%141", "'15'", "-cost%FT_TV%142", "'15'",
               "-cost%FT_T1%117", "'15'", "-cost%FT_T1%118", "'15'",
               "-cost%FT_TV%140", "'15'", "-cost%FT_T1%119", "'15'",
               "-cost%FT_T2%396", "'15'", "-cost%FT_T2%397", "'15'",
               "-cost%FT_T2%394", "'15'", "-cost%FT_T2%395", "'15'",
               "-cost%FT_T2%398", "'15'", "-cost%FT_T2%399", "'15'",
               "-cost%FT_T1%124", "'15'", "-cost%FT_T1%125", "'15'",
               "-cost%FT_T1%126", "'15'", "-cost%FT_T1%127", "'15'",
               "-cost%FT_T1%120", "'15'", "-cost%FT_TV%158", "'15'",
               "-cost%FT_T1%121", "'15'", "-cost%FT_TV%159", "'15'",
               "-cost%FT_T1%122", "'15'", "-cost%FT_TV%156", "'15'",
               "-cost%FT_T1%123", "'15'", "-cost%FT_TV%157", "'15'",
               "-cost%FT_TV%154", "'15'", "-cost%FT_TV%155", "'15'",
               "-cost%FT_TV%152", "'15'", "-cost%FT_TV%153", "'15'",
               "-cost%FT_T1%128", "'15'", "-cost%FT_TV%150", "'15'",
               "-cost%FT_T1%129", "'15'", "-cost%FT_TV%151", "'15'",
               "-cost%FT_T1%130", "'15'", "-cost%FT_T1%575", "'15'",
               "-cost%FT_TV%129", "'15'", "-cost%FT_T1%576", "'15'",
               "-cost%FT_T1%577", "'15'", "-cost%FT_TV%127", "'15'",
               "-cost%FT_T1%578", "'15'", "-cost%FT_TV%128", "'15'",
               "-cost%FT_T1%571", "'15'", "-cost%FT_TV%125", "'15'",
               "-cost%FT_T1%572", "'15'", "-cost%FT_TV%126", "'15'",
               "-cost%FT_T1%573", "'15'", "-cost%FT_TV%123", "'15'",
               "-cost%FT_T1%574", "'15'", "-cost%FT_TV%124", "'15'",
               "-cost%FT_TV%121", "'15'", "-cost%FT_TV%122", "'15'",
               "-cost%FT_TV%120", "'15'", "-cost%FT_T1%579", "'15'",
               "-cost%FT_T2%381", "'15'", "-cost%FT_T2%382", "'15'",
               "-cost%FT_T2%380", "'15'", "-cost%FT_T2%374", "'15'",
               "-cost%FT_T2%375", "'15'", "-cost%FT_T2%372", "'15'",
               "-cost%FT_T2%373", "'15'", "-cost%FT_T2%378", "'15'",
               "-cost%FT_T2%379", "'15'", "-cost%FT_T2%376", "'15'",
               "-cost%FT_T2%377", "'15'", "-cost%FT_T1%580", "'15'",
               "-cost%FT_T1%581", "'15'", "-cost%FT_T1%102", "'15'",
               "-cost%FT_T1%586", "'15'", "-cost%FT_T1%103", "'15'",
               "-cost%FT_T1%587", "'15'", "-cost%FT_T1%104", "'15'",
               "-cost%FT_T1%588", "'15'", "-cost%FT_TV%138", "'15'",
               "-cost%FT_T1%105", "'15'", "-cost%FT_T1%589", "'15'",
               "-cost%FT_TV%139", "'15'", "-cost%FT_T1%582", "'15'",
               "-cost%FT_TV%136", "'15'", "-cost%FT_T1%583", "'15'",
               "-cost%FT_TV%137", "'15'", "-cost%FT_T1%100", "'15'",
               "-cost%FT_T1%584", "'15'", "-cost%FT_TV%134", "'15'",
               "-cost%FT_T1%101", "'15'", "-cost%FT_T1%585", "'15'",
               "-cost%FT_TV%135", "'15'", "-cost%FT_TV%132", "'15'",
               "-cost%FT_TV%133", "'15'", "-cost%FT_TV%130", "'15'",
               "-cost%FT_TV%131", "'15'", "-cost%FT_T1%106", "'15'",
               "-cost%FT_T2%392", "'15'", "-cost%FT_T1%107", "'15'",
               "-cost%FT_T2%393", "'15'", "-cost%FT_T1%108", "'15'",
               "-cost%FT_T2%390", "'15'", "-cost%FT_T1%109", "'15'",
               "-cost%FT_T2%391", "'15'", "-cost%FT_T2%385", "'15'",
               "-cost%FT_T2%386", "'15'", "-cost%FT_T2%383", "'15'",
               "-cost%FT_T2%384", "'15'", "-cost%FT_T2%389", "'15'",
               "-cost%FT_T2%387", "'15'", "-cost%FT_T2%388", "'15'",
               "-cost%FT_T1%590", "'15'", "-cost%FT_T1%591", "'15'",
               "-cost%FT_T1%592", "'15'", "-cost%FT_T2%900", "'15'",
               "-cost%FT_T1%296", "'15'", "-cost%FT_T1%297", "'15'",
               "-cost%FT_T1%298", "'15'", "-cost%FT_T1%299", "'15'",
               "-cost%FT_T2%099", "'15'", "-cost%FT_T2%097", "'15'",
               "-cost%FT_T2%098", "'15'", "-cost%FT_TV%866", "'15'",
               "-cost%FT_TV%867", "'15'", "-cost%FT_TV%864", "'15'",
               "-cost%FT_T1%710", "'15'", "-cost%FT_TV%865", "'15'",
               "-cost%FT_TV%862", "'15'", "-cost%FT_TV%863", "'15'",
               "-cost%FT_TV%860", "'15'", "-cost%FT_TV%861", "'15'",
               "-cost%FT_T1%715", "'15'", "-cost%FT_T1%716", "'15'",
               "-cost%FT_T1%717", "'15'", "-cost%FT_T1%718", "'15'",
               "-cost%FT_T1%711", "'15'", "-cost%FT_T1%712", "'15'",
               "-cost%FT_T1%713", "'15'", "-cost%FT_T1%714", "'15'",
               "-cost%FT_T1%719", "'15'", "-cost%FT_T2%510", "'15'",
               "-cost%FT_T2%511", "'15'", "-cost%FT_T2%514", "'15'",
               "-cost%FT_T2%515", "'15'", "-cost%FT_T2%512", "'15'",
               "-cost%FT_T2%513", "'15'", "-cost%FT_T2%518", "'15'",
               "-cost%FT_T2%519", "'15'", "-cost%FT_T2%516", "'15'",
               "-cost%FT_TV%868", "'15'", "-cost%FT_T2%517", "'15'",
               "-cost%FT_TV%869", "'15'", "-cost%FT_TV%877", "'15'",
               "-cost%FT_TV%878", "'15'", "-cost%FT_T1%720", "'15'",
               "-cost%FT_TV%875", "'15'", "-cost%FT_T1%721", "'15'",
               "-cost%FT_TV%876", "'15'", "-cost%FT_TV%873", "'15'",
               "-cost%FT_TV%874", "'15'", "-cost%FT_TV%871", "'15'",
               "-cost%FT_TV%872", "'15'", "-cost%FT_T1%726", "'15'",
               "-cost%FT_T1%727", "'15'", "-cost%FT_TV%870", "'15'",
               "-cost%FT_T1%728", "'15'", "-cost%FT_T1%729", "'15'",
               "-cost%FT_T1%722", "'15'", "-cost%FT_T1%723", "'15'",
               "-cost%FT_T1%724", "'15'", "-cost%FT_T1%725", "'15'",
               "-cost%FT_T2%521", "'15'", "-cost%FT_T2%522", "'15'",
               "-cost%FT_T2%520", "'15'", "-cost%FT_T2%525", "'15'",
               "-cost%FT_T2%526", "'15'", "-cost%FT_T2%523", "'15'",
               "-cost%FT_T2%524", "'15'", "-cost%FT_T2%529", "'15'",
               "-cost%FT_T2%527", "'15'", "-cost%FT_TV%879", "'15'",
               "-cost%FT_T2%528", "'15'", "-cost%FT_TV%844", "'15'",
               "-cost%FT_TV%845", "'15'", "-cost%FT_TV%842", "'15'",
               "-cost%FT_TV%843", "'15'", "-cost%FT_TV%840", "'15'",
               "-cost%FT_TV%841", "'15'", "-cost%FT_TV%848", "'15'",
               "-cost%FT_TV%849", "'15'", "-cost%FT_TV%846", "'15'",
               "-cost%FT_TV%847", "'15'", "-cost%FT_TV%855", "'15'",
               "-cost%FT_TV%856", "'15'", "-cost%FT_T2%509", "'15'",
               "-cost%FT_TV%853", "'15'", "-cost%FT_TV%854", "'15'",
               "-cost%FT_TV%851", "'15'", "-cost%FT_TV%852", "'15'",
               "-cost%FT_TV%850", "'15'", "-cost%FT_T1%704", "'15'",
               "-cost%FT_T1%705", "'15'", "-cost%FT_T1%706", "'15'",
               "-cost%FT_T1%707", "'15'", "-cost%FT_T1%700", "'15'",
               "-cost%FT_T1%701", "'15'", "-cost%FT_T1%702", "'15'",
               "-cost%FT_T1%703", "'15'", "-cost%FT_T1%708", "'15'",
               "-cost%FT_T1%709", "'15'", "-cost%FT_T2%500", "'15'",
               "-cost%FT_T2%503", "'15'", "-cost%FT_T2%504", "'15'",
               "-cost%FT_T2%501", "'15'", "-cost%FT_T2%502", "'15'",
               "-cost%FT_T2%507", "'15'", "-cost%FT_TV%859", "'15'",
               "-cost%FT_T2%508", "'15'", "-cost%FT_T2%505", "'15'",
               "-cost%FT_TV%857", "'15'", "-cost%FT_T2%506", "'15'",
               "-cost%FT_TV%858", "'15'", "-cost%FT_TV%822", "'15'",
               "-cost%FT_TV%823", "'15'", "-cost%FT_TV%820", "'15'",
               "-cost%FT_TV%821", "'15'", "-cost%FT_TV%828", "'15'",
               "-cost%FT_TV%829", "'15'", "-cost%FT_TV%826", "'15'",
               "-cost%FT_TV%827", "'15'", "-cost%FT_TV%824", "'15'",
               "-cost%FT_TV%825", "'15'", "-cost%FT_TV%833", "'15'",
               "-cost%FT_TV%834", "'15'", "-cost%FT_TV%831", "'15'",
               "-cost%FT_TV%832", "'15'", "-cost%FT_TV%830", "'15'",
               "-cost%FT_TV%839", "'15'", "-cost%FT_TV%837", "'15'",
               "-cost%FT_TV%838", "'15'", "-cost%FT_TV%835", "'15'",
               "-cost%FT_TV%836", "'15'", "-cost%FT_TV%800", "'15'",
               "-cost%FT_TV%801", "'15'", "-cost%FT_TV%808", "'15'",
               "-cost%FT_TV%809", "'15'", "-cost%FT_TV%806", "'15'",
               "-cost%FT_TV%807", "'15'", "-cost%FT_TV%804", "'15'",
               "-cost%FT_TV%805", "'15'", "-cost%FT_TV%802", "'15'",
               "-cost%FT_TV%803", "'15'", "-cost%FT_TV%811", "'15'",
               "-cost%FT_TV%812", "'15'", "-cost%FT_TV%810", "'15'",
               "-cost%FT_TV%819", "'15'", "-cost%FT_TV%817", "'15'",
               "-cost%FT_TV%818", "'15'", "-cost%FT_TV%815", "'15'",
               "-cost%FT_TV%816", "'15'", "-cost%FT_TV%813", "'15'",
               "-cost%FT_TV%814", "'15'", "-cost%FT_T1%674", "'15'",
               "-cost%FT_T1%675", "'15'", "-cost%FT_T1%676", "'15'",
               "-cost%FT_TV%468", "'15'", "-cost%FT_T1%677", "'15'",
               "-cost%FT_TV%469", "'15'", "-cost%FT_T1%670", "'15'",
               "-cost%FT_TV%466", "'15'", "-cost%FT_T1%671", "'15'",
               "-cost%FT_TV%467", "'15'", "-cost%FT_T1%672", "'15'",
               "-cost%FT_TV%464", "'15'", "-cost%FT_T1%673", "'15'",
               "-cost%FT_TV%465", "'15'", "-cost%FT_TV%462", "'15'",
               "-cost%FT_TV%463", "'15'", "-cost%FT_TV%460", "'15'",
               "-cost%FT_TV%461", "'15'", "-cost%FT_T1%678", "'15'",
               "-cost%FT_T2%480", "'15'", "-cost%FT_T1%679", "'15'",
               "-cost%FT_T2%481", "'15'", "-cost%FT_T2%473", "'15'",
               "-cost%FT_T2%474", "'15'", "-cost%FT_T2%471", "'15'",
               "-cost%FT_T2%472", "'15'", "-cost%FT_T2%477", "'15'",
               "-cost%FT_T2%478", "'15'", "-cost%FT_T2%475", "'15'",
               "-cost%FT_T2%476", "'15'", "-cost%FT_T2%479", "'15'",
               "-cost%FT_T1%680", "'15'", "-cost%FT_T1%201", "'15'",
               "-cost%FT_T1%685", "'15'", "-cost%FT_T1%202", "'15'",
               "-cost%FT_T1%686", "'15'", "-cost%FT_T1%203", "'15'",
               "-cost%FT_T1%687", "'15'", "-cost%FT_TV%479", "'15'",
               "-cost%FT_T1%204", "'15'", "-cost%FT_T1%688", "'15'",
               "-cost%FT_T1%681", "'15'", "-cost%FT_TV%477", "'15'",
               "-cost%FT_T1%682", "'15'", "-cost%FT_TV%478", "'15'",
               "-cost%FT_T1%683", "'15'", "-cost%FT_TV%475", "'15'",
               "-cost%FT_T1%200", "'15'", "-cost%FT_T1%684", "'15'",
               "-cost%FT_TV%476", "'15'", "-cost%FT_T1%209", "'15'",
               "-cost%FT_TV%473", "'15'", "-cost%FT_TV%474", "'15'",
               "-cost%FT_TV%471", "'15'", "-cost%FT_TV%472", "'15'",
               "-cost%FT_T1%205", "'15'", "-cost%FT_T1%689", "'15'",
               "-cost%FT_T2%491", "'15'", "-cost%FT_T1%206", "'15'",
               "-cost%FT_T2%492", "'15'", "-cost%FT_TV%470", "'15'",
               "-cost%FT_T1%207", "'15'", "-cost%FT_T1%208", "'15'",
               "-cost%FT_T2%490", "'15'", "-cost%FT_T2%484", "'15'",
               "-cost%FT_T2%001", "'15'", "-cost%FT_T2%485", "'15'",
               "-cost%FT_T2%482", "'15'", "-cost%FT_T2%483", "'15'",
               "-cost%FT_T2%004", "'15'", "-cost%FT_T2%488", "'15'",
               "-cost%FT_T2%005", "'15'", "-cost%FT_T2%489", "'15'",
               "-cost%FT_T2%002", "'15'", "-cost%FT_T2%486", "'15'",
               "-cost%FT_T2%003", "'15'", "-cost%FT_T2%487", "'15'",
               "-cost%FT_T2%008", "'15'", "-cost%FT_T2%009", "'15'",
               "-cost%FT_T1%690", "'15'", "-cost%FT_T2%006", "'15'",
               "-cost%FT_T1%691", "'15'", "-cost%FT_T2%007", "'15'",
               "-cost%FT_T1%652", "'15'", "-cost%FT_TV%448", "'15'",
               "-cost%FT_T1%653", "'15'", "-cost%FT_TV%449", "'15'",
               "-cost%FT_T1%654", "'15'", "-cost%FT_TV%446", "'15'",
               "-cost%FT_T1%655", "'15'", "-cost%FT_TV%447", "'15'",
               "-cost%FT_TV%444", "'15'", "-cost%FT_TV%445", "'15'",
               "-cost%FT_T1%650", "'15'", "-cost%FT_TV%442", "'15'",
               "-cost%FT_T1%651", "'15'", "-cost%FT_TV%443", "'15'",
               "-cost%FT_TV%440", "'15'", "-cost%FT_TV%441", "'15'",
               "-cost%FT_T1%656", "'15'", "-cost%FT_T1%657", "'15'",
               "-cost%FT_T1%658", "'15'", "-cost%FT_T1%659", "'15'",
               "-cost%FT_T2%451", "'15'", "-cost%FT_T2%452", "'15'",
               "-cost%FT_T2%450", "'15'", "-cost%FT_T2%455", "'15'",
               "-cost%FT_T2%456", "'15'", "-cost%FT_T2%453", "'15'",
               "-cost%FT_T2%454", "'15'", "-cost%FT_T2%459", "'15'",
               "-cost%FT_T2%457", "'15'", "-cost%FT_T2%458", "'15'",
               "-cost%FT_T1%663", "'15'", "-cost%FT_TV%459", "'15'",
               "-cost%FT_T1%664", "'15'", "-cost%FT_T1%665", "'15'",
               "-cost%FT_TV%457", "'15'", "-cost%FT_T1%666", "'15'",
               "-cost%FT_TV%458", "'15'", "-cost%FT_TV%455", "'15'",
               "-cost%FT_T1%660", "'15'", "-cost%FT_TV%456", "'15'",
               "-cost%FT_T1%661", "'15'", "-cost%FT_TV%453", "'15'",
               "-cost%FT_T1%662", "'15'", "-cost%FT_TV%454", "'15'",
               "-cost%FT_TV%451", "'15'", "-cost%FT_TV%452", "'15'",
               "-cost%FT_TV%450", "'15'", "-cost%FT_T1%667", "'15'",
               "-cost%FT_T1%668", "'15'", "-cost%FT_T2%470", "'15'",
               "-cost%FT_T1%669", "'15'", "-cost%FT_T2%462", "'15'",
               "-cost%FT_T2%463", "'15'", "-cost%FT_T2%460", "'15'",
               "-cost%FT_T2%461", "'15'", "-cost%FT_T2%466", "'15'",
               "-cost%FT_T2%467", "'15'", "-cost%FT_T2%464", "'15'",
               "-cost%FT_T2%465", "'15'", "-cost%FT_T2%468", "'15'",
               "-cost%FT_T2%469", "'15'", "-cost%FT_T1%630", "'15'",
               "-cost%FT_TV%426", "'15'", "-cost%FT_T1%631", "'15'",
               "-cost%FT_TV%427", "'15'", "-cost%FT_T1%632", "'15'",
               "-cost%FT_TV%424", "'15'", "-cost%FT_T1%633", "'15'",
               "-cost%FT_TV%425", "'15'", "-cost%FT_TV%422", "'15'",
               "-cost%FT_TV%423", "'15'", "-cost%FT_TV%420", "'15'",
               "-cost%FT_TV%421", "'15'", "-cost%FT_T1%638", "'15'",
               "-cost%FT_T1%639", "'15'", "-cost%FT_T1%634", "'15'",
               "-cost%FT_T1%635", "'15'", "-cost%FT_T1%636", "'15'",
               "-cost%FT_T1%637", "'15'", "-cost%FT_T2%430", "'15'",
               "-cost%FT_T2%433", "'15'", "-cost%FT_T2%434", "'15'",
               "-cost%FT_T2%431", "'15'", "-cost%FT_T2%432", "'15'",
               "-cost%FT_T2%437", "'15'", "-cost%FT_T2%438", "'15'",
               "-cost%FT_T2%435", "'15'", "-cost%FT_T2%436", "'15'",
               "-cost%FT_T2%439", "'15'", "-cost%FT_TV%428", "'15'",
               "-cost%FT_TV%429", "'15'", "-cost%FT_T1%641", "'15'",
               "-cost%FT_TV%437", "'15'", "-cost%FT_T1%642", "'15'",
               "-cost%FT_TV%438", "'15'", "-cost%FT_T1%643", "'15'",
               "-cost%FT_TV%435", "'15'", "-cost%FT_T1%644", "'15'",
               "-cost%FT_TV%436", "'15'", "-cost%FT_TV%433", "'15'",
               "-cost%FT_TV%434", "'15'", "-cost%FT_TV%431", "'15'",
               "-cost%FT_T1%640", "'15'", "-cost%FT_TV%432", "'15'",
               "-cost%FT_T1%649", "'15'", "-cost%FT_TV%430", "'15'",
               "-n_warehouses", "'15'", "-cost%FT_T1%645", "'15'",
               "-cost%FT_T1%646", "'15'", "-cost%FT_T1%647", "'15'",
               "-cost%FT_T1%648", "'15'", "-cost%FT_T2%440", "'15'",
               "-cost%FT_T2%441", "'15'", "-cost%FT_T2%444", "'15'",
               "-cost%FT_T2%445", "'15'", "-cost%FT_T2%442", "'15'",
               "-cost%FT_T2%443", "'15'", "-cost%FT_T2%448", "'15'",
               "-cost%FT_T2%449", "'15'", "-cost%FT_T2%446", "'15'",
               "-cost%FT_T2%447", "'15'", "-cost%FT_TV%439", "'15'",
               "-cost%FT_TV%404", "'15'", "-cost%FT_TV%888", "'15'",
               "-cost%FT_TV%405", "'15'", "-cost%FT_TV%889", "'15'",
               "-cost%FT_T1%610", "'15'", "-cost%FT_TV%402", "'15'",
               "-cost%FT_TV%886", "'15'", "-cost%FT_T1%611", "'15'",
               "-cost%FT_TV%403", "'15'", "-cost%FT_TV%887", "'15'",
               "-cost%FT_TV%400", "'15'", "-cost%FT_TV%884", "'15'",
               "-cost%FT_TV%401", "'15'", "-cost%FT_TV%885", "'15'",
               "-cost%FT_TV%882", "'15'", "-cost%FT_TV%883", "'15'",
               "-cost%FT_T1%616", "'15'", "-cost%FT_TV%880", "'15'",
               "-cost%FT_T1%617", "'15'", "-cost%FT_TV%881", "'15'",
               "-cost%FT_T1%618", "'15'", "-cost%FT_T1%619", "'15'",
               "-cost%FT_T1%612", "'15'", "-cost%FT_T1%613", "'15'",
               "-cost%FT_T1%614", "'15'", "-cost%FT_T1%615", "'15'",
               "-cost%FT_T2%891", "'15'", "-cost%FT_T2%892", "'15'",
               "-cost%FT_T2%890", "'15'", "-cost%FT_T2%411", "'15'",
               "-cost%FT_T2%895", "'15'", "-cost%FT_T2%412", "'15'",
               "-cost%FT_T2%896", "'15'", "-cost%FT_T2%893", "'15'",
               "-cost%FT_T2%410", "'15'", "-cost%FT_T2%894", "'15'",
               "-cost%FT_T2%415", "'15'", "-cost%FT_T2%899", "'15'",
               "-cost%FT_T2%416", "'15'", "-cost%FT_T2%413", "'15'",
               "-cost%FT_T2%897", "'15'", "-cost%FT_T2%414", "'15'",
               "-cost%FT_T2%898", "'15'", "-cost%FT_T2%419", "'15'",
               "-cost%FT_TV%408", "'15'", "-cost%FT_TV%409", "'15'",
               "-cost%FT_T2%417", "'15'", "-cost%FT_TV%406", "'15'",
               "-cost%FT_T2%418", "'15'", "-cost%FT_TV%407", "'15'",
               "-cost%FT_TV%415", "'15'", "-cost%FT_TV%899", "'15'",
               "-cost%FT_T1%620", "'15'", "-cost%FT_TV%416", "'15'",
               "-cost%FT_T1%621", "'15'", "-cost%FT_TV%413", "'15'",
               "-cost%FT_TV%897", "'15'", "-cost%FT_T1%622", "'15'",
               "-cost%FT_TV%414", "'15'", "-cost%FT_TV%898", "'15'",
               "-cost%FT_TV%411", "'15'", "-cost%FT_TV%895", "'15'",
               "-cost%FT_TV%412", "'15'", "-cost%FT_TV%896", "'15'",
               "-cost%FT_TV%893", "'15'", "-cost%FT_TV%410", "'15'",
               "-cost%FT_TV%894", "'15'", "-cost%FT_T1%627", "'15'",
               "-cost%FT_TV%891", "'15'", "-cost%FT_T1%628", "'15'",
               "-cost%FT_TV%892", "'15'", "-cost%FT_T1%629", "'15'",
               "-cost%FT_TV%890", "'15'", "-cost%FT_T1%623", "'15'",
               "-cost%FT_T1%624", "'15'", "-cost%FT_T1%625", "'15'",
               "-cost%FT_T1%626", "'15'", "-cost%FT_T2%422", "'15'",
               "-cost%FT_T2%423", "'15'", "-cost%FT_T2%420", "'15'",
               "-cost%FT_T2%421", "'15'", "-cost%FT_T2%426", "'15'",
               "-cost%FT_T2%427", "'15'", "-cost%FT_T2%424", "'15'",
               "-cost%FT_T2%425", "'15'", "-cost%FT_TV%419", "'15'",
               "-cost%FT_T2%428", "'15'", "-cost%FT_TV%417", "'15'",
               "-cost%FT_T2%429", "'15'", "-cost%FT_TV%418", "'15'",
               "-cost%FT_T1%278", "'15'", "-cost%FT_T1%279", "'15'",
               "-cost%FT_T1%274", "'15'", "-cost%FT_T1%275", "'15'",
               "-cost%FT_T1%276", "'15'", "-cost%FT_TV%068", "'15'",
               "-cost%FT_T1%277", "'15'", "-cost%FT_TV%069", "'15'",
               "-cost%FT_T2%080", "'15'", "-cost%FT_TV%066", "'15'",
               "-cost%FT_T2%081", "'15'", "-cost%FT_TV%067", "'15'",
               "-cost%FT_TV%064", "'15'", "-cost%FT_TV%065", "'15'",
               "-cost%FT_T2%084", "'15'", "-cost%FT_TV%062", "'15'",
               "-cost%FT_T2%085", "'15'", "-cost%FT_TV%063", "'15'",
               "-cost%FT_T2%082", "'15'", "-cost%FT_TV%060", "'15'",
               "-cost%FT_T2%083", "'15'", "-cost%FT_TV%061", "'15'",
               "-cost%FT_T2%077", "'15'", "-cost%FT_T2%078", "'15'",
               "-cost%FT_TV%070", "'15'", "-cost%FT_T2%075", "'15'",
               "-cost%FT_T2%076", "'15'", "-cost%FT_T2%079", "'15'",
               "-cost%FT_T1%281", "'15'", "-cost%FT_T1%282", "'15'",
               "-cost%FT_T1%283", "'15'", "-cost%FT_T1%284", "'15'",
               "-cost%FT_T1%280", "'15'", "-cost%FT_T1%289", "'15'",
               "-cost%FT_T1%285", "'15'", "-cost%FT_T1%286", "'15'",
               "-cost%FT_T1%287", "'15'", "-cost%FT_TV%079", "'15'",
               "-cost%FT_T1%288", "'15'", "-cost%FT_T2%091", "'15'",
               "-cost%FT_TV%077", "'15'", "-cost%FT_T2%092", "'15'",
               "-cost%FT_TV%078", "'15'", "-cost%FT_TV%075", "'15'",
               "-cost%FT_T2%090", "'15'", "-cost%FT_TV%076", "'15'",
               "-cost%FT_T2%095", "'15'", "-cost%FT_TV%073", "'15'",
               "-cost%FT_T2%096", "'15'", "-cost%FT_TV%074", "'15'",
               "-cost%FT_T2%093", "'15'", "-cost%FT_TV%071", "'15'",
               "-cost%FT_T2%094", "'15'", "-cost%FT_TV%072", "'15'",
               "-cost%FT_T2%088", "'15'", "-cost%FT_TV%080", "'15'",
               "-cost%FT_T2%089", "'15'", "-cost%FT_TV%081", "'15'",
               "-cost%FT_T2%086", "'15'", "-cost%FT_T2%087", "'15'",
               "-cost%FT_T1%292", "'15'", "-cost%FT_T1%293", "'15'",
               "-cost%FT_T1%294", "'15'", "-cost%FT_T1%295", "'15'",
               "-cost%FT_T1%290", "'15'", "-cost%FT_T1%291", "'15'",
               "-cost%FT_T1%256", "'15'", "-cost%FT_T1%257", "'15'",
               "-cost%FT_T1%258", "'15'", "-cost%FT_T1%259", "'15'",
               "-cost%FT_T1%252", "'15'", "-cost%FT_TV%048", "'15'",
               "-cost%FT_T1%253", "'15'", "-cost%FT_TV%049", "'15'",
               "-cost%FT_T1%254", "'15'", "-cost%FT_TV%046", "'15'",
               "-cost%FT_T1%255", "'15'", "-cost%FT_TV%047", "'15'",
               "-cost%FT_TV%044", "'15'", "-cost%FT_TV%045", "'15'",
               "-cost%FT_TV%042", "'15'", "-cost%FT_TV%043", "'15'",
               "-cost%FT_T2%062", "'15'", "-cost%FT_TV%040", "'15'",
               "-cost%FT_T2%063", "'15'", "-cost%FT_TV%041", "'15'",
               "-cost%FT_T2%060", "'15'", "-cost%FT_T2%061", "'15'",
               "-cost%FT_T2%055", "'15'", "-cost%FT_T2%056", "'15'",
               "-cost%FT_T2%053", "'15'", "-cost%FT_T2%054", "'15'",
               "-cost%FT_T2%059", "'15'", "-cost%FT_T2%057", "'15'",
               "-cost%FT_T2%058", "'15'", "-cost%FT_T1%260", "'15'",
               "-cost%FT_T1%261", "'15'", "-cost%FT_T1%262", "'15'",
               "-cost%FT_T1%267", "'15'", "-cost%FT_T1%268", "'15'",
               "-cost%FT_T1%269", "'15'", "-cost%FT_T1%263", "'15'",
               "-cost%FT_TV%059", "'15'", "-cost%FT_T1%264", "'15'",
               "-cost%FT_T1%265", "'15'", "-cost%FT_TV%057", "'15'",
               "-cost%FT_T1%266", "'15'", "-cost%FT_TV%058", "'15'",
               "-cost%FT_TV%055", "'15'", "-cost%FT_T2%070", "'15'",
               "-cost%FT_TV%056", "'15'", "-cost%FT_TV%053", "'15'",
               "-cost%FT_TV%054", "'15'", "-cost%FT_T2%073", "'15'",
               "-cost%FT_TV%051", "'15'", "-cost%FT_T2%074", "'15'",
               "-cost%FT_TV%052", "'15'", "-cost%FT_T2%071", "'15'",
               "-cost%FT_T2%072", "'15'", "-cost%FT_TV%050", "'15'",
               "-cost%FT_T2%066", "'15'", "-cost%FT_T2%067", "'15'",
               "-cost%FT_T2%064", "'15'", "-cost%FT_T2%065", "'15'",
               "-cost%FT_T2%068", "'15'", "-cost%FT_T2%069", "'15'",
               "-cost%FT_T1%270", "'15'", "-cost%FT_T1%271", "'15'",
               "-cost%FT_T1%272", "'15'", "-cost%FT_T1%273", "'15'",
               "-cost%FT_T1%234", "'15'", "-cost%FT_T1%235", "'15'",
               "-cost%FT_T1%236", "'15'", "-cost%FT_TV%028", "'15'",
               "-cost%FT_T1%237", "'15'", "-cost%FT_TV%029", "'15'",
               "-cost%FT_T1%230", "'15'", "-cost%FT_TV%026", "'15'",
               "-cost%FT_T1%231", "'15'", "-cost%FT_TV%027", "'15'",
               "-cost%FT_T1%232", "'15'", "-cost%FT_TV%024", "'15'",
               "-cost%FT_T1%233", "'15'", "-cost%FT_TV%025", "'15'",
               "-cost%FT_TV%022", "'15'", "-cost%FT_TV%023", "'15'",
               "-cost%FT_TV%020", "'15'", "-cost%FT_TV%021", "'15'",
               "-cost%FT_T1%238", "'15'", "-cost%FT_T2%040", "'15'",
               "-cost%FT_T1%239", "'15'", "-cost%FT_T2%041", "'15'",
               "-cost%FT_T2%033", "'15'", "-cost%FT_T2%034", "'15'",
               "-cost%FT_T2%031", "'15'", "-cost%FT_T2%032", "'15'",
               "-cost%FT_T2%037", "'15'", "-cost%FT_T2%038", "'15'",
               "-cost%FT_T2%035", "'15'", "-cost%FT_T2%036", "'15'",
               "-cost%FT_T2%039", "'15'", "-cost%FT_T1%240", "'15'",
               "-cost%FT_T1%245", "'15'", "-cost%FT_T1%246", "'15'",
               "-cost%FT_T1%247", "'15'", "-cost%FT_TV%039", "'15'",
               "-cost%FT_T1%248", "'15'", "-cost%FT_T1%241", "'15'",
               "-cost%FT_TV%037", "'15'", "-cost%FT_T1%242", "'15'",
               "-cost%FT_TV%038", "'15'", "-cost%FT_T1%243", "'15'",
               "-cost%FT_TV%035", "'15'", "-n_stores", "'15'", "-cost%FT_T1%244",
               "'15'", "-cost%FT_TV%036", "'15'", "-cost%FT_TV%033", "'15'",
               "-cost%FT_TV%034", "'15'", "-cost%FT_TV%031", "'15'",
               "-cost%FT_TV%032", "'15'", "-cost%FT_T1%249", "'15'",
               "-cost%FT_T2%051", "'15'", "-cost%FT_T2%052", "'15'",
               "-cost%FT_TV%030", "'15'", "-cost%FT_T2%050", "'15'",
               "-cost%FT_T2%044", "'15'", "-cost%FT_T2%045", "'15'",
               "-cost%FT_T2%042", "'15'", "-cost%FT_T2%043", "'15'",
               "-cost%FT_T2%048", "'15'", "-cost%FT_T2%049", "'15'",
               "-cost%FT_T2%046", "'15'", "-cost%FT_T2%047", "'15'",
               "-cost%FT_T1%250", "'15'", "-cost%FT_T1%251", "'15'",
               "-cost%FT_T1%212", "'15'", "-cost%FT_T1%696", "'15'",
               "-cost%FT_TV%008", "'15'", "-cost%FT_T1%213", "'15'",
               "-cost%FT_T1%697", "'15'", "-cost%FT_TV%009", "'15'",
               "-cost%FT_T1%214", "'15'", "-cost%FT_T1%698", "'15'",
               "-cost%FT_TV%006", "'15'", "-cost%FT_T1%215", "'15'",
               "-cost%FT_T1%699", "'15'", "-cost%FT_TV%007", "'15'",
               "-cost%FT_T1%692", "'15'", "-cost%FT_TV%004", "'15'",
               "-cost%FT_TV%488", "'15'", "-cost%FT_T1%693", "'15'",
               "-cost%FT_TV%005", "'15'", "-cost%FT_TV%489", "'15'",
               "-cost%FT_T1%210", "'15'", "-cost%FT_T1%694", "'15'",
               "-cost%FT_TV%002", "'15'", "-cost%FT_TV%486", "'15'",
               "-cost%FT_T1%211", "'15'", "-cost%FT_T1%695", "'15'",
               "-cost%FT_TV%003", "'15'", "-cost%FT_TV%487", "'15'",
               "-cost%FT_TV%484", "'15'", "-cost%FT_TV%001", "'15'",
               "-cost%FT_TV%485", "'15'", "-cost%FT_TV%482", "'15'",
               "-cost%FT_TV%483", "'15'", "-cost%FT_T1%216", "'15'",
               "-cost%FT_TV%480", "'15'", "-cost%FT_T1%217", "'15'",
               "-cost%FT_TV%481", "'15'", "-cost%FT_T1%218", "'15'",
               "-cost%FT_T1%219", "'15'", "-cost%FT_T2%011", "'15'",
               "-cost%FT_T2%495", "'15'", "-cost%FT_T2%012", "'15'",
               "-cost%FT_T2%496", "'15'", "-cost%FT_T2%493", "'15'",
               "-cost%FT_T2%010", "'15'", "-cost%FT_T2%494", "'15'",
               "-cost%FT_T2%015", "'15'", "-cost%FT_T2%499", "'15'",
               "-cost%FT_T2%016", "'15'", "-cost%FT_T2%013", "'15'",
               "-cost%FT_T2%497", "'15'", "-cost%FT_T2%014", "'15'",
               "-cost%FT_T2%498", "'15'", "-cost%FT_T2%019", "'15'",
               "-cost%FT_T2%017", "'15'", "-cost%FT_T2%018", "'15'",
               "-cost%FT_T1%223", "'15'", "-cost%FT_TV%019", "'15'",
               "-cost%FT_T1%224", "'15'", "-cost%FT_T1%225", "'15'",
               "-cost%FT_TV%017", "'15'", "-cost%FT_T1%226", "'15'",
               "-cost%FT_TV%018", "'15'", "-cost%FT_TV%015", "'15'",
               "-cost%FT_TV%499", "'15'", "-cost%FT_T1%220", "'15'",
               "-cost%FT_TV%016", "'15'", "-cost%FT_T1%221", "'15'",
               "-cost%FT_TV%013", "'15'", "-cost%FT_TV%497", "'15'",
               "-cost%FT_T1%222", "'15'", "-cost%FT_TV%014", "'15'",
               "-cost%FT_TV%498", "'15'", "-cost%FT_TV%011", "'15'",
               "-cost%FT_TV%495", "'15'", "-cost%FT_TV%012", "'15'",
               "-cost%FT_TV%496", "'15'", "-cost%FT_TV%493", "'15'",
               "-cost%FT_TV%010", "'15'", "-cost%FT_TV%494", "'15'",
               "-cost%FT_T1%227", "'15'", "-cost%FT_TV%491", "'15'",
               "-cost%FT_T1%228", "'15'", "-cost%FT_T2%030", "'15'",
               "-cost%FT_TV%492", "'15'", "-cost%FT_T1%229", "'15'",
               "-cost%FT_TV%490", "'15'", "-cost%FT_T2%022", "'15'",
               "-cost%FT_T2%023", "'15'", "-cost%FT_T2%020", "'15'",
               "-cost%FT_T2%021", "'15'", "-cost%FT_T2%026", "'15'",
               "-cost%FT_T2%027", "'15'", "-cost%FT_T2%024", "'15'",
               "-cost%FT_T2%025", "'15'", "-cost%FT_T2%028", "'15'",
               "-cost%FT_T2%029", "'15'", "-opencost%FT%030", "'15'",
               "-opencost%FT%005", "'15'", "-opencost%FT%006", "'15'",
               "-opencost%FT%007", "'15'", "-opencost%FT%008", "'15'",
               "-opencost%FT%001", "'15'", "-opencost%FT%002", "'15'",
               "-opencost%FT%003", "'15'", "-opencost%FT%004", "'15'",
               "-opencost%FT%009", "'15'", "-opencost%FT%027", "'15'",
               "-opencost%FT%028", "'15'", "-opencost%FT%029", "'15'",
               "-opencost%FT%023", "'15'", "-opencost%FT%024", "'15'",
               "-opencost%FT%025", "'15'", "-opencost%FT%026", "'15'",
               "-opencost%FT%020", "'15'", "-opencost%FT%021", "'15'",
               "-opencost%FT%022", "'15'", "-opencost%FT%016", "'15'",
               "-opencost%FT%017", "'15'", "-opencost%FT%018", "'15'",
               "-opencost%FT%019", "'15'", "-opencost%FT%012", "'15'",
               "-opencost%FT%013", "'15'", "-opencost%FT%014", "'15'",
               "-opencost%FT%015", "'15'", "-opencost%FT%010", "'15'",
               "-opencost%FT%011", "'15'", "-cost%FT_TV%745", "'15'",
               "-cost%FT_TV%746", "'15'", "-cost%FT_T1%830", "'15'",
               "-cost%FT_TV%743", "'15'", "-cost%FT_T1%831", "'15'",
               "-cost%FT_TV%744", "'15'", "-cost%FT_TV%741", "'15'",
               "-cost%FT_TV%742", "'15'", "-cost%FT_TV%740", "'15'",
               "-cost%FT_T1%836", "'15'", "-cost%FT_T1%837", "'15'",
               "-cost%FT_T1%838", "'15'", "-cost%FT_T1%839", "'15'",
               "-cost%FT_T1%832", "'15'", "-cost%FT_T1%833", "'15'",
               "-cost%FT_T1%834", "'15'", "-cost%FT_T1%835", "'15'",
               "-cost%FT_T2%631", "'15'", "-cost%FT_T2%632", "'15'",
               "-cost%FT_T2%630", "'15'", "-cost%FT_T2%635", "'15'",
               "-cost%FT_T2%636", "'15'", "-cost%FT_T2%633", "'15'",
               "-cost%FT_T2%634", "'15'", "-cost%FT_T2%639", "'15'",
               "-cost%FT_TV%749", "'15'", "-cost%FT_T2%637", "'15'",
               "-cost%FT_TV%747", "'15'", "-cost%FT_T2%638", "'15'",
               "-cost%FT_TV%748", "'15'", "-cost%FT_TV%756", "'15'",
               "-cost%FT_T1%840", "'15'", "-cost%FT_TV%757", "'15'",
               "-cost%FT_T1%841", "'15'", "-cost%FT_TV%754", "'15'",
               "-cost%FT_T1%842", "'15'", "-cost%FT_TV%755", "'15'",
               "-cost%FT_TV%752", "'15'", "-cost%FT_TV%753", "'15'",
               "-cost%FT_TV%750", "'15'", "-cost%FT_TV%751", "'15'",
               "-cost%FT_T1%847", "'15'", "-cost%FT_T1%848", "'15'",
               "-cost%FT_T1%849", "'15'", "-cost%FT_T1%843", "'15'",
               "-cost%FT_T1%844", "'15'", "-cost%FT_T1%845", "'15'",
               "-cost%FT_T1%846", "'15'", "-cost%FT_T2%642", "'15'",
               "-cost%FT_T2%643", "'15'", "-cost%FT_T2%640", "'15'",
               "-cost%FT_T2%641", "'15'", "-cost%FT_T2%646", "'15'",
               "-cost%FT_T2%647", "'15'", "-cost%FT_T2%644", "'15'",
               "-cost%FT_T2%645", "'15'", "-cost%FT_T2%648", "'15'",
               "-cost%FT_TV%758", "'15'", "-cost%FT_T2%649", "'15'",
               "-cost%FT_TV%759", "'15'", "-cost%FT_TV%723", "'15'",
               "-cost%FT_TV%724", "'15'", "-cost%FT_T2%619", "'15'",
               "-cost%FT_TV%721", "'15'", "-cost%FT_TV%722", "'15'",
               "-cost%FT_TV%720", "'15'", "-cost%FT_T1%814", "'15'",
               "-cost%FT_T1%815", "'15'", "-cost%FT_T1%816", "'15'",
               "-cost%FT_T1%817", "'15'", "-cost%FT_T1%810", "'15'",
               "-cost%FT_T1%811", "'15'", "-cost%FT_T1%812", "'15'",
               "-cost%FT_T1%813", "'15'", "-cost%FT_T1%818", "'15'",
               "-cost%FT_T1%819", "'15'", "-cost%FT_T2%610", "'15'",
               "-cost%FT_T2%613", "'15'", "-cost%FT_T2%614", "'15'",
               "-cost%FT_T2%611", "'15'", "-cost%FT_TV%729", "'15'",
               "-cost%FT_T2%612", "'15'", "-cost%FT_T2%617", "'15'",
               "-cost%FT_TV%727", "'15'", "-cost%FT_T2%618", "'15'",
               "-cost%FT_TV%728", "'15'", "-cost%FT_T2%615", "'15'",
               "-cost%FT_TV%725", "'15'", "-cost%FT_T2%616", "'15'",
               "-cost%FT_TV%726", "'15'", "-cost%FT_TV%734", "'15'",
               "-cost%FT_TV%735", "'15'", "-cost%FT_TV%732", "'15'",
               "-cost%FT_T1%820", "'15'", "-cost%FT_TV%733", "'15'",
               "-cost%FT_TV%730", "'15'", "-cost%FT_TV%731", "'15'",
               "-cost%FT_T1%825", "'15'", "-cost%FT_T1%826", "'15'",
               "-cost%FT_T1%827", "'15'", "-cost%FT_T1%828", "'15'",
               "-cost%FT_T1%821", "'15'", "-cost%FT_T1%822", "'15'",
               "-cost%FT_T1%823", "'15'", "-cost%FT_T1%824", "'15'",
               "-cost%FT_T1%829", "'15'", "-cost%FT_T2%620", "'15'",
               "-cost%FT_T2%621", "'15'", "-cost%FT_T2%624", "'15'",
               "-cost%FT_T2%625", "'15'", "-cost%FT_T2%622", "'15'",
               "-cost%FT_T2%623", "'15'", "-cost%FT_T2%628", "'15'",
               "-cost%FT_TV%738", "'15'", "-cost%FT_T2%629", "'15'",
               "-cost%FT_TV%739", "'15'", "-cost%FT_T2%626", "'15'",
               "-cost%FT_TV%736", "'15'", "-cost%FT_T2%627", "'15'",
               "-cost%FT_TV%737", "'15'", "-cost%FT_TV%701", "'15'",
               "-cost%FT_TV%702", "'15'", "-cost%FT_TV%700", "'15'",
               "-cost%FT_TV%709", "'15'", "-cost%FT_TV%707", "'15'",
               "-cost%FT_TV%708", "'15'", "-cost%FT_TV%705", "'15'",
               "-cost%FT_TV%706", "'15'", "-cost%FT_TV%703", "'15'",
               "-cost%FT_TV%704", "'15'", "-cost%FT_TV%712", "'15'",
               "-cost%FT_TV%713", "'15'", "-cost%FT_T2%608", "'15'",
               "-cost%FT_TV%710", "'15'", "-cost%FT_T2%609", "'15'",
               "-cost%FT_TV%711", "'15'", "-cost%FT_T1%803", "'15'",
               "-cost%FT_T1%804", "'15'", "-cost%FT_T1%805", "'15'",
               "-cost%FT_T1%806", "'15'", "-cost%FT_T1%800", "'15'",
               "-cost%FT_T1%801", "'15'", "-cost%FT_T1%802", "'15'",
               "-cost%FT_T1%807", "'15'", "-cost%FT_T1%808", "'15'",
               "-cost%FT_T1%809", "'15'", "-cost%FT_T2%602", "'15'",
               "-cost%FT_T2%603", "'15'", "-cost%FT_T2%600", "'15'",
               "-cost%FT_TV%718", "'15'", "-cost%FT_T2%601", "'15'",
               "-cost%FT_TV%719", "'15'", "-cost%FT_T2%606", "'15'",
               "-cost%FT_TV%716", "'15'", "-cost%FT_T2%607", "'15'",
               "-cost%FT_TV%717", "'15'", "-cost%FT_T2%604", "'15'",
               "-cost%FT_TV%714", "'15'", "-cost%FT_T2%605", "'15'",
               "-cost%FT_TV%715", "'15'", "-cost%FT_T1%311", "'15'",
               "-cost%FT_T1%795", "'15'", "-cost%FT_TV%349", "'15'",
               "-cost%FT_T1%312", "'15'", "-cost%FT_T1%796", "'15'",
               "-cost%FT_T1%313", "'15'", "-cost%FT_T1%797", "'15'",
               "-cost%FT_TV%347", "'15'", "-cost%FT_T1%314", "'15'",
               "-cost%FT_T1%798", "'15'", "-cost%FT_TV%348", "'15'",
               "-cost%FT_T1%791", "'15'", "-cost%FT_TV%345", "'15'",
               "-cost%FT_T1%792", "'15'", "-cost%FT_TV%346", "'15'",
               "-cost%FT_T1%793", "'15'", "-cost%FT_TV%343", "'15'",
               "-cost%FT_T1%310", "'15'", "-cost%FT_T1%794", "'15'",
               "-cost%FT_TV%344", "'15'", "-cost%FT_T1%319", "'15'",
               "-cost%FT_TV%341", "'15'", "-cost%FT_TV%342", "'15'",
               "-cost%FT_TV%340", "'15'", "-cost%FT_T1%315", "'15'",
               "-cost%FT_T1%799", "'15'", "-cost%FT_T1%316", "'15'",
               "-cost%FT_T1%317", "'15'", "-cost%FT_T1%318", "'15'",
               "-cost%FT_T2%110", "'15'", "-cost%FT_T2%594", "'15'",
               "-cost%FT_T2%111", "'15'", "-cost%FT_T2%595", "'15'",
               "-cost%FT_T2%592", "'15'", "-cost%FT_T2%593", "'15'",
               "-cost%FT_T2%114", "'15'", "-cost%FT_T2%598", "'15'",
               "-cost%FT_T2%115", "'15'", "-cost%FT_T2%599", "'15'",
               "-cost%FT_T2%112", "'15'", "-cost%FT_T2%596", "'15'",
               "-cost%FT_T2%113", "'15'", "-cost%FT_T2%597", "'15'",
               "-cost%FT_T2%118", "'15'", "-cost%FT_T2%119", "'15'",
               "-cost%FT_T2%116", "'15'", "-cost%FT_T2%117", "'15'",
               "-cost%FT_T1%322", "'15'", "-cost%FT_T1%323", "'15'",
               "-cost%FT_T1%324", "'15'", "-cost%FT_TV%358", "'15'",
               "-cost%FT_T1%325", "'15'", "-cost%FT_TV%359", "'15'",
               "-cost%FT_TV%356", "'15'", "-cost%FT_TV%357", "'15'",
               "-cost%FT_T1%320", "'15'", "-cost%FT_TV%354", "'15'",
               "-cost%FT_T1%321", "'15'", "-cost%FT_TV%355", "'15'",
               "-cost%FT_TV%352", "'15'", "-cost%FT_TV%353", "'15'",
               "-cost%FT_TV%350", "'15'", "-cost%FT_TV%351", "'15'",
               "-cost%FT_T1%326", "'15'", "-cost%FT_T1%327", "'15'",
               "-cost%FT_T1%328", "'15'", "-cost%FT_T1%329", "'15'",
               "-cost%FT_T2%121", "'15'", "-cost%FT_T2%122", "'15'",
               "-cost%FT_T2%120", "'15'", "-cost%FT_T2%125", "'15'",
               "-cost%FT_T2%126", "'15'", "-cost%FT_T2%123", "'15'",
               "-cost%FT_T2%124", "'15'", "-cost%FT_T2%129", "'15'",
               "-cost%FT_T2%127", "'15'", "-cost%FT_T2%128", "'15'",
               "-cost%FT_T1%773", "'15'", "-cost%FT_TV%327", "'15'",
               "-cost%FT_T1%774", "'15'", "-cost%FT_TV%328", "'15'",
               "-cost%FT_T1%775", "'15'", "-cost%FT_TV%325", "'15'",
               "-cost%FT_T1%776", "'15'", "-cost%FT_TV%326", "'15'",
               "-cost%FT_TV%323", "'15'", "-cost%FT_T1%770", "'15'",
               "-cost%FT_TV%324", "'15'", "-cost%FT_T1%771", "'15'",
               "-cost%FT_TV%321", "'15'", "-cost%FT_T1%772", "'15'",
               "-cost%FT_TV%322", "'15'", "-cost%FT_TV%320", "'15'",
               "-cost%FT_T1%777", "'15'", "-cost%FT_T1%778", "'15'",
               "-cost%FT_T2%580", "'15'", "-cost%FT_T1%779", "'15'",
               "-cost%FT_T2%572", "'15'", "-cost%FT_T2%573", "'15'",
               "-cost%FT_T2%570", "'15'", "-cost%FT_T2%571", "'15'",
               "-cost%FT_T2%576", "'15'", "-cost%FT_T2%577", "'15'",
               "-cost%FT_T2%574", "'15'", "-cost%FT_T2%575", "'15'",
               "-cost%FT_T2%578", "'15'", "-cost%FT_T2%579", "'15'",
               "-cost%FT_TV%329", "'15'", "-cost%FT_T1%300", "'15'",
               "-cost%FT_T1%784", "'15'", "-cost%FT_TV%338", "'15'",
               "-cost%FT_T1%301", "'15'", "-cost%FT_T1%785", "'15'",
               "-cost%FT_TV%339", "'15'", "-cost%FT_T1%302", "'15'",
               "-cost%FT_T1%786", "'15'", "-cost%FT_TV%336", "'15'",
               "-cost%FT_T1%303", "'15'", "-cost%FT_T1%787", "'15'",
               "-cost%FT_TV%337", "'15'", "-cost%FT_T1%780", "'15'",
               "-cost%FT_TV%334", "'15'", "-cost%FT_T1%781", "'15'",
               "-cost%FT_TV%335", "'15'", "-cost%FT_T1%782", "'15'",
               "-cost%FT_TV%332", "'15'", "-cost%FT_T1%783", "'15'",
               "-cost%FT_TV%333", "'15'", "-cost%FT_T1%308", "'15'",
               "-cost%FT_TV%330", "'15'", "-cost%FT_T1%309", "'15'",
               "-cost%FT_TV%331", "'15'", "-cost%FT_T1%304", "'15'",
               "-cost%FT_T1%788", "'15'", "-cost%FT_T2%590", "'15'",
               "-cost%FT_T1%305", "'15'", "-cost%FT_T1%789", "'15'",
               "-cost%FT_T2%591", "'15'", "-cost%FT_T1%306", "'15'",
               "-cost%FT_T1%307", "'15'", "-cost%FT_T2%583", "'15'",
               "-cost%FT_T2%100", "'15'", "-cost%FT_T2%584", "'15'",
               "-cost%FT_T2%581", "'15'", "-cost%FT_T2%582", "'15'",
               "-cost%FT_T2%103", "'15'", "-cost%FT_T2%587", "'15'",
               "-cost%FT_T2%104", "'15'", "-cost%FT_T2%588", "'15'",
               "-cost%FT_T2%101", "'15'", "-cost%FT_T2%585", "'15'",
               "-cost%FT_T2%102", "'15'", "-cost%FT_T2%586", "'15'",
               "-cost%FT_T2%107", "'15'", "-cost%FT_T2%108", "'15'",
               "-cost%FT_T2%105", "'15'", "-cost%FT_T2%589", "'15'",
               "-cost%FT_T1%790", "'15'", "-cost%FT_T2%106", "'15'",
               "-cost%FT_T2%109", "'15'", "-cost%FT_T1%751", "'15'",
               "-cost%FT_TV%305", "'15'", "-cost%FT_TV%789", "'15'",
               "-cost%FT_T1%752", "'15'", "-cost%FT_TV%306", "'15'",
               "-cost%FT_T1%753", "'15'", "-cost%FT_TV%303", "'15'",
               "-cost%FT_TV%787", "'15'", "-cost%FT_T1%754", "'15'",
               "-cost%FT_TV%304", "'15'", "-cost%FT_TV%788", "'15'",
               "-cost%FT_TV%301", "'15'", "-cost%FT_TV%785", "'15'",
               "-cost%FT_TV%302", "'15'", "-cost%FT_TV%786", "'15'",
               "-cost%FT_TV%783", "'15'", "-cost%FT_T1%750", "'15'",
               "-cost%FT_TV%300", "'15'", "-cost%FT_TV%784", "'15'",
               "-cost%FT_T1%759", "'15'", "-cost%FT_TV%781", "'15'",
               "-cost%FT_TV%782", "'15'", "-cost%FT_TV%780", "'15'",
               "-cost%FT_T1%755", "'15'", "-cost%FT_T1%756", "'15'",
               "-cost%FT_T1%757", "'15'", "-cost%FT_T1%758", "'15'",
               "-cost%FT_T2%550", "'15'", "-cost%FT_T2%551", "'15'",
               "-cost%FT_T2%554", "'15'", "-cost%FT_T2%555", "'15'",
               "-cost%FT_T2%552", "'15'", "-cost%FT_T2%553", "'15'",
               "-cost%FT_T2%558", "'15'", "-cost%FT_T2%559", "'15'",
               "-cost%FT_T2%556", "'15'", "-cost%FT_T2%557", "'15'",
               "-cost%FT_TV%309", "'15'", "-cost%FT_TV%307", "'15'",
               "-cost%FT_TV%308", "'15'", "-cost%FT_T1%762", "'15'",
               "-cost%FT_TV%316", "'15'", "-cost%FT_T1%763", "'15'",
               "-cost%FT_TV%317", "'15'", "-cost%FT_T1%764", "'15'",
               "-cost%FT_TV%314", "'15'", "-cost%FT_TV%798", "'15'",
               "-cost%FT_T1%765", "'15'", "-cost%FT_TV%315", "'15'",
               "-cost%FT_TV%799", "'15'", "-cost%FT_TV%312", "'15'",
               "-cost%FT_TV%796", "'15'", "-cost%FT_TV%313", "'15'",
               "-cost%FT_TV%797", "'15'", "-cost%FT_T1%760", "'15'",
               "-cost%FT_TV%310", "'15'", "-cost%FT_TV%794", "'15'",
               "-cost%FT_T1%761", "'15'", "-cost%FT_TV%311", "'15'",
               "-cost%FT_TV%795", "'15'", "-cost%FT_TV%792", "'15'",
               "-cost%FT_TV%793", "'15'", "-cost%FT_TV%790", "'15'",
               "-cost%FT_TV%791", "'15'", "-cost%FT_T1%766", "'15'",
               "-cost%FT_T1%767", "'15'", "-cost%FT_T1%768", "'15'",
               "-cost%FT_T1%769", "'15'", "-cost%FT_T2%561", "'15'",
               "-cost%FT_T2%562", "'15'", "-cost%FT_T2%560", "'15'",
               "-cost%FT_T2%565", "'15'", "-cost%FT_T2%566", "'15'",
               "-cost%FT_T2%563", "'15'", "-cost%FT_T2%564", "'15'",
               "-cost%FT_T2%569", "'15'", "-cost%FT_T2%567", "'15'",
               "-cost%FT_T2%568", "'15'", "-cost%FT_TV%318", "'15'",
               "-cost%FT_TV%319", "'15'", "-cost%FT_TV%767", "'15'",
               "-cost%FT_T1%730", "'15'", "-cost%FT_TV%768", "'15'",
               "-cost%FT_T1%731", "'15'", "-cost%FT_TV%765", "'15'",
               "-cost%FT_T1%732", "'15'", "-cost%FT_TV%766", "'15'",
               "-cost%FT_TV%763", "'15'", "-cost%FT_TV%764", "'15'",
               "-cost%FT_TV%761", "'15'", "-cost%FT_TV%762", "'15'",
               "-cost%FT_T1%737", "'15'", "-cost%FT_T1%738", "'15'",
               "-cost%FT_TV%760", "'15'", "-cost%FT_T1%739", "'15'",
               "-cost%FT_T1%733", "'15'", "-cost%FT_T1%734", "'15'",
               "-cost%FT_T1%735", "'15'", "-cost%FT_T1%736", "'15'",
               "-cost%FT_T2%532", "'15'", "-cost%FT_T2%533", "'15'",
               "-cost%FT_T2%530", "'15'", "-cost%FT_T2%531", "'15'",
               "-cost%FT_T2%536", "'15'", "-cost%FT_T2%537", "'15'",
               "-cost%FT_T2%534", "'15'", "-cost%FT_T2%535", "'15'",
               "-cost%FT_T2%538", "'15'", "-cost%FT_TV%769", "'15'",
               "-cost%FT_T2%539", "'15'", "-cost%FT_T1%740", "'15'",
               "-cost%FT_TV%778", "'15'", "-cost%FT_T1%741", "'15'",
               "-cost%FT_TV%779", "'15'", "-cost%FT_T1%742", "'15'",
               "-cost%FT_TV%776", "'15'", "-cost%FT_T1%743", "'15'",
               "-cost%FT_TV%777", "'15'", "-cost%FT_TV%774", "'15'",
               "-cost%FT_TV%775", "'15'", "-cost%FT_TV%772", "'15'",
               "-cost%FT_TV%773", "'15'", "-cost%FT_T1%748", "'15'",
               "-cost%FT_TV%770", "'15'", "-cost%FT_T1%749", "'15'",
               "-cost%FT_TV%771", "'15'", "-cost%FT_T1%744", "'15'",
               "-cost%FT_T1%745", "'15'", "-cost%FT_T1%746", "'15'",
               "-cost%FT_T1%747", "'15'", "-cost%FT_T2%540", "'15'",
               "-cost%FT_T2%543", "'15'", "-cost%FT_T2%544", "'15'",
               "-cost%FT_T2%541", "'15'", "-cost%FT_T2%542", "'15'",
               "-cost%FT_T2%547", "'15'", "-cost%FT_T2%548", "'15'",
               "-cost%FT_T2%545", "'15'", "-cost%FT_T2%546", "'15'",
               "-cost%FT_T2%549", "'15'", "-cost%FT_T1%399", "'15'",
               "-cost%FT_T1%395", "'15'", "-cost%FT_T1%396", "'15'",
               "-cost%FT_T1%397", "'15'", "-cost%FT_T1%398", "'15'",
               "-cost%FT_T2%198", "'15'", "-cost%FT_T2%199", "'15'",
               "-cost%FT_T2%196", "'15'", "-cost%FT_T2%197", "'15'",
               "-cost%FT_T1%377", "'15'", "-cost%FT_T1%378", "'15'",
               "-cost%FT_T1%379", "'15'", "-cost%FT_T1%373", "'15'",
               "-cost%FT_T1%374", "'15'", "-cost%FT_T1%375", "'15'",
               "-cost%FT_T1%376", "'15'", "-cost%FT_T2%180", "'15'",
               "-cost%FT_T2%183", "'15'", "-cost%FT_T2%184", "'15'",
               "-cost%FT_T2%181", "'15'", "-cost%FT_T2%182", "'15'",
               "-cost%FT_T2%176", "'15'", "-cost%FT_T2%177", "'15'",
               "-cost%FT_T2%174", "'15'", "-cost%FT_T2%175", "'15'",
               "-cost%FT_T2%178", "'15'", "-cost%FT_T2%179", "'15'",
               "-cost%FT_T1%380", "'15'", "-cost%FT_T1%381", "'15'",
               "-cost%FT_T1%382", "'15'", "-cost%FT_T1%383", "'15'",
               "-cost%FT_T1%388", "'15'", "-cost%FT_T1%389", "'15'",
               "-cost%FT_T1%384", "'15'", "-cost%FT_T1%385", "'15'",
               "-cost%FT_T1%386", "'15'", "-cost%FT_T1%387", "'15'",
               "-cost%FT_T2%190", "'15'", "-cost%FT_T2%191", "'15'",
               "-cost%FT_T2%194", "'15'", "-cost%FT_T2%195", "'15'",
               "-cost%FT_T2%192", "'15'", "-cost%FT_T2%193", "'15'",
               "-cost%FT_T2%187", "'15'", "-cost%FT_T2%188", "'15'",
               "-cost%FT_T2%185", "'15'", "-cost%FT_T2%186", "'15'",
               "-cost%FT_T2%189", "'15'", "-cost%FT_T1%391", "'15'",
               "-cost%FT_T1%392", "'15'", "-cost%FT_T1%393", "'15'",
               "-cost%FT_T1%394", "'15'", "-cost%FT_T1%390", "'15'",
               "-cost%FT_T1%355", "'15'", "-cost%FT_T1%356", "'15'",
               "-cost%FT_T1%357", "'15'", "-cost%FT_T1%358", "'15'",
               "-cost%FT_T1%351", "'15'", "-cost%FT_TV%389", "'15'",
               "-cost%FT_T1%352", "'15'", "-cost%FT_T1%353", "'15'",
               "-cost%FT_TV%387", "'15'", "-cost%FT_T1%354", "'15'",
               "-cost%FT_TV%388", "'15'", "-cost%FT_TV%385", "'15'",
               "-cost%FT_TV%386", "'15'", "-cost%FT_TV%383", "'15'",
               "-cost%FT_TV%384", "'15'", "-cost%FT_T1%359", "'15'",
               "-cost%FT_T2%161", "'15'", "-cost%FT_TV%381", "'15'",
               "-cost%FT_T2%162", "'15'", "-cost%FT_TV%382", "'15'",
               "-cost%FT_T2%160", "'15'", "-cost%FT_TV%380", "'15'",
               "-cost%FT_T2%154", "'15'", "-cost%FT_T2%155", "'15'",
               "-cost%FT_T2%152", "'15'", "-cost%FT_T2%153", "'15'",
               "-cost%FT_T2%158", "'15'", "-cost%FT_T2%159", "'15'",
               "-cost%FT_T2%156", "'15'", "-cost%FT_T2%157", "'15'",
               "-cost%FT_T1%360", "'15'", "-cost%FT_T1%361", "'15'",
               "-cost%FT_T1%366", "'15'", "-cost%FT_T1%367", "'15'",
               "-cost%FT_T1%368", "'15'", "-cost%FT_T1%369", "'15'",
               "-cost%FT_T1%362", "'15'", "-cost%FT_T1%363", "'15'",
               "-cost%FT_T1%364", "'15'", "-cost%FT_TV%398", "'15'",
               "-cost%FT_T1%365", "'15'", "-cost%FT_TV%399", "'15'",
               "-cost%FT_TV%396", "'15'", "-cost%FT_TV%397", "'15'",
               "-cost%FT_TV%394", "'15'", "-cost%FT_TV%395", "'15'",
               "-cost%FT_T2%172", "'15'", "-cost%FT_TV%392", "'15'",
               "-cost%FT_T2%173", "'15'", "-cost%FT_TV%393", "'15'",
               "-cost%FT_T2%170", "'15'", "-cost%FT_TV%390", "'15'",
               "-cost%FT_T2%171", "'15'", "-cost%FT_TV%391", "'15'",
               "-cost%FT_T2%165", "'15'", "-cost%FT_T2%166", "'15'",
               "-cost%FT_T2%163", "'15'", "-cost%FT_T2%164", "'15'",
               "-cost%FT_T2%169", "'15'", "-cost%FT_T2%167", "'15'",
               "-cost%FT_T2%168", "'15'", "-cost%FT_T1%370", "'15'",
               "-cost%FT_T1%371", "'15'", "-cost%FT_T1%372", "'15'",
               "-cost%FT_T1%333", "'15'", "-cost%FT_T1%334", "'15'",
               "-cost%FT_T1%335", "'15'", "-cost%FT_TV%369", "'15'",
               "-cost%FT_T1%336", "'15'", "-cost%FT_TV%367", "'15'",
               "-cost%FT_T1%330", "'15'", "-cost%FT_TV%368", "'15'",
               "-cost%FT_T1%331", "'15'", "-cost%FT_TV%365", "'15'",
               "-cost%FT_T1%332", "'15'", "-cost%FT_TV%366", "'15'",
               "-cost%FT_TV%363", "'15'", "-cost%FT_TV%364", "'15'",
               "-cost%FT_TV%361", "'15'", "-cost%FT_TV%362", "'15'",
               "-cost%FT_T1%337", "'15'", "-cost%FT_T1%338", "'15'",
               "-cost%FT_T2%140", "'15'", "-cost%FT_TV%360", "'15'",
               "-cost%FT_T1%339", "'15'", "-cost%FT_T2%132", "'15'",
               "-cost%FT_T2%133", "'15'", "-cost%FT_T2%130", "'15'",
               "-cost%FT_T2%131", "'15'", "-cost%FT_T2%136", "'15'",
               "-cost%FT_T2%137", "'15'", "-cost%FT_T2%134", "'15'",
               "-cost%FT_T2%135", "'15'", "-cost%FT_T2%138", "'15'",
               "-cost%FT_T2%139", "'15'", "-cost%FT_T1%344", "'15'",
               "-cost%FT_T1%345", "'15'", "-cost%FT_T1%346", "'15'",
               "-cost%FT_T1%347", "'15'", "-cost%FT_T1%340", "'15'",
               "-cost%FT_TV%378", "'15'", "-cost%FT_T1%341", "'15'",
               "-cost%FT_TV%379", "'15'", "-cost%FT_T1%342", "'15'",
               "-cost%FT_TV%376", "'15'", "-cost%FT_T1%343", "'15'",
               "-cost%FT_TV%377", "'15'", "-cost%FT_TV%374", "'15'",
               "-cost%FT_TV%375", "'15'", "-cost%FT_TV%372", "'15'",
               "-cost%FT_TV%373", "'15'", "-cost%FT_T1%348", "'15'",
               "-cost%FT_T2%150", "'15'", "-cost%FT_TV%370", "'15'",
               "-cost%FT_T1%349", "'15'", "-cost%FT_T2%151", "'15'",
               "-cost%FT_TV%371", "'15'", "-cost%FT_T2%143", "'15'",
               "-cost%FT_T2%144", "'15'", "-cost%FT_T2%141", "'15'",
               "-cost%FT_T2%142", "'15'", "-cost%FT_T2%147", "'15'",
               "-cost%FT_T2%148", "'15'", "-cost%FT_T2%145", "'15'",
               "-cost%FT_T2%146", "'15'", "-cost%FT_T2%149", "'15'",
               "-cost%FT_T1%350", "'15'", "-cost%FT_T1%098", "'15'",
               "-cost%FT_T1%099", "'15'", "-capacity%FT%030", "'15'",
               "-cost%FT_T1%076", "'15'", "-cost%FT_T1%077", "'15'",
               "-cost%FT_T1%078", "'15'", "-cost%FT_T1%079", "'15'",
               "-capacity%FT%020", "'15'", "-capacity%FT%021", "'15'",
               "-capacity%FT%024", "'15'", "-capacity%FT%025", "'15'",
               "-capacity%FT%022", "'15'", "-capacity%FT%023", "'15'",
               "-capacity%FT%028", "'15'", "-cost%FT_T1%083", "'15'",
               "-capacity%FT%029", "'15'", "-cost%FT_T1%084", "'15'",
               "-capacity%FT%026", "'15'", "-cost%FT_T1%085", "'15'",
               "-capacity%FT%027", "'15'", "-cost%FT_T1%086", "'15'",
               "-cost%FT_T1%080", "'15'", "-cost%FT_T1%081", "'15'",
               "-cost%FT_T1%082", "'15'", "-cost%FT_T1%087", "'15'",
               "-cost%FT_T1%088", "'15'", "-cost%FT_T1%089", "'15'",
               "-capacity%FT%010", "'15'", "-capacity%FT%013", "'15'",
               "-capacity%FT%014", "'15'", "-capacity%FT%011", "'15'",
               "-capacity%FT%012", "'15'", "-capacity%FT%017", "'15'",
               "-cost%FT_T1%094", "'15'", "-capacity%FT%018", "'15'",
               "-cost%FT_T1%095", "'15'", "-capacity%FT%015", "'15'",
               "-cost%FT_T1%096", "'15'", "-capacity%FT%016", "'15'",
               "-cost%FT_T1%097", "'15'", "-cost%FT_T1%090", "'15'",
               "-cost%FT_T1%091", "'15'", "-capacity%FT%019", "'15'",
               "-cost%FT_T1%092", "'15'", "-cost%FT_T1%093", "'15'",
               "-cost%FT_T1%058", "'15'", "-cost%FT_T1%059", "'15'",
               "-cost%FT_T1%054", "'15'", "-cost%FT_T1%055", "'15'",
               "-cost%FT_T1%056", "'15'", "-cost%FT_T1%057", "'15'",
               "-capacity%FT%002", "'15'", "-capacity%FT%003", "'15'",
               "-capacity%FT%001", "'15'", "-capacity%FT%006", "'15'",
               "-cost%FT_T1%061", "'15'", "-capacity%FT%007", "'15'",
               "-cost%FT_T1%062", "'15'", "-capacity%FT%004", "'15'",
               "-cost%FT_T1%063", "'15'", "-capacity%FT%005", "'15'",
               "-cost%FT_T1%064", "'15'", "-capacity%FT%008", "'15'",
               "-capacity%FT%009", "'15'", "-cost%FT_T1%060", "'15'",
               "-cost%FT_T1%069", "'15'", "-cost%FT_T1%065", "'15'",
               "-cost%FT_T1%066", "'15'", "-cost%FT_T1%067", "'15'",
               "-cost%FT_T1%068", "'15'", "-cost%FT_T1%072", "'15'",
               "-cost%FT_T1%073", "'15'", "-cost%FT_T1%074", "'15'",
               "-cost%FT_T1%075", "'15'", "-cost%FT_T1%070", "'15'",
               "-cost%FT_T1%071", "'15'", "-cost%FT_TV%624", "'15'",
               "-cost%FT_TV%625", "'15'", "-cost%FT_TV%622", "'15'",
               "-cost%FT_TV%623", "'15'", "-cost%FT_TV%620", "'15'",
               "-cost%FT_TV%621", "'15'", "-cost%FT_T2%752", "'15'",
               "-cost%FT_T2%753", "'15'", "-cost%FT_T2%750", "'15'",
               "-cost%FT_T2%751", "'15'", "-cost%FT_T2%756", "'15'",
               "-cost%FT_T2%757", "'15'", "-cost%FT_T2%754", "'15'",
               "-cost%FT_T2%755", "'15'", "-cost%FT_TV%628", "'15'",
               "-cost%FT_TV%629", "'15'", "-cost%FT_T2%758", "'15'",
               "-cost%FT_TV%626", "'15'", "-cost%FT_T2%759", "'15'",
               "-cost%FT_TV%627", "'15'", "-cost%FT_TV%635", "'15'",
               "-cost%FT_TV%636", "'15'", "-cost%FT_TV%633", "'15'",
               "-cost%FT_TV%634", "'15'", "-cost%FT_TV%631", "'15'",
               "-cost%FT_TV%632", "'15'", "-cost%FT_TV%630", "'15'",
               "-cost%FT_T2%760", "'15'", "-cost%FT_T2%763", "'15'",
               "-cost%FT_T2%764", "'15'", "-cost%FT_T2%761", "'15'",
               "-cost%FT_T2%762", "'15'", "-cost%FT_T2%767", "'15'",
               "-cost%FT_T2%768", "'15'", "-cost%FT_T2%765", "'15'",
               "-cost%FT_T2%766", "'15'", "-cost%FT_TV%639", "'15'",
               "-cost%FT_T2%769", "'15'", "-cost%FT_TV%637", "'15'",
               "-cost%FT_TV%638", "'15'", "-cost%FT_TV%602", "'15'",
               "-cost%FT_TV%603", "'15'", "-cost%FT_TV%600", "'15'",
               "-cost%FT_TV%601", "'15'", "-cost%FT_T2%730", "'15'",
               "-cost%FT_T2%731", "'15'", "-cost%FT_T2%734", "'15'",
               "-cost%FT_T2%735", "'15'", "-cost%FT_T2%732", "'15'",
               "-cost%FT_TV%608", "'15'", "-cost%FT_T2%733", "'15'",
               "-cost%FT_TV%609", "'15'", "-cost%FT_T2%738", "'15'",
               "-cost%FT_TV%606", "'15'", "-cost%FT_T2%739", "'15'",
               "-cost%FT_TV%607", "'15'", "-cost%FT_T2%736", "'15'",
               "-cost%FT_TV%604", "'15'", "-cost%FT_T2%737", "'15'",
               "-cost%FT_TV%605", "'15'", "-cost%FT_TV%613", "'15'",
               "-cost%FT_TV%614", "'15'", "-cost%FT_TV%611", "'15'",
               "-cost%FT_TV%612", "'15'", "-cost%FT_TV%610", "'15'",
               "-cost%FT_T2%741", "'15'", "-cost%FT_T2%742", "'15'",
               "-cost%FT_T2%740", "'15'", "-cost%FT_T2%745", "'15'",
               "-cost%FT_T2%746", "'15'", "-cost%FT_T2%743", "'15'",
               "-cost%FT_TV%619", "'15'", "-cost%FT_T2%744", "'15'",
               "-cost%FT_T2%749", "'15'", "-cost%FT_TV%617", "'15'",
               "-cost%FT_TV%618", "'15'", "-cost%FT_T2%747", "'15'",
               "-cost%FT_TV%615", "'15'", "-cost%FT_T2%748", "'15'",
               "-cost%FT_TV%616", "'15'", "-cost%FT_T2%718", "'15'",
               "-cost%FT_T2%719", "'15'", "-cost%FT_T2%712", "'15'",
               "-cost%FT_T2%713", "'15'", "-cost%FT_T2%710", "'15'",
               "-cost%FT_T2%711", "'15'", "-cost%FT_T2%716", "'15'",
               "-cost%FT_T2%717", "'15'", "-cost%FT_T2%714", "'15'",
               "-cost%FT_T2%715", "'15'", "-cost%FT_T2%729", "'15'",
               "-cost%FT_T2%720", "'15'", "-cost%FT_T2%723", "'15'",
               "-cost%FT_T2%724", "'15'", "-cost%FT_T2%721", "'15'",
               "-cost%FT_T2%722", "'15'", "-cost%FT_T2%727", "'15'",
               "-cost%FT_T2%728", "'15'", "-cost%FT_T2%725", "'15'",
               "-cost%FT_T2%726", "'15'", "-cost%FT_T2%709", "'15'",
               "-cost%FT_T2%707", "'15'", "-cost%FT_T2%708", "'15'",
               "-cost%FT_T1%900", "'15'", "-cost%FT_T2%701", "'15'",
               "-cost%FT_T2%702", "'15'", "-cost%FT_T2%700", "'15'",
               "-cost%FT_T2%705", "'15'", "-cost%FT_T2%706", "'15'",
               "-cost%FT_T2%703", "'15'", "-cost%FT_T2%704", "'15'",
               "-cost%FT_T1%432", "'15'", "-cost%FT_TV%228", "'15'",
               "-cost%FT_T1%433", "'15'", "-cost%FT_TV%229", "'15'",
               "-cost%FT_T1%434", "'15'", "-cost%FT_TV%226", "'15'",
               "-cost%FT_T1%435", "'15'", "-cost%FT_TV%227", "'15'",
               "-cost%FT_TV%224", "'15'", "-cost%FT_TV%225", "'15'",
               "-cost%FT_T1%430", "'15'", "-cost%FT_TV%222", "'15'",
               "-cost%FT_T1%431", "'15'", "-cost%FT_TV%223", "'15'",
               "-cost%FT_TV%220", "'15'", "-cost%FT_TV%221", "'15'",
               "-cost%FT_T1%436", "'15'", "-cost%FT_T1%437", "'15'",
               "-cost%FT_T1%438", "'15'", "-cost%FT_T1%439", "'15'",
               "-cost%FT_T2%231", "'15'", "-cost%FT_T2%232", "'15'",
               "-cost%FT_T2%230", "'15'", "-cost%FT_T2%235", "'15'",
               "-cost%FT_T2%236", "'15'", "-cost%FT_T2%233", "'15'",
               "-cost%FT_T2%234", "'15'", "-cost%FT_T2%239", "'15'",
               "-cost%FT_T2%237", "'15'", "-cost%FT_T2%238", "'15'",
               "-cost%FT_T1%443", "'15'", "-cost%FT_TV%239", "'15'",
               "-cost%FT_T1%444", "'15'", "-cost%FT_T1%445", "'15'",
               "-cost%FT_TV%237", "'15'", "-cost%FT_T1%446", "'15'",
               "-cost%FT_TV%238", "'15'", "-cost%FT_TV%235", "'15'",
               "-cost%FT_T1%440", "'15'", "-cost%FT_TV%236", "'15'",
               "-cost%FT_T1%441", "'15'", "-cost%FT_TV%233", "'15'",
               "-cost%FT_T1%442", "'15'", "-cost%FT_TV%234", "'15'",
               "-cost%FT_TV%231", "'15'", "-cost%FT_TV%232", "'15'",
               "-cost%FT_TV%230", "'15'", "-cost%FT_T1%447", "'15'",
               "-cost%FT_T1%448", "'15'", "-cost%FT_T2%250", "'15'",
               "-cost%FT_T1%449", "'15'", "-cost%FT_T2%242", "'15'",
               "-cost%FT_T2%243", "'15'", "-cost%FT_T2%240", "'15'",
               "-cost%FT_T2%241", "'15'", "-cost%FT_T2%246", "'15'",
               "-cost%FT_T2%247", "'15'", "-cost%FT_T2%244", "'15'",
               "-cost%FT_T2%245", "'15'", "-cost%FT_T2%248", "'15'",
               "-cost%FT_T2%249", "'15'", "-cost%FT_T1%410", "'15'",
               "-cost%FT_T1%894", "'15'", "-cost%FT_TV%206", "'15'",
               "-cost%FT_T1%411", "'15'", "-cost%FT_T1%895", "'15'",
               "-cost%FT_TV%207", "'15'", "-cost%FT_T1%412", "'15'",
               "-cost%FT_T1%896", "'15'", "-cost%FT_TV%204", "'15'",
               "-cost%FT_TV%688", "'15'", "-cost%FT_T1%413", "'15'",
               "-cost%FT_T1%897", "'15'", "-cost%FT_TV%205", "'15'",
               "-cost%FT_TV%689", "'15'", "-cost%FT_T1%890", "'15'",
               "-cost%FT_TV%202", "'15'", "-cost%FT_TV%686", "'15'",
               "-cost%FT_T1%891", "'15'", "-cost%FT_TV%203", "'15'",
               "-cost%FT_TV%687", "'15'", "-cost%FT_T1%892", "'15'",
               "-cost%FT_TV%200", "'15'", "-cost%FT_TV%684", "'15'",
               "-cost%FT_T1%893", "'15'", "-cost%FT_TV%201", "'15'",
               "-cost%FT_TV%685", "'15'", "-cost%FT_T1%418", "'15'",
               "-cost%FT_TV%682", "'15'", "-cost%FT_T1%419", "'15'",
               "-cost%FT_TV%683", "'15'", "-cost%FT_TV%680", "'15'",
               "-cost%FT_TV%681", "'15'", "-cost%FT_T1%414", "'15'",
               "-cost%FT_T1%898", "'15'", "-cost%FT_T1%415", "'15'",
               "-cost%FT_T1%899", "'15'", "-cost%FT_T1%416", "'15'",
               "-cost%FT_T1%417", "'15'", "-cost%FT_T2%693", "'15'",
               "-cost%FT_T2%210", "'15'", "-cost%FT_T2%694", "'15'",
               "-cost%FT_T2%691", "'15'", "-cost%FT_T2%692", "'15'",
               "-cost%FT_T2%213", "'15'", "-cost%FT_T2%697", "'15'",
               "-cost%FT_T2%214", "'15'", "-cost%FT_T2%698", "'15'",
               "-cost%FT_T2%211", "'15'", "-cost%FT_T2%695", "'15'",
               "-cost%FT_T2%212", "'15'", "-cost%FT_T2%696", "'15'",
               "-cost%FT_T2%217", "'15'", "-cost%FT_T2%218", "'15'",
               "-cost%FT_T2%215", "'15'", "-cost%FT_T2%699", "'15'",
               "-cost%FT_T2%216", "'15'", "-cost%FT_T2%219", "'15'",
               "-cost%FT_TV%208", "'15'", "-cost%FT_TV%209", "'15'",
               "-cost%FT_T1%421", "'15'", "-cost%FT_TV%217", "'15'",
               "-cost%FT_T1%422", "'15'", "-cost%FT_TV%218", "'15'",
               "-cost%FT_T1%423", "'15'", "-cost%FT_TV%215", "'15'",
               "-cost%FT_TV%699", "'15'", "-cost%FT_T1%424", "'15'",
               "-cost%FT_TV%216", "'15'", "-cost%FT_TV%213", "'15'",
               "-cost%FT_TV%697", "'15'", "-cost%FT_TV%214", "'15'",
               "-cost%FT_TV%698", "'15'", "-cost%FT_TV%211", "'15'",
               "-cost%FT_TV%695", "'15'", "-cost%FT_T1%420", "'15'",
               "-cost%FT_TV%212", "'15'", "-cost%FT_TV%696", "'15'",
               "-cost%FT_T1%429", "'15'", "-cost%FT_TV%693", "'15'",
               "-cost%FT_TV%210", "'15'", "-cost%FT_TV%694", "'15'",
               "-cost%FT_TV%691", "'15'", "-cost%FT_TV%692", "'15'",
               "-cost%FT_T1%425", "'15'", "-cost%FT_T1%426", "'15'",
               "-cost%FT_TV%690", "'15'", "-cost%FT_T1%427", "'15'",
               "-cost%FT_T1%428", "'15'", "-cost%FT_T2%220", "'15'",
               "-cost%FT_T2%221", "'15'", "-cost%FT_T2%224", "'15'",
               "-cost%FT_T2%225", "'15'", "-cost%FT_T2%222", "'15'",
               "-cost%FT_T2%223", "'15'", "-cost%FT_T2%228", "'15'",
               "-cost%FT_T2%229", "'15'", "-cost%FT_T2%226", "'15'",
               "-cost%FT_T2%227", "'15'", "-cost%FT_TV%219", "'15'",
               "-cost%FT_T1%872", "'15'", "-cost%FT_TV%668", "'15'",
               "-cost%FT_T1%873", "'15'", "-cost%FT_TV%669", "'15'",
               "-cost%FT_T1%874", "'15'", "-cost%FT_TV%666", "'15'",
               "-cost%FT_T1%875", "'15'", "-cost%FT_TV%667", "'15'",
               "-cost%FT_TV%664", "'15'", "-cost%FT_TV%665", "'15'",
               "-cost%FT_T1%870", "'15'", "-cost%FT_TV%662", "'15'",
               "-cost%FT_T1%871", "'15'", "-cost%FT_TV%663", "'15'",
               "-cost%FT_TV%660", "'15'", "-cost%FT_TV%661", "'15'",
               "-cost%FT_T1%876", "'15'", "-cost%FT_T1%877", "'15'",
               "-cost%FT_T1%878", "'15'", "-cost%FT_T1%879", "'15'",
               "-cost%FT_T2%671", "'15'", "-cost%FT_T2%672", "'15'",
               "-cost%FT_T2%670", "'15'", "-cost%FT_T2%675", "'15'",
               "-cost%FT_T2%676", "'15'", "-cost%FT_T2%673", "'15'",
               "-cost%FT_T2%674", "'15'", "-cost%FT_T2%679", "'15'",
               "-cost%FT_T2%677", "'15'", "-cost%FT_T2%678", "'15'",
               "-cost%FT_T1%883", "'15'", "-cost%FT_TV%679", "'15'",
               "-cost%FT_T1%400", "'15'", "-cost%FT_T1%884", "'15'",
               "-cost%FT_T1%401", "'15'", "-cost%FT_T1%885", "'15'",
               "-cost%FT_TV%677", "'15'", "-cost%FT_T1%402", "'15'",
               "-cost%FT_T1%886", "'15'", "-cost%FT_TV%678", "'15'",
               "-cost%FT_TV%675", "'15'", "-cost%FT_T1%880", "'15'",
               "-cost%FT_TV%676", "'15'", "-cost%FT_T1%881", "'15'",
               "-cost%FT_TV%673", "'15'", "-cost%FT_T1%882", "'15'",
               "-cost%FT_TV%674", "'15'", "-cost%FT_T1%407", "'15'",
               "-cost%FT_TV%671", "'15'", "-cost%FT_T1%408", "'15'",
               "-cost%FT_TV%672", "'15'", "-cost%FT_T1%409", "'15'",
               "-cost%FT_TV%670", "'15'", "-cost%FT_T1%403", "'15'",
               "-cost%FT_T1%887", "'15'", "-cost%FT_T1%404", "'15'",
               "-cost%FT_T1%888", "'15'", "-cost%FT_T2%690", "'15'",
               "-cost%FT_T1%405", "'15'", "-cost%FT_T1%889", "'15'",
               "-cost%FT_T1%406", "'15'", "-cost%FT_T2%682", "'15'",
               "-cost%FT_T2%683", "'15'", "-cost%FT_T2%680", "'15'",
               "-cost%FT_T2%681", "'15'", "-cost%FT_T2%202", "'15'",
               "-cost%FT_T2%686", "'15'", "-cost%FT_T2%203", "'15'",
               "-cost%FT_T2%687", "'15'", "-cost%FT_T2%200", "'15'",
               "-cost%FT_T2%684", "'15'", "-cost%FT_T2%201", "'15'",
               "-cost%FT_T2%685", "'15'", "-cost%FT_T2%206", "'15'",
               "-cost%FT_T2%207", "'15'", "-cost%FT_T2%204", "'15'",
               "-cost%FT_T2%688", "'15'", "-cost%FT_T2%205", "'15'",
               "-cost%FT_T2%689", "'15'", "-cost%FT_T2%208", "'15'",
               "-cost%FT_T2%209", "'15'", "-cost%FT_T1%850", "'15'",
               "-cost%FT_TV%646", "'15'", "-cost%FT_T1%851", "'15'",
               "-cost%FT_TV%647", "'15'", "-cost%FT_T1%852", "'15'",
               "-cost%FT_TV%644", "'15'", "-cost%FT_T1%853", "'15'",
               "-cost%FT_TV%645", "'15'", "-cost%FT_TV%642", "'15'",
               "-cost%FT_TV%643", "'15'", "-cost%FT_TV%640", "'15'",
               "-cost%FT_TV%641", "'15'", "-cost%FT_T1%858", "'15'",
               "-cost%FT_T1%859", "'15'", "-cost%FT_T1%854", "'15'",
               "-cost%FT_T1%855", "'15'", "-cost%FT_T1%856", "'15'",
               "-cost%FT_T1%857", "'15'", "-cost%FT_T2%650", "'15'",
               "-cost%FT_T2%653", "'15'", "-cost%FT_T2%654", "'15'",
               "-cost%FT_T2%651", "'15'", "-cost%FT_T2%652", "'15'",
               "-cost%FT_T2%657", "'15'", "-cost%FT_T2%658", "'15'",
               "-cost%FT_T2%655", "'15'", "-cost%FT_T2%656", "'15'",
               "-cost%FT_T2%659", "'15'", "-cost%FT_TV%648", "'15'",
               "-cost%FT_TV%649", "'15'", "-cost%FT_T1%861", "'15'",
               "-cost%FT_TV%657", "'15'", "-cost%FT_T1%862", "'15'",
               "-cost%FT_TV%658", "'15'", "-cost%FT_T1%863", "'15'",
               "-cost%FT_TV%655", "'15'", "-cost%FT_T1%864", "'15'",
               "-cost%FT_TV%656", "'15'", "-cost%FT_TV%653", "'15'",
               "-cost%FT_TV%654", "'15'", "-cost%FT_TV%651", "'15'",
               "-cost%FT_T1%860", "'15'", "-cost%FT_TV%652", "'15'",
               "-cost%FT_T1%869", "'15'", "-cost%FT_TV%650", "'15'",
               "-cost%FT_T1%865", "'15'", "-cost%FT_T1%866", "'15'",
               "-cost%FT_T1%867", "'15'", "-cost%FT_T1%868", "'15'",
               "-cost%FT_T2%660", "'15'", "-cost%FT_T2%661", "'15'",
               "-cost%FT_T2%664", "'15'", "-cost%FT_T2%665", "'15'",
               "-cost%FT_T2%662", "'15'", "-cost%FT_T2%663", "'15'",
               "-cost%FT_T2%668", "'15'", "-cost%FT_T2%669", "'15'",
               "-cost%FT_T2%666", "'15'", "-cost%FT_T2%667", "'15'",
               "-cost%FT_TV%659", "'15'", "-cost%FT_T1%036", "'15'",
               "-cost%FT_T1%037", "'15'", "-cost%FT_T1%038", "'15'",
               "-cost%FT_T1%039", "'15'", "-cost%FT_T1%032", "'15'",
               "-cost%FT_T1%033", "'15'", "-cost%FT_T1%034", "'15'",
               "-cost%FT_T1%035", "'15'", "-cost%FT_T1%040", "'15'",
               "-cost%FT_T1%041", "'15'", "-cost%FT_T1%042", "'15'",
               "-cost%FT_T1%047", "'15'", "-cost%FT_T1%048", "'15'",
               "-cost%FT_T1%049", "'15'", "-cost%FT_T1%043", "'15'",
               "-cost%FT_T1%044", "'15'", "-cost%FT_T1%045", "'15'",
               "-cost%FT_T1%046", "'15'", "-cost%FT_T1%050", "'15'",
               "-cost%FT_T1%051", "'15'", "-cost%FT_T1%052", "'15'",
               "-cost%FT_T1%053", "'15'", "-cost%FT_T1%014", "'15'",
               "-cost%FT_T1%498", "'15'", "-cost%FT_T1%015", "'15'",
               "-cost%FT_T1%499", "'15'", "-cost%FT_T1%016", "'15'",
               "-cost%FT_T1%017", "'15'", "-cost%FT_T1%010", "'15'",
               "-cost%FT_T1%494", "'15'", "-cost%FT_T1%011", "'15'",
               "-cost%FT_T1%495", "'15'", "-cost%FT_T1%012", "'15'",
               "-cost%FT_T1%496", "'15'", "-cost%FT_TV%288", "'15'",
               "-cost%FT_T1%013", "'15'", "-cost%FT_T1%497", "'15'",
               "-cost%FT_TV%289", "'15'", "-cost%FT_TV%286", "'15'",
               "-cost%FT_TV%287", "'15'", "-cost%FT_TV%284", "'15'",
               "-cost%FT_TV%285", "'15'", "-cost%FT_T1%018", "'15'",
               "-cost%FT_TV%282", "'15'", "-cost%FT_T1%019", "'15'",
               "-cost%FT_TV%283", "'15'", "-cost%FT_TV%280", "'15'",
               "-cost%FT_TV%281", "'15'", "-cost%FT_T2%297", "'15'",
               "-cost%FT_T2%298", "'15'", "-cost%FT_TV%290", "'15'",
               "-cost%FT_T2%295", "'15'", "-cost%FT_T2%296", "'15'",
               "-cost%FT_T2%299", "'15'", "-cost%FT_T1%020", "'15'",
               "-cost%FT_T1%025", "'15'", "-cost%FT_T1%026", "'15'",
               "-cost%FT_T1%027", "'15'", "-cost%FT_T1%028", "'15'",
               "-cost%FT_T1%021", "'15'", "-cost%FT_T1%022", "'15'",
               "-cost%FT_T1%023", "'15'", "-cost%FT_TV%299", "'15'",
               "-cost%FT_T1%024", "'15'", "-cost%FT_TV%297", "'15'",
               "-cost%FT_TV%298", "'15'", "-cost%FT_TV%295", "'15'",
               "-cost%FT_TV%296", "'15'", "-cost%FT_T1%029", "'15'",
               "-cost%FT_TV%293", "'15'", "-cost%FT_TV%294", "'15'",
               "-cost%FT_TV%291", "'15'", "-cost%FT_TV%292", "'15'",
               "-cost%FT_T1%030", "'15'", "-cost%FT_T1%031", "'15'",
               "-cost%FT_T1%476", "'15'", "-cost%FT_T1%477", "'15'",
               "-cost%FT_T1%478", "'15'", "-cost%FT_T1%479", "'15'",
               "-cost%FT_T1%472", "'15'", "-cost%FT_TV%268", "'15'",
               "-cost%FT_T1%473", "'15'", "-cost%FT_TV%269", "'15'",
               "-cost%FT_T1%474", "'15'", "-cost%FT_TV%266", "'15'",
               "-cost%FT_T1%475", "'15'", "-cost%FT_TV%267", "'15'",
               "-cost%FT_TV%264", "'15'", "-cost%FT_TV%265", "'15'",
               "-cost%FT_TV%262", "'15'", "-cost%FT_TV%263", "'15'",
               "-cost%FT_T2%282", "'15'", "-cost%FT_TV%260", "'15'",
               "-cost%FT_T2%283", "'15'", "-cost%FT_TV%261", "'15'",
               "-cost%FT_T2%280", "'15'", "-cost%FT_T2%281", "'15'",
               "-cost%FT_T2%275", "'15'", "-cost%FT_T2%276", "'15'",
               "-cost%FT_T2%273", "'15'", "-cost%FT_T2%274", "'15'",
               "-cost%FT_T2%279", "'15'", "-cost%FT_T2%277", "'15'",
               "-cost%FT_T2%278", "'15'", "-cost%FT_T1%480", "'15'",
               "-cost%FT_T1%481", "'15'", "-cost%FT_T1%482", "'15'",
               "-cost%FT_T1%003", "'15'", "-cost%FT_T1%487", "'15'",
               "-cost%FT_T1%004", "'15'", "-cost%FT_T1%488", "'15'",
               "-cost%FT_T1%005", "'15'", "-cost%FT_T1%489", "'15'",
               "-cost%FT_T1%006", "'15'", "-cost%FT_T1%483", "'15'",
               "-cost%FT_TV%279", "'15'", "-cost%FT_T1%484", "'15'",
               "-cost%FT_T1%001", "'15'", "-cost%FT_T1%485", "'15'",
               "-cost%FT_TV%277", "'15'", "-cost%FT_T1%002", "'15'",
               "-cost%FT_T1%486", "'15'", "-cost%FT_TV%278", "'15'",
               "-cost%FT_TV%275", "'15'", "-cost%FT_T2%290", "'15'",
               "-cost%FT_TV%276", "'15'", "-cost%FT_TV%273", "'15'",
               "-cost%FT_TV%274", "'15'", "-cost%FT_T1%007", "'15'",
               "-cost%FT_T2%293", "'15'", "-cost%FT_TV%271", "'15'",
               "-cost%FT_T1%008", "'15'", "-cost%FT_T2%294", "'15'",
               "-cost%FT_TV%272", "'15'", "-cost%FT_T1%009", "'15'",
               "-cost%FT_T2%291", "'15'", "-cost%FT_T2%292", "'15'",
               "-cost%FT_TV%270", "'15'", "-cost%FT_T2%286", "'15'",
               "-cost%FT_T2%287", "'15'", "-cost%FT_T2%284", "'15'",
               "-cost%FT_T2%285", "'15'", "-cost%FT_T2%288", "'15'",
               "-cost%FT_T2%289", "'15'", "-cost%FT_T1%490", "'15'",
               "-cost%FT_T1%491", "'15'", "-cost%FT_T1%492", "'15'",
               "-cost%FT_T1%493", "'15'", "-cost%FT_T1%454", "'15'",
               "-cost%FT_T1%455", "'15'", "-cost%FT_T1%456", "'15'",
               "-cost%FT_TV%248", "'15'", "-cost%FT_T1%457", "'15'",
               "-cost%FT_TV%249", "'15'", "-cost%FT_T1%450", "'15'",
               "-cost%FT_TV%246", "'15'", "-cost%FT_T1%451", "'15'",
               "-cost%FT_TV%247", "'15'", "-cost%FT_T1%452", "'15'",
               "-cost%FT_TV%244", "'15'", "-cost%FT_T1%453", "'15'",
               "-cost%FT_TV%245", "'15'", "-cost%FT_TV%242", "'15'",
               "-cost%FT_TV%243", "'15'", "-cost%FT_TV%240", "'15'",
               "-cost%FT_TV%241", "'15'", "-cost%FT_T1%458", "'15'",
               "-cost%FT_T2%260", "'15'", "-cost%FT_T1%459", "'15'",
               "-cost%FT_T2%261", "'15'", "-cost%FT_T2%253", "'15'",
               "-cost%FT_T2%254", "'15'", "-cost%FT_T2%251", "'15'",
               "-cost%FT_T2%252", "'15'", "-cost%FT_T2%257", "'15'",
               "-cost%FT_T2%258", "'15'", "-cost%FT_T2%255", "'15'",
               "-cost%FT_T2%256", "'15'", "-cost%FT_T2%259", "'15'",
               "-cost%FT_T1%460", "'15'", "-cost%FT_T1%465", "'15'",
               "-cost%FT_T1%466", "'15'", "-cost%FT_T1%467", "'15'",
               "-cost%FT_TV%259", "'15'", "-cost%FT_T1%468", "'15'",
               "-cost%FT_T1%461", "'15'", "-cost%FT_TV%257", "'15'",
               "-cost%FT_T1%462", "'15'", "-cost%FT_TV%258", "'15'",
               "-cost%FT_T1%463", "'15'", "-cost%FT_TV%255", "'15'",
               "-cost%FT_T1%464", "'15'", "-cost%FT_TV%256", "'15'",
               "-cost%FT_TV%253", "'15'", "-cost%FT_TV%254", "'15'",
               "-cost%FT_TV%251", "'15'", "-cost%FT_TV%252", "'15'",
               "-cost%FT_T1%469", "'15'", "-cost%FT_T2%271", "'15'",
               "-cost%FT_T2%272", "'15'", "-cost%FT_TV%250", "'15'",
               "-cost%FT_T2%270", "'15'", "-cost%FT_T2%264", "'15'",
               "-cost%FT_T2%265", "'15'", "-cost%FT_T2%262", "'15'",
               "-cost%FT_T2%263", "'15'", "-cost%FT_T2%268", "'15'",
               "-cost%FT_T2%269", "'15'", "-cost%FT_T2%266", "'15'",
               "-cost%FT_T2%267", "'15'", "-cost%FT_T1%470", "'15'",
               "-cost%FT_T1%471", "'15'"],
            limit_time = Nothing, log_level = LogDebug}
