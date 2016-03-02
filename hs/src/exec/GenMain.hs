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
import Gen.Instance.Point           (readPoint)
import Gen.Instance.Results.Results (showResults)
import Gen.Instance.UI              (instances_no_racing, makeProvider, runMethod)
import Gen.Instance.Undirected      (Undirected (..))
import Gen.IO.Dups                  (deleteDups2, refineDups, solveDups)
import Gen.IO.FindCompact           (findCompact)
import Gen.IO.Formats               (readFromJSON)
import Gen.IO.RunResult             (giveDb, writeDb_)
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
                                     takeExtension, takeExtensions, takeDirectory)
import System.IO                    (hPutStrLn, stderr)
import System.Timeout               (timeout)
import Text.Printf                  (printf)
import GHC.Conc
import Gen.Helpers.MonadNote

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
    [x] | x `elem` [ "essence", "reduce", "link", "meta", "json", "generalise", "search"
                   , "weights" , "script-toolchain", "script-removeDups"
                   , "instance-nsample", "instance-undirected", "instance-summary"
                   , "db", "script-updateChoices", "script-smac-process"
                   , "instance-allsols", "instance-noRacing"] -> do
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

  maxCores <- getNumProcessors

  when (cores > maxCores) $
    docError [nn "CORES used" cores, nn "max cores " maxCores
             , "the cores used must be the number of processors" ]

  setNumCapabilities cores
  putStrLn $ "CORES max : " ++ (show maxCores)
  putStrLn $ "CORES used: " ++ (show cores)


  ws <- case _weightings of
          Nothing -> def
          Just fp -> do
            readFromJSON fp

  notUseful <- case delete_immediately of
          Nothing -> return $ S.fromList $  [(KindAny_, NumberToLarge_), (KindAny_,Timeout_), (KindAny_, Success_), (KindAny_, ConjureUserError_)]
          Just fp -> do
            vals <- readFromJSON fp
            when ( (KindAny_,StatusAny_) `S.member` vals ) $
                 error "Error (--delete_immediately/-X): Specifying (KindAny_,StatusAny_) would delete every generated specification."
            return vals

  putStrLn "(Kind,Status) tuples to discard immediately"
  putStrLn $ renderSized 79 $ vcat $ map pretty (S.toList notUseful)
  putStrLn ""

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
  instances_no_racing essence_path iterations param_gen_time out
                      custom_param_essence seed_ log_level


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
  (p,_) <- ignoreLogs $ makeProvider essence_path i

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
                False -> fileExists "spec.spec.json is required unless --from-essence is specified" (spec_directory </> "spec.spec.json")
            ]


  let errors = catMaybes
        [ aerr "spec-directory" (null spec_directory)
        , aerr "-p|--per-spec-time >0" (per_spec_time == 0)
        , aerr "-t|--total-time > 0 if specified" (total_time_may == Just 0)
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
                 ,R.cores_             = cores
                 ,R.binariesDirectory_ = binaries_directory
                 ,R.toolchainOutput_   = toolchain_ouput
                 ,R.deletePassing_     = not keep_passing
                 ,alwaysCompact_       = always_compact
                 }
                ,resultsDB_           = db
                ,mostReducedChoices_  = error_choices
                ,timeLeft_            = total_time_may
                }

  doMeta out no_csv binaries_directory

  (state,logs) <- runNoteT $ runNotePipeIO log_level $ runRndGen seed_ $ reduceMain (not no_check) args
  let logsPath = out  </> "_note" <.> ".logs"
  logsToFile logsPath logs

  writeDb_ db_only_passing db_directory (resultsDB_  state)
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

  (state,logs) <- runNoteT $ runNotePipeIO log_level $ runRndGen seed_ $ generaliseMain args
  let logsPath = out  </> "_note" <.> ".logs"
  logsToFile logsPath logs
  writeDb_ False db_directory (E.resultsDB_  state)



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
              dirExists    "First argument"        directory
            ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  let out  = fromMaybe ("db") output_directory

  createDirectoryIfMissing True out
  createDbHashesMain no_add
    delete_passing delete_errors delete_skipped errors_to_skipped  directory out


mainWithArgs Script_RemoveDups{..} = do

  errors <- catMaybes <$> (mapM (dirExists "-d") dups_)

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  dirs <- map takeDirectory <$> concatMapM (getAllFilesWithSuffix ".essence")  dups_
  putStrLn $ "dirs:"
  putStrLn $ groom dirs

  dups <- case dups_kind of
    DupRefine -> refineDups dirs
    DupSolve  -> solveDups  dirs
  putStrLn "Result"
  putStrLn $ show $ map pretty  dups
  deleteDups2 dups

mainWithArgs u@Script_SMAC{..} = do
  -- Has to be on stderr to be seen
  liftIO $ hPutStrLn stderr . show . vcat $ ["Command line options: ", pretty (groom u)]

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
    , fileExistsMay "--custom-param-essence" custom_param_essence
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
  (p,pnames) <- ignoreLogs $ makeProvider essence_path i
  putStrLn $ "Provider: " ++ (groom p)

  mPointsGiven <- case given_dir of
    Nothing -> return $ Nothing
    Just x  -> do
      fps <- filesWithSuffix x ".param"
      if length fps < iterations then
          docError [ "Length mismatch"
                   , nn "iterations" iterations
                   , nn "given_dir"  (length fps)
                   ]
      else do
        ps <- mapM readPoint fps
        putStrLn . show . vcat $ [ nn "length given points" (length ps)
                                 , "first 3 points" <+> (vcat $ map pretty . take 3 $ ps)
                                 ]
        return $ Just ps

  let   common              = MCommon{
        mEssencePath        = essence
      , mOutputDir          = out
      , mModelTimeout       = per_model_time
      , mVarInfo            = i
      , mPreGenerate        = pre
      , mIterations         = iterations
      , mMode               = mode
      , mModelsDir          = models_path
      , mGivensProvider     = p
      , mGivenNames         = pnames
      , mPoints             = []
      , mCores              = cores
      , mCompactName        = compactFirst
      , mSubCpu             = 0
      , mPointsGiven        = mPointsGiven
      , mParamGenTime       = param_gen_time
      , mCustomParamEssence = custom_param_essence
      }

  return common


filesWithSuffix :: FilePath -> String -> IO [FilePath]
filesWithSuffix dir ext= do
    ys <- getDirectoryContents dir
    let dots i = not ( i == "." || i == ".." )
    let fps = filter dots ys
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

  let ec = Reduce{per_spec_time = 30,
       -- spec_directory = "/Users/bilalh/CS/Thesis/data/reduction_examples/parametrised_from_user",
       -- spec_directory = "/Users/bilalh/CS/Thesis/data/reduction_examples/parametrised_from_user/original",
       spec_directory = "/Users/bilalh/CS/Thesis/data/reduction_examples/given_reduce/First",
       error_kind = Savilerow_, error_status = StatusAny_,
       error_choices = Nothing, list_kinds = False, list_statuses = False,
       total_time_may = Nothing,
       output_directory = Nothing, _cores = Nothing, _seed = Nothing,
       keep_passing = False, delete_steps = False, delete_others = False,
       toolchain_ouput = ToolchainNull_, binaries_directory = Nothing,
       limit_time = Nothing, always_compact = True, no_csv = False,
       db_directory = Just "db", db_passing_in = Nothing,
       db_only_passing = False, from_essence = True, no_check = True,
       log_level = LogDebug}
  limiter (limit_time ec) (mainWithArgs ec)

_instanceDebug :: IO ()
_instanceDebug = do
  let ec = Instance_Undirected{essence_path = "/Users/bilalh/CS/essence-refinements/_current/prob028-BIBD/prob028-BIBD.essence",
                    per_model_time = 30, iterations = 1, mode = "df-every-givens-all",
                    _cores = Nothing, output_directory = Nothing, limit_time = Nothing,
                    log_level = LogDebug, _seed = Nothing, pre_solutions = Nothing
                   , given_dir=Nothing, param_gen_time = 300, custom_param_essence=Nothing}
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

_smac1 :: IO ()
_smac1 = _smacProcessDebug dir ec
  where
    dir = "/Users/bilalh/aaa/results/prob055-efpa/smac/sample-64_rndsols%1%10000/"
    ec = Script_SMAC{s_output_directory = ".", s_eprime = "empty",
                     s_instance_specific = "0", s_cutoff_time = 64.0,
                     s_cutoff_length = 2147483647, s_seed = -1,
                     s_param_arr =
                     ["-numCodeWords", "'50'", "-dist", "'50'", "-lam", "'50'",
                      "-numChars", "'50'"],  limit_time = Nothing, log_level = LogDebug}


_smac3 :: IO ()
_smac3 = _smacProcessDebug dir ec
  where
    dir = "/Users/bilalh/_rehearsal/results/prob039-rehearsal/smac/AAA_rndsols%1%10000/"
    ec = Script_SMAC{s_output_directory = ".", s_eprime = "empty",
                     s_instance_specific = "0", s_cutoff_time = 64.0,
                     s_cutoff_length = 2147483647, s_seed = -1,
                     limit_time = Nothing, log_level = LogDebug,
                     s_param_arr =
                         $never
                     }
