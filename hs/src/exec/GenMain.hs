{-# LANGUAGE LambdaCase, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs?

module Main where

import Data.Time                   (formatTime, getCurrentTime)
import Data.Time.Format            (defaultTimeLocale)
import Gen.Arbitrary.Data
import Gen.Classify.AddMeta        (metaMain)
import Gen.Classify.AddMeta        (addMeta)
import Gen.Classify.AddSpecE       (addSpecJson, specEMain)
import Gen.Classify.Sorter         (getRecursiveContents, sorterMain')
import Gen.Essence.Generate        (generateEssence)
import Gen.Generalise.Generalise   (generaliseMain)
import Gen.Helpers.StandardImports
import Gen.IO.Term
import Gen.IO.Toolchain            (KindI (..), StatusI (..), doMeta, kindsList,
                                    statusesList)
import Gen.Reduce.Data             (RState (..), mkrGen)
import Gen.Reduce.FormatResults    (formatResults)
import Gen.Reduce.Reduce           (reduceMain)
import Gen.Reduce.Runner           (giveDb, saveDB)
import Gen.UI.UI
import System.Console.CmdArgs      (cmdArgs)
import System.CPUTime              (getCPUTime)
import System.Environment          (withArgs)
import System.Exit                 (exitFailure, exitSuccess, exitWith)
import System.FilePath             (takeExtensions)
import System.Timeout              (timeout)
import Text.Printf                 (printf)

import qualified Data.Set                as S
import qualified Gen.Essence.Data        as EC
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
    [x] | x `elem` [ "essence", "reduce", "link", "meta", "json", "generalise"
                   , "script-toolchain", "script-recheck"] -> do
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


     getLimit (Essence{_mode=TypeCheck, given_dir=Just{}}) =
         error "--given and --mode typecheck can not be used together"
     getLimit x | Just i <- limit_time x, i <=0  = error "--limit-time must be > then 0"
     getLimit (Essence{_mode=TypeCheck, limit_time=Nothing, total_time=t}) = Just t
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
mainWithArgs Essence{..} = do
  let ls = case given_dir of
             Nothing  -> [ aerr "-t|--total-time" (total_time == 0)]
             Just{} -> []

  fileErr <- sequence
            [
              dirExistsMay "--givens"  given_dir
            , dirExistsMay "--bin-dir" binaries_directory
            ]

  let errors = catMaybes $
        [ aerr "-p|--per-spec-time" (per_spec_time == 0)
        , aerr "-c|--cores" (_cores == 0)
        , aerr "--size > 0" (_size <= 0)
        , aerr "--given and -t/--total-time can not be used together" (isJust given_dir && total_time /= 0)
        ]  ++ fileErr ++ ls

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  out   <- giveOutputDirectory output_directory
  seed_ <- giveSeed _seed
  givenSpecs <- giveSpec given_dir


  let config = EC.EssenceConfig
               { outputDirectory_ = out
               , mode_            = toEssenceMode _mode
               , totalTime_       = total_time
               , perSpecTime_     = per_spec_time
               , size_            = _size
               , cores_           = _cores
               , seed_            = seed_

               , totalIsRealTime    = total_is_real_time
               , deletePassing_     = delete_passing
               , binariesDirectory_ = binaries_directory
               , oldConjure_        = old_conjure
               , toolchainOutput_   = toolchain_ouput
               , notUseful          = S.fromList [(Savilerow_, NumberToLarge_)]
               , givenSpecs_        = givenSpecs
               }

  doMeta out no_csv binaries_directory
  generateEssence config

  where
    toEssenceMode :: ModeChoice -> EC.EssenceMode
    toEssenceMode TypeCheck = EC.TypeCheck_
    toEssenceMode Refine    = EC.Refine_
    toEssenceMode Solve     = EC.Solve_

mainWithArgs Instance{..} = do
  error . show . vcat $ ["gen instance not done yet" ]

mainWithArgs Reduce{..} = do

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
        , aerr "-c|--cores >0" (_cores == 0)
        ] ++ fileErr

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  case from_essence of
    False -> return ()
    True  ->  addSpecJson False (spec_directory </> "spec.essence")
          >>  addMeta (spec_directory </> "spec.spec.json")

  seed_ <- giveSeed _seed
  db    <- giveDb db_directory
  out   <- giveOutputDirectory output_directory

  let args = def{oErrKind_           = error_kind
                ,oErrStatus_         = error_status
                ,oErrChoices_        = error_choices
                ,outputDir_          = out
                ,specDir_            = spec_directory
                ,R.cores_            = _cores
                ,rgen_               = mkrGen (seed_)
                ,specTime_           = per_spec_time
                ,binariesDirectory_  = binaries_directory
                ,toolchainOutput_    = toolchain_ouput
                ,deletePassing_      = delete_passing
                ,resultsDB_          = db
                ,mostReducedChoices_ = error_choices
                ,resultsDB_dir       = db_directory
                }

  doMeta out no_csv binaries_directory

  (_,state) <- reduceMain args
  saveDB db_only_passing db_directory (resultsDB_  state)
  formatResults (delete_steps) state

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
        , aerr "-c|--cores >0" (_cores == 0)
        ] ++ fileErr

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  case from_essence of
    False -> return ()
    True  ->  addSpecJson False (spec_directory </> "spec.essence")
          >>  addMeta (spec_directory </> "spec.spec.json")

  seed_ <- giveSeed _seed
  db    <- giveDb db_directory
  out   <- giveOutputDirectory output_directory

  let args :: E.GState =
             def{E.oErrKind_           = error_kind
                ,E.oErrStatus_         = error_status
                ,E.oErrChoices_        = error_choices
                ,E.outputDir_          = out
                ,E.specDir_            = spec_directory
                ,E.cores_              = _cores
                ,E.rgen_               = mkrGen (seed_)
                ,E.specTime_           = per_spec_time
                ,E.binariesDirectory_  = binaries_directory
                ,E.toolchainOutput_    = toolchain_ouput
                ,E.deletePassing_      = delete_passing
                ,E.resultsDB_          = db
                ,E.choicesToUse_       = error_choices
                ,E.resultsDB_dir       = db_directory
                }

  doMeta out no_csv binaries_directory

  state <- generaliseMain args
  saveDB False db_directory (E.resultsDB_  state)


mainWithArgs Link{..} = do
  errors <- catMaybes <$> (mapM (dirExists "-d") directories )

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  sorterMain' directories

mainWithArgs SpecEE{..} = do
  errors <- catMaybes <$> (mapM (dirExists "-d") directories )

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  if meta_only then
      return ()
  else do
    putStrLn "Creating .spec.json files"
    specEMain print_specs directories

  putStrLn "Creating .meta.json files"
  metaMain directories

mainWithArgs Script_Toolchain{..} = do

  fileErr <- catMaybes <$> sequence
            [
              fileExists    "essence path" essence_path
            , fileExistsMay "--essence-param" essence_param
            , fileExistsMay "--choices" choices_path
            , dirExistsMay "--bin-dir" binaries_directory
            ]

  let errors = catMaybes
        [ aerr "-t|--total-time" (total_time == 0)
        , aerr "-c|--cores" (_cores == 0)
        ] ++ fileErr


  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  out <- giveOutputDirectory output_directory

  (code,_) <- Toolchain.toolchain Toolchain.ToolchainData
           {
             Toolchain.essencePath       = essence_path
           , Toolchain.outputDirectory   = out
           , Toolchain.toolchainTime     = total_time
           , Toolchain.essenceParam      = essence_param
           , Toolchain.refineType        = f refine_type choices_path
           , Toolchain.cores             = _cores
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


  let errors = catMaybes
        [ aerr "-c|--cores" (_cores == 0)
        ] ++ fileErr

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  out <- giveOutputDirectory output_directory

  (code,_) <- Recheck.toolchainRecheck Recheck.RecheckData
           {
             Recheck.essencePath       = essence_path
           , Recheck.outputDirectory   = out
           , Recheck.cores             = _cores
           , Recheck.binariesDirectory = binaries_directory
           , Recheck.oldConjure        = old_conjure
           , Recheck.toolchainOutput   = toolchain_ouput
           , Recheck.dryRun            = dry_run
           }
  exitWith code


aerr :: String -> Bool -> Maybe String
aerr n b | b = Just $ "Required: " ++ n
aerr _ _     = Nothing

dirExistsMay :: String -> Maybe FilePath -> IO (Maybe String)
dirExistsMay _ Nothing  = return Nothing
dirExistsMay s (Just x) = dirExists s x

dirExists :: String -> FilePath -> IO (Maybe String)
dirExists s x = do
  b <- doesDirectoryExist x
  return $ aerr (s ++ " ~ Directory does not exist " ++ x ) (not b)

fileExistsMay :: String -> Maybe FilePath -> IO (Maybe String)
fileExistsMay _ Nothing  = return Nothing
fileExistsMay s (Just x) = fileExists s x

fileExists ::String -> FilePath -> IO (Maybe String)
fileExists s x = do
  b <- doesFileExist x
  return $ aerr (s ++ " ~ File does not exist " ++ x ) (not b)



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



_essenceDebug :: IO ()
_essenceDebug = do
    let ec = Essence
             { output_directory   = Just "__/solve"
             , _mode              = Solve

             , total_time         = 20
             , per_spec_time      = 5
             , _size              = 2
             , _cores             = 1
             , _seed              = Just 44

             , delete_passing     = False
             , binaries_directory = Nothing
             , old_conjure        = False
             , limit_time         = Nothing
             , total_is_real_time = True
             , toolchain_ouput    = ToolchainNull_
             , no_csv             = False
             , given_dir          = Nothing
             }
    limiter (limit_time ec) (mainWithArgs ec)

_givenDebug :: IO ()
_givenDebug = do
    let ec = Essence
             { output_directory   = Just "out"
             , _mode              = Solve

             , total_time         = 0
             , per_spec_time      = 120
             , _size              = 4
             , _cores             = 1
             , _seed              = Just 44

             , delete_passing     = False
             , binaries_directory = Nothing
             , old_conjure        = False
             , limit_time         = Nothing
             , total_is_real_time = True
             , toolchain_ouput    = ToolchainScreen_
             , no_csv             = True
             , given_dir          = Just "zz"
             }
    limiter (limit_time ec) (mainWithArgs ec)


_reduceDebug :: IO ()
_reduceDebug = do
    let ec = Reduce{spec_directory      = "/Users/bilalh/Desktop/Results/_notable/reduce_examples/1425940601_40"
                   , error_kind         = RefineRandom_
                   , error_status       = StatusAny_
                   , error_choices      = Nothing
                   , list_kinds         = False
                   , list_statuses      = False
                   , output_directory   = Just "/Users/bilalh/Desktop/Results/_notable/reduce_examples/1425940601_40/out"
                   , per_spec_time      = 60
                   , _cores             = 1
                   , _seed              = Nothing
                   , toolchain_ouput    = ToolchainNull_
                   , binaries_directory = Nothing
                   , limit_time         = Nothing
                   , no_csv             = False
                   , delete_passing     = False
                   , db_directory       = Nothing
                   , delete_steps       = False
                   , db_only_passing    = False
                   , from_essence       = False
                   }
    limiter (limit_time ec) (mainWithArgs ec)
