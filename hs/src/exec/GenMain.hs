{-# LANGUAGE LambdaCase, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs?

module Main where

import           Gen.Arbitrary.Data
import           Gen.Classify.AddMeta        (metaMain)
import           Gen.Classify.AddSpecE       (specEMain)
import           Gen.Classify.Sorter         (sorterMain')
import           Gen.Essence.Generate        (generateEssence)
import           Gen.Helpers.StandardImports
import           Gen.IO.CmdArgsHelpers
import           Gen.IO.Toolchain            (kindsList, statusesList)
import qualified Gen.IO.Toolchain            as Toolchain
import qualified Gen.IO.ToolchainRecheck     as Recheck

import qualified Gen.Essence.Data            as EC
import           Gen.Reduce.Data             (RState (..), mkrGen)
import qualified Gen.Reduce.Data             as R
import           Gen.Reduce.FormatResults    (formatResults)
import           Gen.Reduce.Reduce           (reduceMain)
import           Gen.UI.UI
import           System.Console.CmdArgs      (cmdArgs)
import           System.CPUTime              (getCPUTime)
import           System.Environment          (withArgs)
import           System.Exit                 (exitFailure, exitSuccess)
import           System.Exit                 (exitWith)
import           System.Timeout              (timeout)
import           Text.Printf                 (printf)

main :: IO ()
main = do
  getArgs >>= \case
    [] -> do
       args <- helpArg
       void $ withArgs [args] (cmdArgs ui)
    [x] | x `elem` [ "essence", "reduce", "link", "meta", "json"
                   , "script-toolchain", "script-recheck"] -> do
       args <- helpArg
       void $ withArgs [x, args] (cmdArgs ui)

    ["reduce", "--list-kinds"] -> do
        mapM_ (putStrLn) kindsList
        exitSuccess
    ["reduce", "--list-statuses"] -> do
        mapM_ (putStrLn) statusesList
        exitSuccess

    ["--toolchain-path"] -> do
         dir <- Toolchain.getToolchainDir Nothing
         putStrLn dir

    ["", "--toolchain-path"] -> do
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


     getLimit x | Just i <- limit_time x, i <=0  = error "--limit-time must be > then 0"
     getLimit (Essence{_mode=m,limit_time = Nothing, total_time=t}) | m == TypeCheck=Just t
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
  let errors = catMaybes
        [ aerr "-t|--total-time" (total_time == 0)
        , aerr "-p|--per-spec-time" (per_spec_time == 0)
        , aerr "-c|--cores" (_cores == 0)
        , aerr "--size > 0" (_size <= 0)
        ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  seed_ <- giveSeed _seed
  let config = EC.EssenceConfig
               { outputDirectory_ = output_directory
               , mode_            = toEssenceMode _mode

               , totalTime_   = total_time
               , perSpecTime_ = per_spec_time
               , size_        = _size
               , cores_       = _cores
               , seed_        = seed_

               , totalIsRealTime    = total_is_real_time
               , deletePassing_     = delete_passing
               , binariesDirectory_ = binaries_directory
               , oldConjure_        = old_conjure
               , toolchainOutput_   = toolchain_ouput
               }

  generateEssence config

  where
    toEssenceMode :: ModeChoice -> EC.EssenceMode
    toEssenceMode TypeCheck = EC.TypeCheck_
    toEssenceMode Refine    = EC.Refine_
    toEssenceMode Solve     = EC.Solve_

mainWithArgs Instance{..} = do
  error . show . vcat $ ["gen instance not done yet" ]

mainWithArgs Reduce{..} = do

  let errors = catMaybes
        [ aerr "spec-directory" (null spec_directory)
        , aerr "-o|--output-directory" (null output_directory)
        , aerr "-p|--per-spec-time" (per_spec_time == 0)
        , aerr "-c|--cores" (_cores == 0)
        ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure


  seed_ <- giveSeed _seed
  let args = def{oErrKind_          = error_kind
                ,oErrStatus_        = error_status
                ,oErrEprime_        = Nothing
                ,outputDir_         = output_directory
                ,specDir_           = spec_directory
                ,R.cores_           = _cores
                ,rgen_              = mkrGen (seed_)
                ,specTime_          = per_spec_time
                ,binariesDirectory_ = binaries_directory
                ,toolchainOutput_   = toolchain_ouput
                }
  (_,state) <- reduceMain args
  formatResults state
  return ()


mainWithArgs Link{..} = do
  sorterMain' directories

mainWithArgs Meta{..} = do
  metaMain directories

mainWithArgs SpecEE{..} = do
  specEMain print_specs directories

mainWithArgs Script_Toolchain{..} = do
  let errors = catMaybes
        [ aerr "-o|--output-directory" (null output_directory)
        , aerr "-t|--total-time" (total_time == 0)
        , aerr "-c|--cores" (_cores == 0)
        ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  (code,_) <- Toolchain.toolchain Toolchain.ToolchainData
           {
             Toolchain.essencePath       = essence_path
           , Toolchain.outputDirectory   = output_directory
           , Toolchain.totalTime         = total_time
           , Toolchain.essenceParam      = essence_param
           , Toolchain.refineType        = refine_type
           , Toolchain.cores             = _cores
           , Toolchain.seed              = _seed
           , Toolchain.binariesDirectory = binaries_directory
           , Toolchain.oldConjure        = old_conjure
           , Toolchain.toolchainOutput   = toolchain_ouput
           }
  exitWith code

mainWithArgs Script_ToolchainRecheck{..} = do
  let errors = catMaybes
        [ aerr "-o|--output-directory" (null output_directory)
        , aerr "-c|--cores" (_cores == 0)
        ]

  case errors of
    [] -> return ()
    xs -> mapM putStrLn xs >> exitFailure

  (code,_) <- Recheck.toolchainRecheck Recheck.RecheckData
           {
             Recheck.essencePath       = essence_path
           , Recheck.outputDirectory   = output_directory
           , Recheck.cores             = _cores
           , Recheck.binariesDirectory = binaries_directory
           , Recheck.oldConjure        = old_conjure
           , Recheck.toolchainOutput   = toolchain_ouput
           }
  exitWith code


aerr :: String -> Bool -> Maybe String
aerr n b | b = Just $ "Required: " ++ n
aerr _ _     = Nothing


giveSeed :: Maybe Int -> IO Int
giveSeed (Just s)  = return s
giveSeed Nothing = randomRIO (0,2^(31 :: Int)-1)


_mainDebug :: IO ()
_mainDebug = do
    let ec = Essence
             { output_directory = "__/solve"
             , _mode            = Solve

             , total_time    = 20
             , per_spec_time = 5
             , _size         = 2
             , _cores        = 1
             , _seed         = Just 44

             , delete_passing     = False
             , binaries_directory = Nothing
             , old_conjure        = False
             , limit_time         = Nothing
             , total_is_real_time = True
             , toolchain_ouput    = ToolchainNull_
             }
    limiter (limit_time ec) (mainWithArgs ec)
