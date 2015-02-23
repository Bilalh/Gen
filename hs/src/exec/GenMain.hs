{-# LANGUAGE RecordWildCards,LambdaCase #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs?

module Main where

import TestGen.Helpers.StandardImports
import TestGen.Arbitrary.Data
import UI.UI

import TestGen.Classify.Sorter(sorterMain')
import TestGen.Classify.AddMeta(metaMain)
import TestGen.Classify.AddSpecE(specEMain)
import TestGen.Helpers.Runner(kindsList, statusesList)

import TestGen.Reduce.Data(RState(..),mkrGen)
import qualified TestGen.Reduce.Data as R

import TestGen.Reduce.Reduce(reduceMain)
import TestGen.Reduce.FormatResults(formatResults)

import TestGen.Essence.Generate(generateEssence)

import System.Console.CmdArgs ( cmdArgs )
import System.CPUTime ( getCPUTime )
import System.Timeout ( timeout )
import Text.Printf ( printf )

import System.Environment(withArgs)
import System.Exit(exitSuccess,exitFailure)


main :: IO ()
main = do
  getArgs >>= \case
    [] -> do
       void $ withArgs ["--help"] (cmdArgs ui)
    [x] | x `elem` ["essence", "reduce", "link", "meta","json"] ->
       void $ withArgs [x, "--help"] (cmdArgs ui)

    ["reduce", "--list-kinds"] -> do
        mapM_ (putStrLn) kindsList
        exitSuccess
    ["reduce", "--list-statuses"] -> do
        mapM_ (putStrLn) statusesList
        exitSuccess

    _ -> do
      input <- cmdArgs ui

      let workload = do
            putStrLn . show . vcat $ ["Command line options: ", pretty (groom input)]
            mainWithArgs input

      limiter (getLimit input) workload

   where
     getLimit x | Just i <- limit_time x, i <=0  = error "--limit-time must be > then 0"
     getLimit (Essence{_mode=m,limit_time = Nothing, total_time=t}) | m == TypeCheck = Just t
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
  let config = EssenceConfig
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
               }

  generateEssence config

  where
    toEssenceMode :: ModeChoice -> EssenceMode
    toEssenceMode TypeCheck = TypeCheck_
    toEssenceMode Refine    = Refine_
    toEssenceMode Solve     = Solve_

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
  let args = def{oErrKind_   = error_kind
                ,oErrStatus_ = error_status
                ,oErrEprime_ = Nothing
                ,outputDir_  = output_directory
                ,specDir_    = spec_directory
                ,R.cores_    = _cores
                ,newConjure_ = not old_conjure
                ,rgen_       = mkrGen (seed_)
                ,specTime_   = per_spec_time
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
             }
    limiter (limit_time ec) (mainWithArgs ec)
