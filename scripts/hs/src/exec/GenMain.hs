{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs

module Main where

import TestGen.Prelude
import UI.UI

import TestGen.Classify.Sorter(sorterMain')
import TestGen.Classify.AddMeta(metaMain)
import TestGen.Classify.AddSpecE(specEMain)
import TestGen.Helpers.Runner(kindsList, statusesList)


import System.Console.CmdArgs ( cmdArgs )
import System.CPUTime ( getCPUTime )
import System.Timeout ( timeout )
import Text.Printf ( printf )

import System.Environment(getArgs, withArgs)
import System.Exit(exitSuccess)

main :: IO ()
main = do
  getArgs >>= \case
    [] -> do
       void $ withArgs ["--help"] (cmdArgs ui)
    [x] | x `elem` ["reduce", "link", "meta", "specE"] ->
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
            putStrLn . show $ "Command line options: " <+> pretty (groom input)
            mainWithArgs input

      case limit_time input of
        Just sec | sec > 0 -> do
          putStrLn $ "Running with a timelimit of " ++ show sec ++ " seconds."
          res <- timeout (sec * 1000000) workload
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
        _ -> workload

mainWithArgs :: UI -> IO ()
mainWithArgs Reduce{..} = do
  putStrLn "reducing"

mainWithArgs Link{..} = do
  sorterMain' directories

mainWithArgs Meta{..} = do
  metaMain directories

mainWithArgs SpecEE{..} = do
  specEMain directories
