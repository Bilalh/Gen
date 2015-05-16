{-# LANGUAGE QuasiQuotes #-}
module Solver.SolverTest ( tests ) where

import Gen.Imports
import Gen.TestPrelude
import Gen.Solver.Solver
import Conjure.UI.IO
import Conjure.Language.Definition
import Test.Tasty.HUnit(Assertion,assertFailure,assertBool)
import System.FilePath(takeDirectory,takeFileName)


tests ::  IO TestTree
tests = do
  specs <- testSpecs
  parts <- mapM doSpec specs
  groups <- return $ testGroup "Solver" parts
  return groups


doSpec :: (MonadFail m, MonadIO m) => FilePath -> m TestTree
doSpec fp = do
  model <- readModelFromFile fp
  let solMay = solveSpec model
  possibleFps  <- liftIO $ allFilesWithSuffix ".solution" (takeDirectory fp)
  let parts = testGroup (takeDirectory fp)
        [
          testCase (pretty $ takeFileName $  fp) $
            checkSpec solMay possibleFps
        ]
  return parts


solveSpec :: Model -> Maybe Model
solveSpec model = do
  let (solution,_) = runIdentity $  runLogger LogNone $ solver model
  solution

checkSpec :: Maybe Solution -> [FilePath] -> Assertion
checkSpec Nothing [] = assertBool "No solution expected" True
checkSpec Nothing [x]  = do
  s <- readModelFromFile x
  assertFailure $ "Expected a solution, namely: " ++ (show $ pretty x) ++ "\n"
                                                  ++ (show $ pretty s)
checkSpec Nothing  xs = assertFailure $ "Expected a solution, possible: " ++ (show $ length xs)
checkSpec (Just s) [] = assertFailure $ "Expected no solution but got:\n"
                        ++ (show $ pretty s)
checkSpec (Just s) xs = do
  res <- pure any <*> pure (compareSpec) <*> (mapM readModelFromFile xs)
  (flip assertBool res)  $ show $ vcat ["No matching solution,"
                                       , pretty s
                                       ]
    where
    compareSpec :: Model -> Bool
    compareSpec c = pretty c == pretty s


testSpecs :: IO [FilePath]
testSpecs = allFilesWithSuffix ".essence" "tests"
