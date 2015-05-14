{-# LANGUAGE QuasiQuotes #-}
module Gen.Essence.Solver where

import Gen.Imports
import Conjure.UI.IO
import Conjure.Language.Definition
import Conjure.Language.Instantiate
import Conjure.Language.TH

type Solution = Model

solve :: Model -> IO Solution
solve model = do
  let exprs =  concat [ xs | (SuchThat xs) <- mStatements model ]

  let vals = [("var1", [essence| ({} : `set of bool`) |])]
  -- let vals = [("var1", [essence| {1} |])]

  res <- forM exprs $ \expr -> do
                instantiateExpression vals expr

  let sat = all isTrue res

  case sat of
    True  -> putStrLn "Vaild"
    False -> putStrLn "Invaild"

  return model

isTrue :: Constant -> Bool
isTrue (ConstantBool True) = True
isTrue _                   = False

run :: IO ()
run = do
  let fp = "/Users/bilalh/Desktop/Results/_notable/solver/a.essence"
  model <- readModelFromFile fp
  solution <- solve model
  print . pretty $ solution
