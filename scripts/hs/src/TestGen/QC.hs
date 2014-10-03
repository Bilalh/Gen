{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TestGen.QC where

import AST.SpecE

import TestGen.Arbitrary.Arbitrary
import TestGen.Runner(SettingI(..))
import TestGen.TestGen(runRefine')
import Common.Helpers(timestamp)

import Language.E hiding(trace)
import Debug.Trace(trace)

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Test.QuickCheck.Property as QC
import qualified Test.QuickCheck as QC


import System.FilePath((</>))
import System.Random(randomRIO)


prop_specs_refine :: Int -> FilePath -> SpecE -> Property
prop_specs_refine time out specE = do
    let sp = toSpec specE
    typeChecks sp ==>
        monadicIO $ do
            ts <- run timestamp >>= return . show
            num <- run (randomRIO (10,99) :: IO Int)  >>= return . show
            let uname  =  (ts ++ "_" ++ num )
            result <- run $ runRefine' sp (out </> uname ) time
            case successful_ result of
                True  -> return ()
                False -> fail uname



typeChecks :: Spec -> Bool
typeChecks sp = case fst $ runCompESingle "Error while type checking." $
    typeCheckSpec sp of
        Left  e  ->
            trace (show e ++ (show . pretty $ sp)) False
        Right () -> True

infix 4 /==
(/==) :: (Eq a, Show a) => a -> a -> Property
x /== y =
  counterexample (show x ++ " == " ++ show y) (x /= y)


rmain =
    quickCheckWith stdArgs{QC.maxSize=100,maxSuccess=100} (prop_specs_refine 30 "__")
