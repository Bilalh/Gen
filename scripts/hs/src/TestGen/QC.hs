{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

module TestGen.QC where

import TestGen.QC.ArbitrarySpec
import TestGen.Runner(SettingI(..))
import TestGen.TestGen(runRefine')
import Common.Helpers(timestamp)

import Language.E hiding(trace)
import Debug.Trace(trace)

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Test.QuickCheck.Property as P

import System.FilePath((</>), (<.>))
import System.Random(randomRIO)


prop_specs_refine :: Int -> FilePath -> SpecE -> Property
prop_specs_refine time out specE = do
    let spec = toSpec specE
    typeChecks spec ==>
        monadicIO $ do
            ts <- run timestamp >>= return . show
            num <- run (randomRIO (10,99) :: IO Int)  >>= return . show
            let uname  =  (ts ++ "_" ++ num )
            result <- run $ runRefine' spec (out </> uname ) time
            case successful_ result of
                True  -> return ()
                False -> fail uname



-- Strange things happen
typeChecks :: Spec -> Bool
typeChecks spec = True

-- typeChecks spec = case fst $ runCompESingle "Error while type checking." $
--     typeCheckSpec spec of
--         Left  e  ->
--             trace (show e ++ (show . pretty $ spec)) False
--         Right () -> True

infix 4 /==
(/==) :: (Eq a, Show a) => a -> a -> Property
x /== y =
  counterexample (show x ++ " == " ++ show y) (x /= y)


