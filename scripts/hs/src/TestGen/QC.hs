{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

module TestGen.QC where
import TestGen.QC.ArbitrarySpec

import Language.E hiding(trace)

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import TestGen.Runner(SettingI(..))
import TestGen.TestGen(runRefine')

import qualified Test.QuickCheck.Property as P


prop_specs_refine :: SpecE -> Property
prop_specs_refine specE =
    let spec = toSpec specE in
    typeChecks spec ==>
        monadicIO $ do
            -- liftM whenFail _2
            (ts, result) <- run $ runRefine' spec "__" 4
            case successful_ result of
                True  -> return ()
                False -> fail ts

typeChecks :: Spec -> Bool
typeChecks spec = case fst $ runCompESingle "Error while type checking." $
    typeCheckSpec spec of
        Left  e  -> True
        Right () -> True

infix 4 /==
(/==) :: (Eq a, Show a) => a -> a -> Property
x /== y =
  counterexample (show x ++ " == " ++ show y) (x /= y)


