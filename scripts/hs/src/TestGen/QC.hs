{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TestGen.QC where

import AST.SpecE

import TestGen.Arbitrary.Arbitrary
import TestGen.Runner(SettingI(..))
import TestGen.Old.TestGen(runRefine')
import Common.Helpers(timestamp)

import Language.E hiding(trace)
import Debug.Trace(trace)

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Test.QuickCheck.Property as QC
import qualified Test.QuickCheck as QC

import Data.Time.Clock.POSIX(getPOSIXTime)

import System.FilePath((</>))
import System.Random(randomRIO)
import Language.E

import TestGen.Args(TArgs(..))
import System.IO(IOMode(..),hPutStrLn, openFile, hClose)

prop_specs_refine :: Int -> FilePath -> SpecE -> Property
prop_specs_refine time out specE = do
    let sp = toSpec specE
    fst (typeChecks sp) ==>
        monadicIO $ do
            ts <- run timestamp >>= return . show
            num <- run (randomRIO (10,99) :: IO Int)  >>= return . show
            let uname  =  (ts ++ "_" ++ num )
            result <- run $ runRefine' sp (out </> uname ) time
            case successful_ result of
                True  -> return ()
                False -> fail uname


prop_specs_type_check :: SpecE -> Property
prop_specs_type_check specE = do
    let sp = toSpec specE
        (res,doc) = typeChecks sp
    counterexample (show doc ++ (show . pretty $ sp)) (res)

typeChecks :: Spec -> (Bool, Doc)
typeChecks sp = case fst $ runCompESingle "Error while type checking." $
    typeCheckSpec sp of
        Left  e  -> (False, e)
            -- trace (show e ++ (show . pretty $ sp)) False
        Right () -> (True, "")

infix 4 /==
(/==) :: (Eq a, Show a) => a -> a -> Property
x /== y =
  counterexample (show x ++ " == " ++ show y) (x /= y)


rmain =
    quickCheckWith stdArgs{QC.maxSize=4,maxSuccess=50} (prop_specs_refine 100 "__")

cmain =
    quickCheckWith stdArgs{QC.maxSize=4,maxSuccess=2000} (prop_specs_type_check)

prop_int :: Int -> Property
prop_int x  = do
    counterexample (show x) (x < 10)


generate :: TArgs -> IO ()
generate TArgs{..} = do
    let maxSuccess = totalTime_  `div` perSpecTime_
    --  Sanity checks
    putStrLn "Typechecking 2000 random specs, with depth up to size 4"
    quickCheckWith stdArgs{QC.maxSize=4,maxSuccess=2000}
        (prop_specs_type_check)

    putStrLn "Generating specs, with depth up to size 4"
    startTime <- round `fmap` getPOSIXTime
    failed <- helper startTime []

    let paths = map (\fn -> baseDirectory_ </> fn </> "spec.essence" ) failed

    print paths
    writeToFile ( baseDirectory_ </> "failures.txt" ) paths


    where

    writeToFile name arr =
      do h <- openFile (name) WriteMode
         mapM_ (hPutStrLn h) arr
         hClose h

    helper :: Int -> [String] -> IO [String]
    helper startTime results = do
        before <- round `fmap` getPOSIXTime

        let maxSuccess = (totalTime_ - (before - startTime)) `div` perSpecTime_

        case maxSuccess <= 0 of
            True  -> return results
            False -> do

                r <- quickCheckWithResult stdArgs{QC.maxSize=5,maxSuccess}
                    (prop_specs_refine perSpecTime_ baseDirectory_)
                let results'  = addResults r results


                after <- round `fmap` getPOSIXTime
                putStrLn $ "Running for " ++ (show $ after - startTime ) ++ " s"

                case (r, after - startTime < totalTime_) of

                    (GaveUp{}, _ ) -> do
                        putStrLn "Gaveup"
                        return results'

                    (_, False) -> do
                        putStrLn "Time up"
                        return results'

                    (_, True)  -> helper startTime results'

        where
        addResults :: Result ->  [String] -> [String]
        addResults Failure{reason} arr = (reason) : arr
        addResults _ arr = arr
