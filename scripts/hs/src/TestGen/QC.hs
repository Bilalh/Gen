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

import System.FilePath((</>), (<.>), takeFileName)
import System.Directory(createDirectoryIfMissing, getHomeDirectory)
import System.Random(randomRIO)
import Language.E

import TestGen.Args(TArgs(..))
import System.IO(IOMode(..),hPutStrLn, openFile, hClose)
import Data.Time


type Cores = Int

prop_specs_refine :: Cores ->  Int -> FilePath -> WithLogs SpecE -> Property
prop_specs_refine cores time out (WithLogs specE logs) = do
    let sp = toSpec specE
    fst (typeChecks sp) ==>
        monadicIO $ do
            ts <- run timestamp >>= return . show
            num <- run (randomRIO (10,99) :: IO Int)  >>= return . show
            let uname  =  (ts ++ "_" ++ num )
            run $ createDirectoryIfMissing True (out </> uname)
            run $  writeFile (out </> uname </> "spec.logs" ) (renderNormal logs)


            result <- run $ runRefine' cores sp (out </> uname ) time
            case successful_ result of
                True  -> return ()
                False -> fail uname


prop_specs_type_check ::  WithLogs SpecE -> Property
prop_specs_type_check (WithLogs specE logs) = do
    let sp = toSpec specE
        (res,doc) = typeChecks sp
    counterexample
        (show doc ++ (show . pretty $ sp) ++ (show . pretty $ logs ) )
        (res)

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

prop_int :: Int -> Property
prop_int x  = do
    counterexample (show x) (x < 10)


generate :: TArgs -> IO ()
generate TArgs{..} = do
    let maxSuccess = totalTime_  `div` perSpecTime_
    --  Sanity checks
    putStrLn "Typechecking 2000 random specs, with depth up to size 5"
    quickCheckWith stdArgs{QC.maxSize=5,maxSuccess=2000}
        (prop_specs_type_check)

    putStrLn "Generating specs, with depth up to size 5"
    startTime <- round `fmap` getPOSIXTime
    failed <- helper startTime []
    

    -- let paths = map (\fn -> takeFileName' baseDirectory_ </> fn </> "spec.essence" ) failed
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
                    (prop_specs_refine cores_ perSpecTime_ baseDirectory_)
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

takeFileName' :: FilePath -> FilePath
takeFileName' fp = case reverse fp of
    ('/': xs) -> takeFileName (reverse xs)
    _         -> takeFileName fp



rmain n =
    quickCheckWith stdArgs{QC.maxSize=n,maxSuccess=50} (prop_specs_refine 7 10  "__")

cmain n = do

    c <- getCurrentTime
    let (y,m,d) = toGregorian $ utctDay c
    home <- getHomeDirectory
    date <- round `fmap` getPOSIXTime

    let dir = home </> "__" </> "logs" </> "cmain"
            </> ( intercalate "-" [show y, padShowInt 2 m, padShowInt 2 d] )

    createDirectoryIfMissing True dir

    res <- quickCheckWithResult stdArgs{QC.maxSize=n,maxSuccess=2000} (prop_specs_type_check)
    case res of
        Failure{reason, output} ->
            writeFile (dir </> (show date) <.> "output") output

        _ -> return ()
