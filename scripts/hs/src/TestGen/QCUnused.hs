{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-cse #-} 
-- cse means output is not outputted

module TestGen.QCUnused where

import AST.SpecE
import Language.E hiding(trace)
import Language.E.Pipeline.ReadIn(writeSpec)

import Common.Helpers(timestamp)

import TestGen.Arbitrary.Arbitrary
import TestGen.Helpers.Runner
import TestGen.Helpers.Args(TArgs(..))
import TestGen.Helpers.Runner(SettingI(..), RefineR)
import TestGen.Prelude


import Data.Time
import Data.Time.Clock.POSIX(getPOSIXTime)
import Debug.Trace(trace)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Property as QC


import System.Directory(createDirectoryIfMissing, getHomeDirectory, doesFileExist)
import System.FilePath((</>), (<.>), takeFileName)
import System.IO(IOMode(..),hPutStrLn, openFile, hClose)
import System.Random(randomRIO)

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Text.Groom(groom)

import Data.IORef(newIORef, readIORef, writeIORef, atomicWriteIORef)

import qualified Data.ByteString.Char8 as BS

import TestGen.Prelude
import TestGen.Arbitrary.Arbitrary(spec'', WithLogs)
import TestGen.Arbitrary.Type(atype_only)

import TestGen.QC
import TestGen.QCDebug
import Test.QuickCheck.Random(mkQCGen)


import  Test.QuickCheck.State
import Test.QuickCheck.Property hiding(Result)
import Test.QuickCheck.Text

quickTypeCheck :: (ArbSpec a) => a -> Args ->  IO (Maybe (a, Doc,Int))
quickTypeCheck _ args = do
    input  <- newIORef Nothing
    outputp <- newIORef Nothing

    result <- quickCheckWithResult args2 (tyWithLogs input outputp)
    case result of
        Failure {numTests,reason} -> do
            inn <- readIORef input
            out <- readIORef outputp
            let ret = do
                    jinn <- inn
                    return (jinn, pretty reason, numTests)                
                                
            return ret

        _  -> return Nothing

    where
    args2 = args{ chatty = True }

    tyWithLogs input outputp x = monadicIO $ do
        run $ atomicWriteIORef input (Just x)
        
        let specE = getSpec x
            sp = toSpec specE
            (res,doc) = typeChecks sp
        -- run $ putStrLn (show doc)
        
        if res then
            return ()
        else
            fail (show doc)

prop_specs_type_check_bool ::  ArbSpec a => a -> a -> Bool
prop_specs_type_check_bool _ arb = 
    let specE = getSpec arb
        sp = toSpec specE
        (res,doc) = typeChecks sp
    in res


quickCheckByBool :: (Arbitrary a, Show a) => Args -> (a -> Bool) -> IO (Maybe a, Int)
quickCheckByBool args prop  = do 
    input  <- newIORef Nothing
    result <- quickCheckWithResult args2 (logInput input prop)
    case result of
        Failure {numTests, output} -> do
            r <- readIORef input
            putStrLn "------"
            putStrLn output
            putStrLn "~~~~~~"
            return (r, numTests)
            
        -- TODO return acutal number of tests
        _  -> return ( Nothing, -1)
        
    where
    args2 = args{ chatty = True }
    
    logInput input prop x = monadicIO $ do 
        run $ atomicWriteIORef input (Just x)
        assert (prop x)
    
main1 = do
        (failed :: Maybe SpecE, _) <- quickCheckByBool
            stdArgs{QC.maxSize=5,maxSuccess=2000}
            (prop_specs_type_check_bool (undefined :: SpecE))
        case failed of
            Just x -> putStrLn $ "The input that failed was:\n" ++ (show $ pretty x)
            Nothing -> putStrLn "The test passed"


prop2 dir n gen= do
    let fp = ""
    forAll (specwithLogs n gen) $ \(WithLogs specE logs) -> do 
        let sp        = toSpec specE
        let (res,doc) = typeChecks sp
        monadicIO $ do
            if res then
                return ()
            else do
                -- run $ print . pretty $ doc
                ts <- run timestamp >>= return . show
                -- num <- run (randomRIO (10,99) :: IO Int)  >>= return . show
                let uname  =  (ts)
                run $ createDirectoryIfMissing True ( dir </> uname)
                let fp = dir </> uname </> "spec.essence"
                
                -- run $ print . pretty $ sp
                run $ writeSpec fp sp 
                run $ writeFile (fp <.> "error") (renderWide (doc <+> vcat ["---", prettySpecDebug $ sp] ) )
                run $ writeFile (fp <.> "specE") (show specE ++ "\n\n" ++ groom specE)
                run $ writeFile (fp <.> "logs") (renderNormal logs)
            
            
                fail (show doc)
    
     

    
runprop2 dir n gen  = quickCheckWith stdArgs{QC.maxSize=5,maxSuccess=2000} (prop2 dir n gen)

prop3 :: Depth -> Cores ->  Int -> FilePath -> Generators -> Property
prop3 n cores time out gen = do
    
    forAll (specwithLogs n gen) $ \(WithLogs specE logs) -> do 
        
        let sp = toSpec specE
        fst (typeChecks sp) ==>
            monadicIO $ do
                ts <- run timestamp >>= return . show
                num <- run (randomRIO (10,99) :: IO Int)  >>= return . show
                let uname  =  (ts ++ "_" ++ num )
                run $ createDirectoryIfMissing True (out </> uname)
                run $  writeFile (out </> uname </> "spec.logs" ) (renderNormal logs)
                run $  writeFile (out </> uname </> "spec.specE" ) (show specE)
            


                result <- run $ runRefine' 3 cores sp (out </> uname ) time False
                case successful_ result of
                    True  -> return ()
                    False -> fail uname



generateSpecs2 :: TArgs -> Generators -> IO ()
generateSpecs2 TArgs{..} gen = do
    let maxSuccess = totalTime_  `div` perSpecTime_
    
    createDirectoryIfMissing  True tmpdir
    case typecheckOnly_ of 
        (Just times) -> do
            error "no typecheckOnly_"
        
        Nothing -> do
            putStrLn "Generating specs, with depth up to size 5"
            startTime <- round `fmap` getPOSIXTime
            failed <- helper startTime []
            saveFailures failed (baseDirectory_ </> "failures.txt")


    where

    replay = case rseed_ of 
        Just iseed ->  Just (mkQCGen iseed, size_)
        Nothing    -> Nothing
    
    
    tmpdir  = baseDirectory_ </> "temp"
    

    saveFailures :: [String] -> FilePath -> IO ()
    saveFailures  = saveFailures' "spec.essence"
   
    saveFailures' :: String -> [String] -> FilePath -> IO ()
    saveFailures' name failed fp  = do 
        let paths = map (\fn -> baseDirectory_ </> fn </> name) failed
        print ("Current Failures found" :: String)
        putStrLn (unlines paths)
        writeToFile ( fp ) paths             
    
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

                r <- quickCheckWithResult stdArgs{QC.maxSize=size_,maxSuccess, replay}
                    (prop3 size_ cores_ perSpecTime_ baseDirectory_ gen)
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

                    (f@Failure{reason}, True)  -> do
                        saveFailures results' (tmpdir </> (show after) <.> "txt")
                        writeFile (baseDirectory_ </> reason </> "spec.qc")  ( show f{output=""})
                        helper startTime results'
                        
                    (_, True) -> do
                      helper startTime results'

    addResults :: Result ->  [String] -> [String]
    addResults Failure{reason} arr = (reason) : arr
    addResults _ arr = arr
    
computeSize' a n d
          -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
          -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
          | n `roundTo` QC.maxSize a +  QC.maxSize a <= maxSuccess a ||
            n >= maxSuccess a ||
            maxSuccess a `mod`  QC.maxSize a == 0 = (n `mod`  QC.maxSize a + d `div` 10) `min`  QC.maxSize a
          | otherwise =
            ((n `mod`  QC.maxSize a) *  QC.maxSize a `div` (maxSuccess a `mod`  QC.maxSize a) + d `div` 10) `min`  QC.maxSize a

n `roundTo` m = (n `div` m) * m    


ab b = do
    b < 100000

-- | Prints out the generated testcase every time the property is tested.
-- Only variables quantified over /inside/ the 'verbose' are printed.
vv :: Testable prop => prop -> Property
vv = mapResult (\res -> res { callbacks = newCallbacks (callbacks res) })
  where newCallbacks cbs =
          -- PostTest Counterexample (\st res -> putLine (terminal st) (status res ++ ":")):
          -- []
          [ PostTest Counterexample f | PostFinalFailure Counterexample f <- cbs ]
        status MkResult{ok = Just True} = "Passed"
        status MkResult{ok = Just False} = "Failed"
        status MkResult{ok = Nothing} = "Skipped (precondition false)"