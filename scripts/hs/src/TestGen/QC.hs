{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}

{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-cse #-} 
-- cse means output is not outputted

module TestGen.QC where

import AST.SpecE 
import Language.E hiding(trace)
import Language.E.Pipeline.ReadIn(writeSpec)

import Common.Helpers(timestamp)

import TestGen.Arbitrary.Arbitrary
import TestGen.Helpers.Runner(runRefine)
import TestGen.Arbitrary.Domain(dom)
import TestGen.Helpers.Args(TArgs(..))
import TestGen.Helpers.Runner(SettingI(..), RefineR)
import TestGen.Prelude(SpecState, Generators,Domain, listOfBounds,SS(depth_),FG ,ArbSpec(..))


import Data.Time
import Data.Time.Clock.POSIX(getPOSIXTime)
import Debug.Trace(trace)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Property as QC

import System.Directory(createDirectoryIfMissing, getHomeDirectory)
import System.FilePath((</>), (<.>), takeFileName)
import System.IO(IOMode(..),hPutStrLn, openFile, hClose)
import System.Random(randomRIO)

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Text.Groom(groom)

import Data.IORef(newIORef, readIORef, writeIORef, atomicWriteIORef)

import qualified Data.ByteString.Char8 as BS


type Cores = Int
prop_specs_refine :: ArbSpec a => WithLogs a -> Cores ->  Int -> FilePath -> WithLogs a -> Property
prop_specs_refine  _ cores time out (WithLogs arb logs) = do
    let specE = getSpec arb
        sp    = toSpec specE
    fst (typeChecks sp) ==>
        monadicIO $ do
            ts <- run timestamp >>= return . show
            num <- run (randomRIO (10,99) :: IO Int)  >>= return . show
            let uname  =  (ts ++ "_" ++ num )
            run $ createDirectoryIfMissing True (out </> uname)
            run $  writeFile (out </> uname </> "spec.logs" ) (renderNormal logs)
            run $  writeFile (out </> uname </> "spec.specE" ) (show specE)
            


            result <- run $ runRefine' cores sp (out </> uname ) time
            case successful_ result of
                True  -> return ()
                False -> fail uname


prop_specs_type_check ::  ArbSpec a => a -> a -> Property
prop_specs_type_check _ arb = do
    let specE = getSpec arb
        sp = toSpec specE
        (res,doc) = typeChecks sp
    counterexample
        (show doc ++ (show . typeChecks $ sp) )
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


generateSpecs :: ArbSpec a => WithLogs a -> TArgs -> IO ()
generateSpecs unused TArgs{..} = do
    let maxSuccess = totalTime_  `div` perSpecTime_
    
    createDirectoryIfMissing  True tmpdir
    case typecheckOnly_ of 
        (Just times) -> do
            putStrLn $ "Typechecking " ++ (show times) ++ 
                " random specs, with depth up to size 5"
            
            failed <- typecheckHelper times [] 0
            saveFailures failed (baseDirectory_ </> "failures.txt")
        
        Nothing -> do
            --  Sanity check
            putStrLn "Typechecking 2000 random specs, with depth up to size 5"
            quickCheckWith stdArgs{QC.maxSize=5,maxSuccess=2000}
                (prop_specs_type_check unused )
            putStrLn "Generating specs, with depth up to size 5"
            startTime <- round `fmap` getPOSIXTime
    
            failed <- helper startTime []
            saveFailures failed (baseDirectory_ </> "failures.txt")


    where

    tmpdir  = baseDirectory_ </> "temp"
    

    saveFailures :: [String] -> FilePath -> IO ()
    saveFailures failed fp  = do 
        -- let paths = map (\fn -> takeFileName' baseDirectory_ </> fn </> "spec.essence" ) failed
        let paths = map (\fn -> baseDirectory_ </> fn </> "spec.essence" ) failed
        print ("Current Failures found" :: String)
        putStrLn (unlines paths)
        writeToFile ( fp ) paths
                
    
    writeToFile name arr =
      do h <- openFile (name) WriteMode
         mapM_ (hPutStrLn h) arr
         hClose h

    typecheckHelper :: Int -> [String] ->Int -> IO [String]
    typecheckHelper left results index = do 
        putStrLn $ "left: " ++ (show left)
        
        let indexStr = padShowInt 4 index
            dir = baseDirectory_ </> (indexStr)
        createDirectoryIfMissing True dir
        
        r <- quickTypeCheck1 unused 
            (dir </> "spec.essence")
            stdArgs{QC.maxSize=5,maxSuccess=left}
        case r of
            (Nothing) -> do
                return results
            (Just (count)) -> do 
                                
                let results' = (indexStr) : results
                let leftAfter = left - count
                if leftAfter > 0 then
                    typecheckHelper (leftAfter) results' (index+1)
                else
                    return results'


    helper :: Int -> [String] -> IO [String]
    helper startTime results = do
        before <- round `fmap` getPOSIXTime

        let maxSuccess = (totalTime_ - (before - startTime)) `div` perSpecTime_

        case maxSuccess <= 0 of
            True  -> return results
            False -> do

                r <- quickCheckWithResult stdArgs{QC.maxSize=5,maxSuccess}
                    (prop_specs_refine unused 
                        cores_ perSpecTime_ baseDirectory_)
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

                    (_, True)  -> do
                        saveFailures results' (tmpdir </> (show after) <.> "txt")
                        helper startTime results'

    addResults :: Result ->  [String] -> [String]
    addResults Failure{reason} arr = (reason) : arr
    addResults _ arr = arr

takeFileName' :: FilePath -> FilePath
takeFileName' fp = case reverse fp of
    ('/': xs) -> takeFileName (reverse xs)
    _         -> takeFileName fp

runRefine' :: Int -> Spec -> FilePath -> Int -> IO RefineR
runRefine' cores spec dir specTime = do
    print . pretty $ spec

    createDirectoryIfMissing True  dir

    let name = (dir </> "spec" <.> ".essence")
    writeSpec name spec

    let specLim = specTime
    result <- runRefine cores name dir specLim
    putStrLn . groom $  result
    return result



rmain n =
    quickCheckWith stdArgs{QC.maxSize=n,maxSuccess=50} 
    (prop_specs_refine (undefined :: WithLogs SpecE) 7 10  "__")

cmain n = do

    c <- getCurrentTime
    let (y,m,d) = toGregorian $ utctDay c
    home <- getHomeDirectory
    date <- round `fmap` getPOSIXTime

    let dir = home </> "__" </> "logs" </> "cmain"
            </> ( intercalate "-" [show y, padShowInt 2 m, padShowInt 2 d] )

    createDirectoryIfMissing True dir

    res <- quickCheckWithResult stdArgs{QC.maxSize=n,maxSuccess=2000} 
        (prop_specs_type_check (undefined :: SpecE))
    case res of
        Failure{reason, output} ->
            writeFile (dir </> (show date) <.> "output") output

        _ -> return ()


quickTypeCheck1 :: (ArbSpec a) => WithLogs a -> FilePath -> Args ->  IO (Maybe (Int))
quickTypeCheck1 unused fp args = do
    
    result <- quickCheckWithResult args{chatty=False} (ty unused)
    case result of 
        Failure {numTests,reason} -> do
            return (Just (numTests ))
        _ -> return Nothing

    where
        ty :: (ArbSpec a) => WithLogs a -> WithLogs a -> Property
        ty _ (WithLogs arb logs) = do
            let specE = getSpec arb
                sp = toSpec specE
                (res,doc) = typeChecks sp
            monadicIO $ do
                if res then
                    return ()
                else do
                    -- run $ print . pretty $ doc
                    -- run $ print . pretty $ sp
                    run $ writeSpec fp sp 
                    run $ writeFile (fp <.> "error") (renderWide (doc <+> vcat ["---", prettySpecDebug $ sp] ) )
                    run $ writeFile (fp <.> "specE") (show specE ++ "\n\n" ++ groom specE)
                    run $ writeFile (fp <.> "logs") (renderNormal logs)
                    
                    
                    fail (show doc)

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
