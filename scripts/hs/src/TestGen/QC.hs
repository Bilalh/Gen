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
import TestGen.Helpers.Args(TArgs(..))
import TestGen.Helpers.Runner(SettingI(..), RefineR)
import TestGen.Prelude(SpecState, Generators,Domain, listOfBounds,SS(depth_),FG ,ArbSpec(..))


import Data.Time
import Data.Time.Clock.POSIX(getPOSIXTime)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Property as QC

import System.Directory(createDirectoryIfMissing, getHomeDirectory, doesFileExist)
import System.FilePath((</>), (<.>), takeFileName)
import System.IO(IOMode(..),hPutStrLn, openFile, hClose)
import System.Random(randomRIO)

import Test.QuickCheck
import Test.QuickCheck.Random(mkQCGen)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Text.Groom(groom)

import TestGen.QCDebug

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
        (show doc ++ (show $ pretty sp) )
        (res)

typeChecks :: Spec -> (Bool, Doc)
typeChecks sp = case fst $ runCompESingle "Error while type checking." $
    typeCheckSpec sp of
        Left  e  -> (False, e)
            -- trace (show e ++ (show . pretty $ sp)) False
        Right () -> (True, "")



generateSpecs :: ArbSpec a => WithLogs a -> TArgs -> IO ()
generateSpecs unused TArgs{..} = do
    let maxSuccess = totalTime_  `div` perSpecTime_
    
    createDirectoryIfMissing  True tmpdir
    case typecheckOnly_ of 
        (Just times) -> do
            putStrLn $ "Type checking " ++ (show times) ++ 
                " random specs, with depth up to size 5"
            
            (failed, ourErrors) <- typecheckHelper times [] [] 0
            saveFailures failed (baseDirectory_ </> "failures.txt")
            saveFailures' "gen.error" ourErrors (baseDirectory_ </> "genErrors.txt")
        
        Nothing -> do
            --  Sanity check
            putStrLn "Typechecking 2000 random specs, with depth up to size 5"
            quickCheckWith stdArgs{QC.maxSize=size_,maxSuccess=2000, replay}
                (prop_specs_type_check unused )
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

    typecheckHelper :: Int -> [String] -> [String] -> Int -> IO ([String], [String])
    typecheckHelper left results ourErrors index = do 
        putStrLn $ "left: " ++ (show left)
        
        let indexStr = padShowInt 4 index
            dir = baseDirectory_ </> (indexStr)
        createDirectoryIfMissing True dir
        
        r <- quickTypeCheck1 unused 
            (dir </> "spec.essence")
            stdArgs{QC.maxSize=size_,maxSuccess=left, replay}
        case r of
            (Nothing) -> do
                return (results, ourErrors)
            (Just (count, output)) -> do 
                                
                specExist <-  doesFileExist (dir </> "spec.essence")
                when (not specExist) $ do 
                    writeFile (dir </> "gen.error") output 
                
                let (results', ourErrors') =      
                        if specExist then
                            ( indexStr : results, ourErrors)
                        else 
                            ( results, indexStr : ourErrors)
                                
                let leftAfter = left - count
                if leftAfter > 0 then
                    typecheckHelper (leftAfter) results' ourErrors' (index+1)
                else
                    return (results', ourErrors')


    helper :: Int -> [String] -> IO [String]
    helper startTime results = do
        before <- round `fmap` getPOSIXTime

        let maxSuccess = (totalTime_ - (before - startTime)) `div` perSpecTime_

        case maxSuccess <= 0 of
            True  -> return results
            False -> do

                r <- quickCheckWithResult stdArgs{QC.maxSize=size_,maxSuccess, replay}
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


quickTypeCheck1 :: (ArbSpec a) => WithLogs a -> FilePath -> Args ->  IO (Maybe (Int, String))
quickTypeCheck1 unused fp args = do
    
    result <- quickCheckWithResult args{chatty=False} (prop_typeCheckSave fp unused)
    case result of 
        Failure {numTests,reason, output} -> do
            return (Just (numTests, output ))
        _ -> return Nothing

    where

prop_typeCheckSave :: (ArbSpec a) => FilePath -> WithLogs a -> WithLogs a -> Property
prop_typeCheckSave fp _  (WithLogs arb logs) = do
    let specE = getSpec arb
        sp = toSpec specE
        (res,doc) = typeChecks sp
    monadicIO $ do
        if res then
            return ()
        else do
            -- run $ print . pretty $ doc
            run $ print . pretty $ sp
            run $ writeSpec fp sp 
            run $ writeFile (fp <.> "error") (renderWide (doc <+> vcat ["---", prettySpecDebug $ sp] ) )
            run $ writeFile (fp <.> "specE") (show specE ++ "\n\n" ++ groom specE)
            run $ writeFile (fp <.> "logs") (renderNormal logs)
            
            
            fail (show doc)



rmain n =
    quickCheckWith stdArgs{QC.maxSize=n,maxSuccess=50} 
    (prop_specs_refine (undefined :: WithLogs SpecE) 7 10  "__")

cmain unused seedInt n = do

    int <- case seedInt of 
            Just i  -> return i 
            Nothing -> randomRIO (1,2^31)

    replay <- return $ Just (mkQCGen int, n)

    c <- getCurrentTime
    let (y,m,d) = toGregorian $ utctDay c
    home <- getHomeDirectory
    date <- round `fmap` getPOSIXTime

    let dir = home </> "__" </> "logs" </> "cmain"
            </> ( intercalate "-" [show y, padShowInt 2 m, padShowInt 2 d] )
            </> (show date)

    createDirectoryIfMissing True dir

    res <- quickCheckWithResult stdArgs{QC.maxSize=n,maxSuccess=2000, replay} 
        (prop_typeCheckSave (dir </> "spec.essence") unused  ) 
    case res of
        f@Failure{reason, output, usedSize} -> do
            writeFile (dir </> "spec.output")  (output ++ "\nseed: " ++ (show int) ++ "\n" ++ show f{output=""})
            putStrLn $ "seed: " ++ (show int)
            putStrLn $ "usedSize: " ++ (show usedSize)
        _ -> return ()
