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
import TestGen.Helpers.Runner(runRefine)
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