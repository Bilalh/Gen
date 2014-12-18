{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module TestGen.Reduce.Reduce where

import TestGen.Reduce.Data
import TestGen.Reduce.Runner
import TestGen.Reduce.Reduction


import TestGen.Prelude
import TestGen.Helpers.Runner(KindI(..))
import TestGen.QCDebug(specE1)

import qualified TestGen.Arbitrary.Arbitrary as A
import qualified TestGen.Arbitrary.Domain as A
import qualified TestGen.Arbitrary.Expr as A

import qualified Data.Map as M
import qualified Test.QuickCheck as QC

import Control.Arrow((&&&))
import System.Random(randomIO)


reduceMain :: SpecE -> IO SpecE 
reduceMain s1 = do
    
    (sfin,state) <- (flip runStateT) _tempRR $ do
        -- s1 <- removeConstraints sp
        s2 <- simplyConstraints s1
    
        sfin <- return s2 
        return sfin 

    putStrLn "----"    
    putStrLn "Start"
    print . pretty $ s1
    putStrLn "----"    
    putStrLn "Final"
    print . pretty $ sfin
    putStrLn "----"    
    
    return sfin

    
simplyConstraints :: SpecE -> RR SpecE
simplyConstraints (SpecE ds es) = do
    fin <- process (doConstraints es)
    if fin == [] then do 
        runSpec (SpecE ds []) >>= \case
            True  -> return (SpecE ds [])
            False -> return (SpecE ds es)
            
        return (SpecE ds es)
    else
        return (SpecE ds fin)
    
    where
    process :: [[Expr]] -> RR [Expr]
    
    -- cannot simply any futher
    process xs | any (== []) xs = return []
    
    process xs | all (singleElem) xs = do
        let fix = map head xs
        res <- runSpec (SpecE ds fix)
        if res then do
            return fix
        else
            return []
        
    process esR = do
        fix <- choose esR
        res <- runSpec (SpecE ds fix)
        if res then do
            inner <- process (doConstraints fix )
            if inner == [] then
                return fix
            else 
                return inner
        else 
            removeNext esR >>= process
    
    -- Fix the next constraint
    choose :: [[Expr]] -> RR [Expr]
    choose esR = do
        return $ map pickFirst esR
        
        where 
        pickFirst []    = error "pickfirst empty"
        pickFirst [x]   = x
        pickFirst (x:_) = x 
        
    -- Keep the orginal exprs apart from the first
    doConstraints :: [Expr] -> [[Expr]]
    doConstraints []      = [[]]  
    doConstraints (x:xs)  = (reduce x) : map (\y -> y : reduce y) xs
           
    removeNext :: [[a]] -> RR [[a]]
    removeNext []                     = error "removeNext empty"
    removeNext xs | all singleElem xs = return xs
    removeNext xs | any null xs       = error "removeNext sub empty"

    removeNext ([x]:xs)    = ([x]:)  <$> removeNext xs
    removeNext ((_:fs):xs) = return $ fs:xs
    removeNext (x:xs )     = (x:) <$> removeNext xs
    
    
removeUnusedDomain :: SpecE -> RR SpecE    
removeUnusedDomain sp = undefined  


    
tailR :: [a] -> [a]    
tailR []     = error "tailR empty list"
tailR [x]    = [x]
tailR (_:xs) = xs
    
singleElem :: [a] -> Bool
singleElem [x] = True
singleElem _   = False
    

_reduce :: (Reduce a, ToEssence a, FromEssence a) => E -> IO [a]
_reduce e = 
    case  fromEssence e of 
        Left err -> error . show .  (pretty &&& pretty . groom)  $ err
        Right ee -> do
            let res = reduce ee
            mapM_ (print  . pretty . toEssence)  res
            return res
 
_e :: FromEssence a => E -> a
_e e =  case fromEssence e of 
        Left err -> error . show .  (pretty &&& pretty . groom) $ err
        Right ee -> ee


_tempRR :: RState
_tempRR = RState{oErrKind_         =Validate_
                ,oErrEprime_       = "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_errors/Validate_/ErrorUnknown_/1418178864_89/model000001.eprime"
                ,mostReduced_      = Nothing
                ,mostReducedFP_    = Nothing
                ,otherErrorsFound_ = []
                ,outputdir_        = "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_errors/Validate_/ErrorUnknown_/1418178864_89/reduce/"
                ,rgen_             = mkrGen 6
                }

