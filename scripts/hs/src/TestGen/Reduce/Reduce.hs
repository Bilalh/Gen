{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module TestGen.Reduce.Reduce where

import TestGen.Prelude
import qualified TestGen.Arbitrary.Arbitrary as A
import qualified TestGen.Arbitrary.Domain as A
import qualified TestGen.Arbitrary.Expr as A
import TestGen.Arbitrary.Data

import qualified Data.Map as M
import qualified Data.Text as T
import Data.List

import qualified Test.QuickCheck as QC

import TestGen.QCDebug(specE1)

import System.Random(randomIO)
import Control.Arrow((&&&))

class Reduce a where
    reduce :: a -> [a]
    single :: a -> [Expr]

instance Reduce Expr where
    reduce (EBinOp op) = single op ++ map EBinOp (reduce op) 
    reduce b = [] -- no reductions possible   

    single a = error "no default of single"

instance Reduce BinOp where
    reduce (BOr a b) = map ( uncurry BOr ) $  catMaybes
        [ (a, etrue) *| simpler etrue b , (a,efalse)  *| simpler efalse b
        , (etrue,b)  *| simpler etrue a , (efalse, b) *| simpler efalse a ]

    reduce b = []
    single b = [etrue,  efalse]

etrue  = ELit (EB True)
efalse = ELit (EB False)



reduceMain :: SpecE -> IO SpecE 
reduceMain s1 = do
    -- s1 <- removeConstraints sp
    s2 <- simplyConstraints s1
    
    sfin <- return s2 

    putStrLn "----"    
    putStrLn "Start"
    print . pretty $ s1
    putStrLn "----"    
    putStrLn "Final"
    print . pretty $ sfin
    putStrLn "----"    
    
    return sfin

    
simplyConstraints :: SpecE -> IO SpecE
simplyConstraints (SpecE ds es) = do
    fin <- process (map simplyConstraint es)
    if fin == [] then do 
        runSpec (SpecE ds []) >>= \case
            True  -> return (SpecE ds [])
            False -> return (SpecE ds es)
            
        return (SpecE ds es)
    else
        return (SpecE ds fin)
    
    where
    process :: [[Expr]] -> IO [Expr]
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
            inner <- process (map simplyConstraint fix )
            if inner == [] then
                return fix
            else 
                return inner
        else 
            process (map tailR esR)
    
    singleElem [x] = True
    singleElem _   = False
    
    tailR :: [a] -> [a]    
    tailR []     = error "tailR empty list"
    tailR [x]    = [x]
    tailR (_:xs) = xs
        
    choose :: [[Expr]] -> IO [Expr]
    choose esR = do
        mapM rndExpr esR
        
    rndExpr :: [Expr] -> IO Expr
    rndExpr [] = error "rndExpr emptyList"
    rndExpr xs = return (xs !! 0)

simplyConstraint :: Expr -> [Expr]
simplyConstraint e =  reduce e    
    
removeUnusedDomain :: SpecE -> IO SpecE    
removeUnusedDomain sp = undefined  

-- True means error still happens
runSpec :: SpecE -> IO Bool
runSpec sp = do
    stillErroed :: Bool <- randomIO 
    print $ (stillErroed, pretty sp)
    return stillErroed


_reduce e = 
    case  fromEssence e of 
        Left err -> error . show .  (pretty &&& pretty . groom)  $ err
        Right ee -> do
            let res = reduce ee
            mapM (print  . pretty . toEssence)  res
            return res


-- a r= a r +1
--
-- ts = [1, 2,3,a 4]
--
-- func tt =
--     let a = tt !! 0
--         b = tt !! 3
--         c = [b, a]
--     in c !! 1


-- True if a1 is less simpler then a2
class Simpler a b where
    simpler :: (ToEssence a, Eq a, ToEssence b, Eq b) => a -> b -> Bool

instance Simpler Expr Expr where
    simpler (ELit a ) (ELit b)   = simpler a b
    simpler (ELit a)  (EBinOp b) = simpler a b 
    
    -- simpler _ _ = False
    simpler a b = error . show . vcat . map (pretty)  $ [toEssence a, toEssence b ]

instance Simpler Literal Literal where
    simpler (EB _) (EB _) = False
    simpler (EB _) _      = True
    
    -- simpler _ _ = False
    simpler a b = error . show . vcat . map (pretty)  $ [toEssence a, toEssence b ]

instance Simpler Literal BinOp where
    simpler (EB _) _ = True
    simpler (EI _) _ = True

    -- simpler _ _ = False
    simpler a b = error . show . vcat . map (pretty)  $ [toEssence a, toEssence b ]
    

infixl 1 *|
(*|) :: a -> Bool -> Maybe a
a  *| c | c = Just a
_  *| _    = Nothing

 
_e :: FromEssence a => E -> a
_e e =  case fromEssence e of 
        Left err -> error . show .  (pretty &&& pretty . groom) $ err
        Right ee -> ee
