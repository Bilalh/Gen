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

import TestGen.Helpers.Runner

import System.Directory(createDirectoryIfMissing, removeDirectoryRecursive)

class Reduce a where
    reduce   :: a -> [a]    -- list of smaller exprs
    single   :: a -> [Expr] -- smallest literal e.g  [true, false] for  a /\ b
    subterms :: a -> [Expr] -- a /\ b  -->   [a, b]

    -- reduce a   = error "no default reduce"
    -- single a   = error "no default of single"
    -- subterms a = error "no default of subterms"

instance Reduce Expr where
    reduce (EBinOp op) = single op ++ subterms op ++ map EBinOp (reduce op) 
    
    reduce a   = [] -- no reductions possible   
    
    single a   = error "no single expr"
    subterms a = [] 
    

instance Reduce BinOp where
    reduce (BOr a b) = map ( uncurry BOr ) $  catMaybes
        [ (a, etrue) *| simpler etrue b , (a,efalse)  *| simpler efalse b
        , (etrue,b)  *| simpler etrue a , (efalse, b) *| simpler efalse a ]

    reduce b = []
    
    single (BOr _ _) = [etrue,  efalse]
    single (BEQ _ _) = [etrue,  efalse]
    
    single a = error . show . vcat   
        $ ["single missing case", pretty $ toEssence a, pretty $ groom a ]
    

    subterms (BOr a b) = [a,b] 
    subterms (BEQ a b) = [a,b] 
    

    subterms a = error . show . vcat   
        $ ["subterms missing case", pretty $ toEssence a, pretty $ groom a ]

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
runSpec spE = do
    let sp = toSpec spE
    let path = "/Users/bilalh/CS/break_conjure/fixed/46c3d2b43f4e/2014-12-10_02-01_1418176894/_errors/Validate_/ErrorUnknown_/1418178864_89/out"
    
    -- removeDirectoryRecursive breaks if dir eixsts
    createDirectoryIfMissing True path >> removeDirectoryRecursive path
    
    res <- runToolchain' 33 4 sp (path) 120 True
    let stillErroed  = sameError res
     
    print $ (stillErroed, pretty sp)
    return stillErroed

    where 
    -- This would be a different error
    sameError (Left SettingI{successful_=False}) = False
    sameError (Right (_, SettingI{successful_=False,data_=SolveM ms })) = 
        
        let
            f ResultI{last_status, erroed= Just index, results } = 
                let kind = kind_ (results !! index)
                in kind
        
            kinds = M.toList $  M.map f ms
        in any (\(name,kind) -> kind == Validate_) kinds
 
    sameError _ = False

-- True if a1 is less simpler then a2
class Simpler a b where
    simpler :: (ToEssence a, Eq a, ToEssence b, Eq b) => a -> b -> Bool

-- instance Simpler a b where
--     simpler _ _ = True

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


_reduce e = 
    case  fromEssence e of 
        Left err -> error . show .  (pretty &&& pretty . groom)  $ err
        Right ee -> do
            let res = reduce ee
            mapM (print  . pretty . toEssence)  res
            return res
 
_e :: FromEssence a => E -> a
_e e =  case fromEssence e of 
        Left err -> error . show .  (pretty &&& pretty . groom) $ err
        Right ee -> ee
