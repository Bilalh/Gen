{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestGen.Reduce.Reduction(Reduce(..), runReduce) where

import TestGen.Reduce.Data
import TestGen.Reduce.Simpler
 
import TestGen.Prelude

-- import qualified TestGen.Arbitrary.Arbitrary as A
-- import qualified TestGen.Arbitrary.Domain as A
-- import qualified TestGen.Arbitrary.Expr as A

-- import qualified Data.Map as M
-- import qualified Test.QuickCheck as QC


class (HasGen m, WithDoms m) => Reduce a m where
    reduce   :: a -> m [a]    -- list of smaller exprs
    single   :: a -> m [Expr] -- smallest literal e.g  [true, false] for  a /\ b
    subterms :: a -> m [Expr] -- a /\ b  -->   [a, b]

    -- reduce a   = error "no default reduce"
    -- single a   = error "no default of single"
    -- subterms a = error "no default of subterms"

instance (HasGen m, WithDoms m) =>  Reduce Expr m where
    reduce (EBinOp op) = do
      -- single op ++ subterms op ++ map EBinOp (reduce op) 
      a1 <- single op
      a2 <- subterms op
      a3 <- reduce op 
      return $ a1 ++ a2 ++ (map EBinOp a3)
      
    
    reduce _   = return [] -- no reductions possible   
    single _   = error "no single expr"
    subterms _ = return [] 
    

instance (HasGen m, WithDoms m) =>  Reduce BinOp m where
    reduce (BOr x1 x2)    = return $ reduceBoolBinOP BOr x1 x2
    reduce (BAnd x1 x2)   = return $ reduceBoolBinOP BAnd x1 x2
    reduce (Bimply x1 x2) = return $ reduceBoolBinOP Bimply x1 x2
    reduce (Biff x1 x2)   = return $ reduceBoolBinOP Biff x1 x2

    -- reduce (BIn x1 x2) = _h
    -- reduce (BOver x1 x2) = _h
    -- reduce (BEQ x1 x2) = _h
    -- reduce (BNEQ x1 x2) = _h
    -- reduce (BLT x1 x2) = _h
    -- reduce (BLTE x1 x2) = _h
    -- reduce (BGT x1 x2) = _h
    -- reduce (BGTE x1 x2) = _h
    -- reduce (BDiff x1 x2) = _h
    -- reduce (BPlus x1 x2) = _h
    -- reduce (BMult x1 x2) = _h
    -- reduce (BDiv x1 x2) = _h
    -- reduce (BPow x1 x2) = _h
    -- reduce (BMod x1 x2) = _h
    -- reduce (Bsubset x1 x2) = _h
    -- reduce (BsubsetEq x1 x2) = _h
    -- reduce (Bsupset x1 x2) = _h
    -- reduce (BsupsetEq x1 x2) = _h
    -- reduce (Bintersect x1 x2) = _h
    -- reduce (Bunion x1 x2) = _h
    -- reduce (BlexLT x1 x2) = _h
    -- reduce (BlexLTE x1 x2) = _h
    -- reduce (BlexGT x1 x2) = _h
    -- reduce (BlexGTE x1 x2) = _h

    reduce _ = return $ []
    -- reduce a = error . show . vcat   
    --     $ ["reduce missing case", pretty $ toEssence a, pretty $ groom a ]
    
    single (BAnd _ _)   = return $ [etrue,  efalse]
    single (BOr _ _)    = return $ [etrue,  efalse]
    single (Bimply _ _) = return $ [etrue,  efalse]
    single (Biff _ _)   = return $ [etrue,  efalse]

    single (BEQ x1 x2)  = singleEq x1 x2
    single (BNEQ x1 x2) = singleEq x1 x2

    -- single (BIn x1 x2) = _e
    -- single (BOver x1 x2) = _e
    -- single (BLT x1 x2) = _e
    -- single (BLTE x1 x2) = _e
    -- single (BGT x1 x2) = _e
    -- single (BGTE x1 x2) = _e
    -- single (BDiff x1 x2) = _e
    -- single (BPlus x1 x2) = _e
    -- single (BMult x1 x2) = _e
    -- single (BDiv x1 x2) = _e
    -- single (BPow x1 x2) = _e
    -- single (BMod x1 x2) = _e
    -- single (Bsubset x1 x2) = _e
    -- single (BsubsetEq x1 x2) = _e
    -- single (Bsupset x1 x2) = _e
    -- single (BsupsetEq x1 x2) = _e
    -- single (Bintersect x1 x2) = _e
    -- single (Bunion x1 x2) = _e
    -- single (BlexLT x1 x2) = _e
    -- single (BlexLTE x1 x2) = _e
    -- single (BlexGT x1 x2) = _e
    -- single (BlexGTE x1 x2) = _e
    
    single a = error . show . vcat   
        $ ["single missing case", pretty $ toEssence a, pretty $ groom a ]


    subterms (BAnd x1 x2)   = return [x1, x2]
    subterms (BOr x1 x2)    = return [x1, x2]
    subterms (Bimply x1 x2) = return [x1, x2]
    subterms (Biff x1 x2)   = return [x1, x2]
 

    -- subterms (BIn x1 x2) = _k
    -- subterms (BOver x1 x2) = _k
    -- subterms (BEQ x1 x2) = _k
    -- subterms (BNEQ x1 x2) = _k
    -- subterms (BLT x1 x2) = _k
    -- subterms (BLTE x1 x2) = _k
    -- subterms (BGT x1 x2) = _k
    -- subterms (BGTE x1 x2) = _k
    -- subterms (BDiff x1 x2) = _k
    -- subterms (BPlus x1 x2) = _k
    -- subterms (BMult x1 x2) = _k
    -- subterms (BDiv x1 x2) = _k
    -- subterms (BPow x1 x2) = _k
    -- subterms (BMod x1 x2) = _k
    -- subterms (Bsubset x1 x2) = _k
    -- subterms (BsubsetEq x1 x2) = _k
    -- subterms (Bsupset x1 x2) = _k
    -- subterms (BsupsetEq x1 x2) = _k
    -- subterms (Bintersect x1 x2) = _k
    -- subterms (Bunion x1 x2) = _k
    -- subterms (BlexLT x1 x2) = _k
    -- subterms (BlexLTE x1 x2) = _k
    -- subterms (BlexGT x1 x2) = _k
    -- subterms (BlexGTE x1 x2) = _k
    
    subterms a = error . show . vcat   
        $ ["subterms missing case", pretty $ toEssence a, pretty $ groom a ]


reduceBoolBinOP :: (Expr -> Expr -> b) -> Expr -> Expr -> [b]
reduceBoolBinOP t a b= map ( uncurry t ) $  catMaybes
        [ (a, etrue) *| simpler etrue b , (a,efalse)  *| simpler efalse b
        , (etrue,b)  *| simpler etrue a , (efalse, b) *| simpler efalse a ]


singleEq :: (HasGen m, WithDoms m) => Expr -> Expr -> m [b]
singleEq a b = undefined 


-- return the simplest literals
singleLit :: Type -> Expr
singleLit = undefined
-- singleLit TInt = [-2, 10]
-- singleLit TBool = _x
-- singleLit (TMatix x) = _x
-- singleLit (TSet x) = _x
-- singleLit (TMSet x) = _x
-- singleLit (TFunc x1 x2) = _x
-- singleLit (TTuple x) = _x
-- singleLit (TRel x) = _x
-- singleLit (TPar x) = _x
-- singleLit (TUnamed x) = _x
-- singleLit (TEnum x) = _x
-- singleLit TAny = _x


runReduce spe x = do
  state <- newEState spe
  return $ runIdentity $ flip evalStateT state $ reduce x 
