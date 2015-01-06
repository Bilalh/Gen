{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module TestGen.Reduce.Simpler where

import TestGen.Prelude
import TestGen.Prelude

import TestGen.Arbitrary.Type(typesUnify)

-- True if a1 is less simpler then a2
class (Pretty a, Eq a, Pretty b, Eq b) => Simpler a b where
    simpler :: (WithDoms m) => a -> b -> m Bool

instance Simpler Type Type where
    simpler TBool TBool = return False
    simpler TBool _     = return True

    simpler TInt TBool  = return False
    simpler TInt TInt   = return  False
    simpler TInt _      = return  True

    simpler (TSet x) (TSet y)     = simpler x y
    simpler (TMSet x) (TMSet y)   = simpler x y
    simpler (TMatix x) (TMatix y) = simpler x y
    simpler (TPar x) (TPar y)     = simpler x y

    simpler (TFunc x1 x2) (TFunc y1 y2) = do
        a <- simpler x1 x2
        b <- simpler y1 y2
        return $ a && b

    simpler (TTuple x) (TTuple y) = do
        res <- zipWithM simpler x y
        return $ and res

    simpler (TRel x) (TRel y) = do
        res <- zipWithM simpler x y
        return $ and res

    -- simpler (TUnamed x) y = _h
    -- simpler (TEnum x) y = _h
    simpler TAny TBool = return False
    simpler TAny TAny  = return False
    simpler TAny _     = return True

    simpler a b = error . show . vcat  $
                  ["simpler", pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]

instance Simpler Expr Expr where
    simpler (ELit a ) (ELit b)   = simpler a b
    simpler (ELit a)  (EBinOp b) = simpler a b

    simpler a@(EVar _) b = do
      tya <- ttypeOf a
      tyb <- ttypeOf b
      simpler tya tyb

    simpler a@(EQVar _) b = do
      tya <- ttypeOf a
      tyb <- ttypeOf b
      simpler tya tyb

    simpler (EBinOp a) (EBinOp b) = simpler a b

    -- simpler (EUniOp a) b =_h
    -- simpler (EProc a) b =_h
    -- simpler (EDom a) b =_h
    -- simpler (EQuan a1 a2 a3 a4) b =_h


    simpler EEmptyGuard EEmptyGuard = return False
    simpler EEmptyGuard b = do
      tyb <- ttypeOf b
      return $ tyb /= TBool


    -- simpler _ _ = False
    simpler a b = error . show . vcat  $
                  ["simpler", pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]


instance WithDoms m => TypeOf [Literal] m where
    ttypeOf []    = return TAny
    ttypeOf (x:_) = ttypeOf x

instance Simpler BinOp BinOp where
    -- simpler (BIn x1 x2) y        = _h
    -- simpler (BOver x1 x2) y      = _h

    simpler (BEQ x1 x2) (BEQ y1 y2)  = do
      r1 <- simpler x1 y1
      r2 <- simpler x2 y2
      return $ (r1 && r2 )
                 || (r1 && (x2 == y2) )
                 || (r2 && (x1 == y1) )

    simpler (BNEQ x1 x2) (BNEQ y1 y2) = do
      r1 <- simpler x1 y1
      r2 <- simpler x2 y2
      return $ (r1 && r2 )
                 || (r1 && (x2 == y2) )
                 || (r2 && (x1 == y1) )

    -- simpler (BLT x1 x2) y        = _h
    -- simpler (BLTE x1 x2) y       = _h
    -- simpler (BGT x1 x2) y        = _h
    -- simpler (BGTE x1 x2) y       = _h
    -- simpler (BDiff x1 x2) y      = _h
    -- simpler (BPlus x1 x2) y      = _h
    -- simpler (BMult x1 x2) y      = _h
    -- simpler (BDiv x1 x2) y       = _h
    -- simpler (BPow x1 x2) y       = _h
    -- simpler (BMod x1 x2) y       = _h
    -- simpler (BAnd x1 x2) y       = _h
    -- simpler (BOr x1 x2) y        = _h
    -- simpler (Bimply x1 x2) y     = _h
    -- simpler (Biff x1 x2) y       = _h
    -- simpler (Bsubset x1 x2) y    = _h
    -- simpler (BsubsetEq x1 x2) y  = _h
    -- simpler (Bsupset x1 x2) y    = _h
    -- simpler (BsupsetEq x1 x2) y  = _h
    -- simpler (Bintersect x1 x2) y = _h
    -- simpler (Bunion x1 x2) y     = _h
    -- simpler (BlexLT x1 x2) y     = _h
    -- simpler (BlexLTE x1 x2) y    = _h
    -- simpler (BlexGT x1 x2) y     = _h
    -- simpler (BlexGTE x1 x2) y    = _h


instance Simpler Literal Literal where
    simpler (EB _) (EB _) = return False
    simpler (EB _) _      = return True

    simpler (EI _) (EB _) = return False
    simpler (EI _) (EI _) = return False
    simpler (EI _) _      = return True

    simpler (EExpr x) (EExpr y) =  simpler x y

    simpler (ETuple x) (ETuple y) = do
        res <- zipWithM simpler x y
        return $ and res

    simpler (EMatrix x _) (EMatrix y _) = do
      tx <- ttypeOf x
      ty <- ttypeOf y
      res <- zipWithM simpler x y

      return $ (and res) || ( (typesUnify tx ty) && length x < length y )


    simpler (ESet x) (ESet y)             =  do
        res <- zipWithM simpler x y
        return $ and res

    simpler (EMSet x) (EMSet y)           =  do
        res <- zipWithM simpler x y
        return $ and res


    simpler (ERelation x) (ERelation y)   =  do
        res <- zipWithM simpler x y
        return $ and res

    -- simpler (EFunction x) (EFunction y)   =  and $ zipWith simpler x y
    -- simpler (EPartition x) (EPartition y) =  and $ zipWith simpler x y


    -- simpler _ _ = False
    simpler a b = error . show . vcat  $
                  ["simpler", pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]

instance Simpler Literal BinOp where
    simpler (EB _) _ = return True
    simpler (EI _) _ = return True

    -- simpler _ _ = False
    simpler a b = error . show . vcat  $
                  ["simpler", pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]
