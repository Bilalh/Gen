{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module TestGen.Reduce.Simpler where

import TestGen.Prelude

-- True if a1 is less simpler then a2
class (Pretty a, Eq a, Pretty b, Eq b) => Simpler a b where
    simpler :: a -> b -> Bool

instance Simpler Type Type where
    simpler TBool TBool = False
    simpler TBool _     = True

    simpler TInt TBool  = False
    simpler TInt TInt   = False
    simpler TInt _      = True

    simpler (TSet x) (TSet y)     = simpler x y
    simpler (TMSet x) (TMSet y)   = simpler x y
    simpler (TMatix x) (TMatix y) = simpler x y
    simpler (TPar x) (TPar y)           = simpler x y

    simpler (TFunc x1 x2) (TFunc y1 y2) = simpler x1 x2 && simpler y1 y2
    simpler (TTuple x) (TTuple y)       = and $ zipWith simpler x y
    simpler (TRel x) (TRel y)           = and $ zipWith simpler x y
    -- simpler (TUnamed x) y = _h
    -- simpler (TEnum x) y = _h
    -- simpler TAny _ = _h

    simpler a b = error . show . vcat  $
                  ["simpler", pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]

instance Simpler Expr Expr where
    simpler (ELit a ) (ELit b)   = simpler a b
    simpler (ELit a)  (EBinOp b) = simpler a b

    -- simpler _ _ = False
    simpler a b = error . show . vcat  $
                  ["simpler", pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]

instance Simpler Literal Literal where
    simpler (EB _) (EB _) = False
    simpler (EB _) _      = True

    simpler (EI _) (EB _) = False
    simpler (EI _) (EI _) = False
    simpler (EI _) _      = True

    simpler (EExpr x) (EExpr y)           =  simpler x y

    simpler (ETuple x) (ETuple y)         =  and $ zipWith simpler x y
    simpler (EMatrix x _) (EMatrix y _)   =  and $ zipWith simpler x y
    simpler (ESet x) (ESet y)             =  and $ zipWith simpler x y
    simpler (EMSet x) (EMSet y)           =  and $ zipWith simpler x y
    -- simpler (EFunction x) (EFunction y)   =  and $ zipWith simpler x y
    simpler (ERelation x) (ERelation y)   =  and $ zipWith simpler x y
    -- simpler (EPartition x) (EPartition y) =  and $ zipWith simpler x y


    -- simpler _ _ = False
    simpler a b = error . show . vcat  $
                  ["simpler", pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]

instance Simpler Literal BinOp where
    simpler (EB _) _ = True
    simpler (EI _) _ = True

    -- simpler _ _ = False
    simpler a b = error . show . vcat  $
                  ["simpler", pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]
