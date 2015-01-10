{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module TestGen.Reduce.Simpler where

import TestGen.Prelude

import TestGen.Arbitrary.Type(typesUnify)


-- True if a1 is simpler then a2
class (Pretty a, Eq a, Show a, Pretty b, Eq b, Show b, Standardise a, Standardise b) => Simpler a b where
    simplerImp :: (WithDoms m, HasLogger m) => a -> b -> m Bool
    simpler :: (WithDoms m, HasLogger m) => a -> b -> m Bool

    simpler a b = do
      -- addLog "simplerStart" [nn "a" a, nn "b" b]
      na <- standardise a
      nb <- standardise b
      res <- simplerImp na nb
      addLog "simpler" [nn "a" a, nn "b" b
                       , nn "res" res
                       -- , nn "ga" (groom a), nn "gb" (groom b)
                       -- , nn "ga" (groom na), nn "gb" (groom nb)
                       ]
      return res

instance Simpler Type Type where
    simplerImp TBool TBool = return False
    simplerImp TBool _     = return True

    simplerImp TInt TBool  = return False
    simplerImp TInt TInt   = return  False
    simplerImp TInt _      = return  True

    simplerImp (TSet x) (TSet y)     = simpler x y
    simplerImp (TMSet x) (TMSet y)   = simpler x y
    simplerImp (TMatix x) (TMatix y) = simpler x y
    simplerImp (TPar x) (TPar y)     = simpler x y

    simplerImp (TFunc x1 x2) (TFunc y1 y2) = do
        a <- simplerImp x1 x2
        b <- simplerImp y1 y2
        return $ a && b

    simplerImp (TTuple x) (TTuple y) = do
        res <- zipWithM simplerImp x y
        return $ and res

    simplerImp (TRel x) (TRel y) = do
        res <- zipWithM simplerImp x y
        return $ and res

    -- simplerImp (TUnamed x) y = _h
    -- simplerImp (TEnum x) y = _h
    simplerImp TAny TBool = return False
    simplerImp TAny TAny  = return False
    simplerImp TAny _     = return True

    simplerImp a b = rrError "simpler"
                  [pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]

instance Simpler Expr Expr where
  simplerImp (ELit a ) (ELit b)   = simpler a b
  simplerImp (ELit a)  (EBinOp b) = simpler a b

  simplerImp a@(EVar _) b = do
    tya <- ttypeOf a
    tyb <- ttypeOf b
    simplerImp tya tyb

  simplerImp a@(EQVar _) b = do
    tya <- ttypeOf a
    tyb <- ttypeOf b
    simplerImp tya tyb

  simplerImp (EBinOp a) (EBinOp b) = simpler a b

  -- simplerImp (EUniOp a) b =_h
  -- simplerImp (EProc a) b =_h
  -- simplerImp (EDom a) b =_h
  -- simplerImp (EQuan a1 a2 a3 a4) b =_h


  simplerImp EEmptyGuard EEmptyGuard = return False
  simplerImp EEmptyGuard b = do
    tyb <- ttypeOf b
    return $ tyb /= TBool


  simplerImp (ETyped _ x) (ETyped _ y) = simpler x y
  simplerImp x (ETyped _ y)            = simpler x y
  simplerImp (ETyped _ x) y            = simpler x y

  simplerImp a b = rrError "simpler"
                [pretty $ a, pretty $  b
                , pretty $ groom a, pretty $ groom b ]

instance Simpler BinOp BinOp where
    -- simplerImp (BIn x1 x2) y        = _h
    -- simplerImp (BOver x1 x2) y      = _h

    simplerImp (BEQ x1 x2) (BEQ y1 y2)  = do
      r1 <- simplerImp x1 y1
      r2 <- simplerImp x2 y2
      return $ (r1 && r2 )
                 || (r1 && (x2 == y2) )
                 || (r2 && (x1 == y1) )

    simplerImp (BNEQ x1 x2) (BNEQ y1 y2) = do
      r1 <- simplerImp x1 y1
      r2 <- simplerImp x2 y2
      return $ (r1 && r2 )
                 || (r1 && (x2 == y2) )
                 || (r2 && (x1 == y1) )

    -- simplerImp (BLT x1 x2) y        = _h
    -- simplerImp (BLTE x1 x2) y       = _h
    -- simplerImp (BGT x1 x2) y        = _h
    -- simplerImp (BGTE x1 x2) y       = _h
    -- simplerImp (BDiff x1 x2) y      = _h
    -- simplerImp (BPlus x1 x2) y      = _h
    -- simplerImp (BMult x1 x2) y      = _h
    -- simplerImp (BDiv x1 x2) y       = _h
    -- simplerImp (BPow x1 x2) y       = _h
    -- simplerImp (BMod x1 x2) y       = _h
    -- simplerImp (BAnd x1 x2) y       = _h
    -- simplerImp (BOr x1 x2) y        = _h
    -- simplerImp (Bimply x1 x2) y     = _h
    -- simplerImp (Biff x1 x2) y       = _h
    -- simplerImp (Bsubset x1 x2) y    = _h
    -- simplerImp (BsubsetEq x1 x2) y  = _h
    -- simplerImp (Bsupset x1 x2) y    = _h
    -- simplerImp (BsupsetEq x1 x2) y  = _h
    -- simplerImp (Bintersect x1 x2) y = _h
    -- simplerImp (Bunion x1 x2) y     = _h
    -- simplerImp (BlexLT x1 x2) y     = _h
    -- simplerImp (BlexLTE x1 x2) y    = _h
    -- simplerImp (BlexGT x1 x2) y     = _h
    -- simplerImp (BlexGTE x1 x2) y    = _h

instance Simpler Literal Literal where
    simplerImp (EB _) (EB _) = return False
    simplerImp (EB _) _      = return True

    simplerImp (EI _) (EB _) = return False
    simplerImp (EI _) (EI _) = return False
    simplerImp (EI _) _      = return True

    simplerImp x (EExpr (ELit y)) =  simpler x y

    simplerImp (EExpr x) (EExpr y) =  simpler x y

    simplerImp (ETuple x) (ETuple y) = do
        res <- zipWithM simplerImp x y
        return $ and res

    simplerImp a@(EMatrix x _) b@(EMatrix y _) = do
      tx <- ttypeOf x
      ty <- ttypeOf y
      res <- zipWithM simplerImp x y

      let bo = (and res) || ( (typesUnify tx ty) && length x < length y )
      return bo

    simplerImp a@(ESet x) b@(ESet y)             =  do
        addLog "simplerImp" [nn "a" a, nn "b" b]
        res <- zipWithM simplerImp x y
        return $ and res

    simplerImp (EMSet x) (EMSet y)           =  do
        res <- zipWithM simplerImp x y
        return $ and res


    simplerImp (ERelation x) (ERelation y)   =  do
        res <- zipWithM simplerImp x y
        return $ and res

    -- simplerImp (EFunction x) (EFunction y)   =  and $ zipWith simpler x y
    -- simplerImp (EPartition x) (EPartition y) =  and $ zipWith simpler x y


    -- simplerImp _ _ = False
    simplerImp a b = rrError "simpler Missing case Literal"
                  [pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]

instance Simpler Literal BinOp where
    simplerImp (EB _) _ = return True
    simplerImp (EI _) _ = return True

    -- simplerImp _ _ = False
    simplerImp a b = rrError "simpler"
                  [pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]
