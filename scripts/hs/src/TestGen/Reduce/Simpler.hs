{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module TestGen.Reduce.Simpler where

import TestGen.Prelude

import TestGen.Arbitrary.Type(typesUnify)


-- True if a1 is simpler then a2
class (Pretty a, Eq a, Show a, Pretty b, Eq b, Show b, Standardise a, Standardise b)
    => Simpler a b where
  simplerImp :: (WithDoms m, HasLogger m) => a -> b -> m Ordering
  simpler  :: (WithDoms m, HasLogger m) => a -> b -> m Ordering
  simpler1 :: (WithDoms m, HasLogger m) => a -> b -> m Bool

  simpler a b = do
    -- addLog "simplerStart" [nn "a" a, nn "b" b]
    na <- standardise a
    nb <- standardise b
    res <- simplerImp na nb
    addLog "simpler" [nn "a" a, nn "b" b
                     , nn "res" (show res)
                     -- , nn "ga" (groom a), nn "gb" (groom b)
                     -- , nn "ga" (groom na), nn "gb" (groom nb)
                     ]
    return $ res

  simpler1 a b= simpler a b >>= (return . (== LT))

compareCombine :: Ordering -> Ordering -> Ordering
compareCombine LT LT = LT
compareCombine LT EQ = LT
compareCombine LT GT = EQ
compareCombine EQ LT = LT
compareCombine EQ EQ = EQ
compareCombine EQ GT = GT
compareCombine GT LT = EQ
compareCombine GT EQ = GT
compareCombine GT GT = GT

chainCompare :: [Ordering] -> Ordering
chainCompare []  = EQ
chainCompare [x] = x
chainCompare xs  = foldl1 compareCombine  xs


negOrder :: Ordering -> Ordering
negOrder LT = GT
negOrder EQ = EQ
negOrder GT = LT

instance Simpler Type Type where
    simplerImp TBool TBool = return EQ
    simplerImp TBool _     = return LT

    simplerImp TInt TBool  = return  GT
    simplerImp TInt TInt   = return  EQ
    simplerImp TInt _      = return  LT

    simplerImp (TSet x) (TSet y)     = simpler x y
    simplerImp (TMSet x) (TMSet y)   = simpler x y
    simplerImp (TMatix x) (TMatix y) = simpler x y
    simplerImp (TPar x) (TPar y)     = simpler x y

    simplerImp (TFunc x1 x2) (TFunc y1 y2) = do
        a <- simplerImp x1 x2
        b <- simplerImp y1 y2
        return $ compareCombine a b

    simplerImp (TTuple x) (TTuple y) = do
        res <- zipWithM simplerImp x y
        return $ chainCompare res

    simplerImp (TRel x) (TRel y) = do
        res <- zipWithM simplerImp x y
        return $ chainCompare res

    -- simplerImp (TUnamed x) y = _h
    -- simplerImp (TEnum x) y = _h
    simplerImp TAny TBool = return GT
    simplerImp TAny TAny  = return EQ
    simplerImp TAny _     = return LT

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


  simplerImp EEmptyGuard EEmptyGuard = return EQ
  simplerImp EEmptyGuard b = do
    tyb <- ttypeOf b
    return $ if tyb == TBool then EQ else LT


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
      return $ compareCombine r1 r2

    simplerImp (BNEQ x1 x2) (BNEQ y1 y2) = do
      r1 <- simplerImp x1 y1
      r2 <- simplerImp x2 y2
      return $ compareCombine r1 r2


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
    simplerImp (EB _) (EB _) = return EQ
    simplerImp (EB _) _      = return LT

    simplerImp (EI _) (EB _) = return GT
    simplerImp (EI _) (EI _) = return EQ
    simplerImp (EI _) _      = return LT

    simplerImp x (EExpr (ELit y)) =  simpler x y

    simplerImp (EExpr x) (EExpr y) =  simpler x y

    simplerImp (EExpr x) l = simpler x l
    simplerImp l (EExpr e) = do
      res <- simpler e l
      return $  res

    simplerImp (ETuple x) (ETuple y) = do
        res <- zipWithM simplerImp x y
        return $ chainCompare res

    simplerImp (EMatrix x _) (EMatrix y _) = do
      tx <- ttypeOf x
      ty <- ttypeOf y
      res <- zipWithM simplerImp x y

      let bo = case chainCompare res of
                  EQ -> case (typesUnify tx ty) of
                          True  -> compare (length x) (length y)
                          False -> EQ
                  t  -> t

      return bo

    simplerImp a@(ESet x) b@(ESet y)             =  do
        addLog "simplerImp" [nn "a" a, nn "b" b]
        res <- zipWithM simplerImp x y
        return $ chainCompare res

    simplerImp (EMSet x) (EMSet y)           =  do
        res <- zipWithM simplerImp x y
        return $ chainCompare res


    simplerImp (ERelation x) (ERelation y)   =  do
        res <- zipWithM simplerImp x y
        return $ chainCompare res

    simplerImp a@(EFunction x) b@(EFunction y)   = simplerImpError a b
    simplerImp a@(EPartition x) b@(EPartition y) =  simplerImpError a b


    simplerImp a b = rrError "simpler Missing case Literal"
                  [pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]


instance Simpler Literal BinOp where
    simplerImp (EB _) _ = return LT
    simplerImp (EI _) _ = return LT

    simplerImp a b = simplerImpError a b



instance Simpler Expr Literal where
    simplerImp EEmptyGuard b = do
      tyb <- ttypeOf b
      return $ if tyb == TBool then EQ else LT
    simplerImp (ELit e) l    = simplerImp e l

    simplerImp (EVar e) l = do
      tyE :: Type <- typeOfVar e >>= \case
             Just ty -> return ty
             Nothing -> rrError "simpler no type of var" [nn "var" e]
      tyl <- ttypeOf l
      simplerImp (tyE) tyl

    simplerImp (EQVar e) l = do
      tyE :: Type <- typeOfVar e >>= \case
             Just ty -> return ty
             Nothing -> rrError "simpler no type of var" [nn "var" e]
      tyl <- ttypeOf l
      simplerImp (tyE) tyl

    simplerImp (ETyped tyE e) l = do
      tyl <- ttypeOf l
      case typesUnify tyE tyl of
        True  -> simplerImp e l
        False -> simplerImp tyE tyl

    simplerImp a@(EBinOp e) l          = simplerImpError a l
    simplerImp a@(EUniOp e) l          = simplerImpError a l
    simplerImp a@(EProc e) l           = simplerImpError a l
    simplerImp a@(EQuan e1 e2 e3 e4) l = simplerImpError a l
    simplerImp a@(EDom e) l            = simplerImpError a l

simplerImpError :: (Simpler a b, HasLogger m) => a -> b -> m Ordering
simplerImpError a b = rrError "simplerImp"
                      [pretty $ a, pretty $  b
                      , pretty $ groom a, pretty $ groom b ]
