{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses,
             TypeSynonymInstances #-}
module Gen.Reduce.Simpler where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Conjure.Language.Expression.Op
import Data.List                        (foldl1)
import Gen.Arbitrary.Type               (typesUnify)
import Gen.AST.TH
import Gen.Prelude

-- True if a1 is simpler then a2
class (Pretty a, Eq a, Show a, Pretty b, Eq b, Show b, Standardise a, Standardise b)
    => Simpler a b where
  simplerImp :: (HasLogger m) => a -> b -> m Ordering
  simpler  :: (HasLogger m) => a -> b -> m Ordering
  simpler1 :: (HasLogger m) => a -> b -> m Bool

  simpler a b = do
    -- addLog "simplerStart" [nn "a" a, nn "b" b]
    ea <- standardise a
    eb <- standardise b
    res <- simplerImp ea eb
    addLog "simpler" [nn "a" a, nn "b" b
                     , nn "res" (show res)
                     -- , nn "ga" (groom a), nn "gb" (groom b)
                     -- , nn "ga" (groom na), nn "gb" (groom nb)
                     ]
    return $ res

  simpler1 a b= simpler a b >>= (return . (== LT))


instance Simpler TType TType where
    simplerImp TBool TBool = return EQ
    simplerImp TBool _     = return LT
    simplerImp  _ TBool    = return GT

    simplerImp TInt TInt   = return  EQ
    simplerImp TInt _      = return  LT
    simplerImp  _  TInt    = return  GT

    -- simplerImp (TSet x)   (TSet y)   = simplerImp x y
    -- simplerImp (TMSet x)  (TMSet y)  = simplerImp x y
    -- simplerImp (TMatix x) (TMatix y) = simplerImp x y

    -- simplerImp (TSet x)   (TMSet y)  = simplerImp x y
    -- simplerImp (TSet x)   (TMatix y) = simplerImp x y

    -- simplerImp (TMSet x)  (TSet y)   = simplerImp x y
    -- simplerImp (TMSet x)  (TMatix y) = simplerImp x y

    -- simplerImp (TMatix x)  (TMSet y) = simplerImp x y
    -- simplerImp (TMatix x)  (TSet y)  = simplerImp x y

    -- simplerImp (TPar x)   (TPar y) = simplerImp x y

    -- simplerImp (TFunc x1 x2) (TFunc y1 y2) = do
    --     a <- simplerImp x1 x2
    --     b <- simplerImp y1 y2
    --     return $ compareCombine a b

    -- simplerImp (TTuple x) (TTuple y) = do
    --     res <- zipWithM simplerImp x y
    --     return $ chainCompare res

    -- simplerImp (TRel x) (TRel y) = do
    --     res <- zipWithM simplerImp x y
    --     return $ chainCompare res

    -- simplerImp a@(TUnamed _) b =  rrError "simplerImp"
    --                               [pretty $ a, pretty $  b
    --                               , pretty $ groom a, pretty $ groom b ]
    -- simplerImp a@(TEnum _) b = rrError "simplerImp"
    --                               [pretty $ a, pretty $  b
    --                               , pretty $ groom a, pretty $ groom b ]

    simplerImp TAny TAny  = return EQ
    simplerImp TAny _     = return LT
    simplerImp _ TAny     = return GT

    simplerImp x  y = return $ compare (depthOf x) (depthOf y)

instance Simpler Expr Expr where
  simplerImp (ELit a ) (ELit b) = simplerImp a b
  simplerImp (EOp a)  (EOp b)   = simplerImp a b
  -- simplerImp (EDom a)  (EDom b) = simplerImp a b


  simplerImp (EVar (Var _ tya)) (EVar (Var _ tyb)) = do
    simplerImp tya tyb

  simplerImp (ETyped _ x) (ETyped _ y) = simplerImp x y
  simplerImp (ETyped _ x) y            = simplerImp x y
  simplerImp x (ETyped _ y)            = simplerImp x y

  simplerImp EEmptyGuard EEmptyGuard = return EQ
  simplerImp EEmptyGuard b = do
    tyb <- ttypeOf b
    return $ if tyb == TBool then EQ else LT

  simplerImp (EOp a) b = simplerImp a b
  simplerImp b (EOp a) = simplerImp a b


  simplerImp (EQuan _ _ _ a3 a4) b   = simplerTu2 (a3,a4) b
  simplerImp b (EQuan _ _ _ a3 a4)   = negOrderM $ simplerTu2 (a3,a4) b

  simplerImp a@(EDom _) b = rrError "simplerImp"
                           [pretty $ a, pretty $ b
                           , pretty $ groom a, pretty $ groom b ]
  simplerImp a b@(EDom _) = rrError "simplerImp"
                            [pretty $ a, pretty $ b
                            , pretty $ groom a, pretty $ groom b ]

  simplerImp a b = do
      tya <- ttypeOf a
      tyb <- ttypeOf b
      simplerImp tya tyb


instance Simpler (Op Expr) (Op Expr) where

    simplerImp x y = return EQ

instance Simpler Constant Constant where
    simplerImp (ConstantBool _) (ConstantBool _) = return EQ
    simplerImp (ConstantBool _) _      = return LT
    simplerImp _     (ConstantBool _)  = return GT

    simplerImp (ConstantInt _) (ConstantInt _) = return EQ
    simplerImp (ConstantInt _) _      = return LT
    simplerImp _     (ConstantInt _)  = return GT

instance Simpler Literal Literal where

    simplerImp (AbsLitTuple x) (AbsLitTuple y) = do
        res <- zipWithM simplerImp x y
        return $ chainCompare res

    simplerImp (AbsLitMatrix _ x ) (AbsLitMatrix _ y) = do
      res <- zipWithM simplerImp x y

      let bo = case chainCompare res of
                  EQ -> compare (length x) (length y)
                  t  -> t

      return bo

    simplerImp a@(AbsLitSet x) b@(AbsLitSet y)             =  do
        addLog "simplerImp" [nn "a" a, nn "b" b]
        res <- zipWithM simplerImp x y
        return $ chainCompare res

    simplerImp (AbsLitMSet x) (AbsLitMSet y)           =  do
        res <- zipWithM simplerImp x y
        return $ chainCompare res


    simplerImp (AbsLitRelation x) (AbsLitRelation y)   =  do
        res <- zipWithM (\a b -> zipWithM simplerImp a b ) x y
        return $ chainCompare $ map chainCompare res

    simplerImp (AbsLitFunction x) (AbsLitFunction y)   = do
      cs <- zipWithM simplerImp x y
      return $ chainCompare cs

    simplerImp (AbsLitPartition x) (AbsLitPartition y) = do
        cs <- zipWithM ( \a b -> zipWithM simplerImp a b ) x y
        return $ chainCompare (concat cs)

    simplerImp a b = do
        tya <- ttypeOf a
        tyb <- ttypeOf b
        simplerImp tya tyb

simplerImpError :: (Simpler a b, HasLogger m) => a -> b -> m Ordering
simplerImpError a b = rrError "simplerImp"
                      [pretty $ a, pretty $  b
                      , pretty $ groom a, pretty $ groom b ]


instance Simpler Expr Literal where
    simplerImp EEmptyGuard b = do
      tyb <- ttypeOf b
      return $ if tyb == TBool then EQ else LT
    simplerImp (ELit e) l    = simplerImp e l

    simplerImp (EVar (Var _ tyE)) l = do
      tyl <- ttypeOf l
      simplerImp (tyE) tyl

    simplerImp (ETyped tyE e) l = do
      tyl <- ttypeOf l
      case typesUnify tyE tyl of
        True  -> simplerImp e l
        False -> simplerImp tyE tyl

    simplerImp (EOp e) l             = simplerImp e l
    simplerImp (EQuan _ _ _ e3 e4) l   = simplerTu2 (e3, e4) l

    simplerImp a@(EDom _) l            = simplerImpError a l


instance Simpler (Op Expr) Expr where simplerImp x y = negOrderM $ simplerImp y x
instance Simpler Expr (Op Expr) where
    simplerImp a b = do
        tya <- ttypeOf a
        tyb <- ttypeOf b
        simplerImp tya tyb



instance Simpler Literal (Op Expr) where
    simplerImp x y = negOrderM $ simplerImp y x
instance Simpler (Op Expr) Literal where
    simplerImp a b = do
        tya <- ttypeOf a
        tyb <- ttypeOf b
        simplerImp tya tyb

simplerTu2 :: forall (m :: * -> *) a a1 b.
              (Simpler a1 b, Simpler a b, HasLogger m) =>
              (a, a1) -> b -> m Ordering
simplerTu2 (a,b) c = do
    ac <- simplerImp a c
    bc <- simplerImp b c
    return $ compareChain [ac,bc]

simplerTu3 :: forall (m :: * -> *) a a1 a2 b.
              (Simpler a2 b, Simpler a1 b, Simpler a b, HasLogger m) =>
              (a, a1, a2) -> b -> m Ordering
simplerTu3 (a,b,c) x = do
    ax <- simplerImp a x
    bx <- simplerImp b x
    cx <- simplerImp c x
    return $ compareChain [ax,bx,cx]

instance (Simpler a c, Simpler b d) => Simpler (a,b) (c,d)  where
    simplerImp (x1, x2) (y1, y2)  = do
      r1 <- simplerImp x1 y1
      r2 <- simplerImp x2 y2
      return $ compareCombine r1 r2

instance (Simpler a c, Simpler b d, Simpler c e) => Simpler (a,b,c) (c,d,e)  where
    simplerImp (x1, x2, x3) (y1, y2, y3)  = do
      r1 <- simplerImp x1 y1
      r2 <- simplerImp x2 y2
      r3 <- simplerImp x3 y3
      return $ chainCompare [r1,r2,r3]

compareChain :: [Ordering] -> Ordering
compareChain (EQ:xs) = compareChain xs
compareChain (x :_ ) = x
compareChain []      = EQ

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

negOrderM :: Monad m => m Ordering -> m Ordering
negOrderM o = do
  oo <- o
  return . negOrder $ oo
