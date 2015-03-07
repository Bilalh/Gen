{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}
module Gen.Reduce.Simpler where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Data.List                        (foldl1)
import Gen.Arbitrary.Type               (typesUnify)
import Gen.Prelude


-- True if a1 is simpler then a2
class (Pretty a, Eq a, Show a, Pretty b, Eq b, Show b, Standardise a, Standardise b)
    => Simpler a b where
  simplerImp :: (WithDoms m, HasLogger m) => a -> b -> m Ordering
  simpler  :: (WithDoms m, HasLogger m) => a -> b -> m Ordering
  simpler1 :: (WithDoms m, HasLogger m) => a -> b -> m Bool

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

    simplerImp (TSet x)   (TSet y)   = simplerImp x y
    simplerImp (TMSet x)  (TMSet y)  = simplerImp x y
    simplerImp (TMatix x) (TMatix y) = simplerImp x y

    simplerImp (TSet x)   (TMSet y)  = simplerImp x y
    simplerImp (TSet x)   (TMatix y) = simplerImp x y

    simplerImp (TMSet x)  (TSet y)   = simplerImp x y
    simplerImp (TMSet x)  (TMatix y) = simplerImp x y

    simplerImp (TMatix x)  (TMSet y) = simplerImp x y
    simplerImp (TMatix x)  (TSet y)  = simplerImp x y

    simplerImp (TPar x)   (TPar y) = simplerImp x y

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

    simplerImp a@(TUnamed _) b =  rrError "simplerImp"
                                  [pretty $ a, pretty $  b
                                  , pretty $ groom a, pretty $ groom b ]
    simplerImp a@(TEnum _) b = rrError "simplerImp"
                                  [pretty $ a, pretty $  b
                                  , pretty $ groom a, pretty $ groom b ]

    simplerImp TAny TAny  = return EQ
    simplerImp TAny _     = return LT
    simplerImp _ TAny     = return GT

    simplerImp x  y = return $ compare (depthOf x) (depthOf y)

instance Simpler Expr Expr where
  simplerImp (ELit a ) (ELit b)    = simplerImp a b
  simplerImp (EUniOp a) (EUniOp b) = simplerImp a b
  simplerImp (EBinOp a) (EBinOp b) = simplerImp a b
  simplerImp (EProc a)  (EProc b)  = simplerImp a b
  -- simplerImp (EDom a)  (EDom b) = simplerImp a b


  simplerImp (EVar _ tya) (EVar _ tyb) = do
    simplerImp tya tyb

  simplerImp (ETyped _ x) (ETyped _ y) = simplerImp x y
  simplerImp (ETyped _ x) y            = simplerImp x y
  simplerImp x (ETyped _ y)            = simplerImp x y

  simplerImp EEmptyGuard EEmptyGuard = return EQ
  simplerImp EEmptyGuard b = do
    tyb <- ttypeOf b
    return $ if tyb == TBool then EQ else LT

  simplerImp (EUniOp a) b = simplerImp a b
  simplerImp b (EUniOp a) = simplerImp a b

  simplerImp (EBinOp a) b = simplerImp a b
  simplerImp b (EBinOp a) = simplerImp a b

  simplerImp (EProc a) b  = simplerImp a b
  simplerImp b (EProc a)  = simplerImp a b

  simplerImp (EQuan _ _ a3 a4) b   = simplerTu2 (a3,a4) b
  simplerImp b (EQuan _ _ a3 a4)   = negOrderM $ simplerTu2 (a3,a4) b

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


instance Simpler UniOp UniOp where
    simplerImp (UBar f) (UBar t) = simplerImp f t
    simplerImp (UBar f) (UNeg t) = simplerImp f t
    simplerImp (UNeg f) (UBar t) = simplerImp f t
    simplerImp (UNeg f) (UNeg t) = simplerImp f t


instance Simpler BinOp BinOp where
    simplerImp x@(BIn _ _) y   = simplerImpError x y
    simplerImp x@(BOver _ _) y = simplerImpError x y

    simplerImp (BEQ x1 x2) y        = simplerTu2 (x1,x2) y
    simplerImp (BNEQ x1 x2) y       = simplerTu2 (x1,x2) y
    simplerImp (BLT x1 x2) y        = simplerTu2 (x1,x2) y
    simplerImp (BLTE x1 x2) y       = simplerTu2 (x1,x2) y
    simplerImp (BGT x1 x2) y        = simplerTu2 (x1,x2) y
    simplerImp (BGTE x1 x2) y       = simplerTu2 (x1,x2) y
    simplerImp (BDiff x1 x2) y      = simplerTu2 (x1,x2) y
    simplerImp (BPlus x1 x2) y      = simplerTu2 (x1,x2) y
    simplerImp (BMult x1 x2) y      = simplerTu2 (x1,x2) y
    simplerImp (BDiv x1 x2) y       = simplerTu2 (x1,x2) y
    simplerImp (BPow x1 x2) y       = simplerTu2 (x1,x2) y
    simplerImp (BMod x1 x2) y       = simplerTu2 (x1,x2) y
    simplerImp (BAnd x1 x2) y       = simplerTu2 (x1,x2) y
    simplerImp (BOr x1 x2) y        = simplerTu2 (x1,x2) y
    simplerImp (Bimply x1 x2) y     = simplerTu2 (x1,x2) y
    simplerImp (Biff x1 x2) y       = simplerTu2 (x1,x2) y
    simplerImp (Bsubset x1 x2) y    = simplerTu2 (x1,x2) y
    simplerImp (BsubsetEq x1 x2) y  = simplerTu2 (x1,x2) y
    simplerImp (Bsupset x1 x2) y    = simplerTu2 (x1,x2) y
    simplerImp (BsupsetEq x1 x2) y  = simplerTu2 (x1,x2) y
    simplerImp (Bintersect x1 x2) y = simplerTu2 (x1,x2) y
    simplerImp (Bunion x1 x2) y     = simplerTu2 (x1,x2) y
    simplerImp (BlexLT x1 x2) y     = simplerTu2 (x1,x2) y
    simplerImp (BlexLTE x1 x2) y    = simplerTu2 (x1,x2) y
    simplerImp (BlexGT x1 x2) y     = simplerTu2 (x1,x2) y
    simplerImp (BlexGTE x1 x2) y    = simplerTu2 (x1,x2) y

instance Simpler Proc Proc where
    simplerImp (PallDiff p) q         = simplerImp p q
    simplerImp (Pindex p1 p2) q       = simplerTu2 (p1, p2) q
    simplerImp (Pfreq p1 p2) q        = simplerTu2 (p1, p2) q
    simplerImp (Phist p1 p2) q        = simplerTu2 (p1, p2) q
    simplerImp (Pmax p) q             = simplerImp p q
    simplerImp (Pmin p) q             = simplerImp p q
    simplerImp (PtoInt p) q           = simplerImp p q
    simplerImp (PtoMSet p) q          = simplerImp p q
    simplerImp (PtoRelation p) q      = simplerImp p q
    simplerImp (PtoSet p) q           = simplerImp p q
    simplerImp (Pdefined p) q         = simplerImp p q
    simplerImp (Pimage p1 p2) q       = simplerTu2 (p1, p2) q
    simplerImp (Pinverse p1 p2) q     = simplerTu2 (p1, p2) q
    simplerImp (PpreImage p1 p2) q    = simplerTu2 (p1, p2) q
    simplerImp (Prange p) q           = simplerImp p q
    simplerImp (Pparts p) q           = simplerImp p q
    simplerImp (Pparty p1 p2) q       = simplerTu2 (p1, p2) q
    simplerImp (Pparticipants p) q    = simplerImp p q
    simplerImp (Papart p1 p2 p3) q    = simplerTu3 (p1, p2, p3) q
    simplerImp (Ptogether p1 p2 p3) q = simplerTu3 (p1, p2, p3) q
    simplerImp (Papply p1 p2) q       = do
        a <- simplerImp p1 q
        bs  <- mapM (\x -> simplerImp x q) p2
        return $ chainCompare (a:bs)

instance Simpler Constant Constant where
    simplerImp (ConstantBool _) (ConstantBool _) = return EQ
    simplerImp (ConstantBool _) _      = return LT
    simplerImp _     (ConstantBool _)  = return GT

    simplerImp (ConstantInt _) (ConstantInt _) = return EQ
    simplerImp (ConstantInt _) _      = return LT
    simplerImp _     (ConstantInt _)  = return GT

instance Simpler (AbstractLiteral Expr) (AbstractLiteral Expr) where

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


instance Simpler Expr (AbstractLiteral Expr) where
    simplerImp EEmptyGuard b = do
      tyb <- ttypeOf b
      return $ if tyb == TBool then EQ else LT
    simplerImp (ELit e) l    = simplerImp e l

    simplerImp (EVar _ tyE) l = do
      tyl <- ttypeOf l
      simplerImp (tyE) tyl

    simplerImp (ETyped tyE e) l = do
      tyl <- ttypeOf l
      case typesUnify tyE tyl of
        True  -> simplerImp e l
        False -> simplerImp tyE tyl

    simplerImp (EBinOp e) l          = simplerImp e l
    simplerImp (EUniOp e) l          = simplerImp e l
    simplerImp (EProc e) l           = simplerImp e l
    simplerImp (EQuan _ _ e3 e4) l   = simplerTu2 (e3, e4) l

    simplerImp a@(EDom _) l            = simplerImpError a l


instance Simpler UniOp Expr where simplerImp x y = negOrderM $ simplerImp y x
instance Simpler Expr UniOp where
    simplerImp a b = do
        tya <- ttypeOf a
        tyb <- ttypeOf b
        simplerImp tya tyb

instance Simpler BinOp Expr where simplerImp x y = negOrderM $ simplerImp y x
instance Simpler Expr BinOp where
    simplerImp a b = do
        tya <- ttypeOf a
        tyb <- ttypeOf b
        simplerImp tya tyb

instance Simpler Proc Expr where simplerImp x y = negOrderM $ simplerImp y x
instance Simpler Expr Proc where
    simplerImp a b = do
        tya <- ttypeOf a
        tyb <- ttypeOf b
        simplerImp tya tyb

instance Simpler (AbstractLiteral Expr) UniOp where
    simplerImp x y = negOrderM $ simplerImp y x
instance Simpler UniOp (AbstractLiteral Expr) where
    simplerImp a b = do
        tya <- ttypeOf a
        tyb <- ttypeOf b
        simplerImp tya tyb

instance Simpler (AbstractLiteral Expr) BinOp where
    simplerImp x y = negOrderM $ simplerImp y x
instance Simpler BinOp (AbstractLiteral Expr) where
    simplerImp a b = do
        tya <- ttypeOf a
        tyb <- ttypeOf b
        simplerImp tya tyb

instance Simpler (AbstractLiteral Expr) Proc where
    simplerImp x y = negOrderM $ simplerImp y x
instance Simpler Proc (AbstractLiteral Expr) where
    simplerImp a b = do
        tya <- ttypeOf a
        tyb <- ttypeOf b
        simplerImp tya tyb

simplerTu2 :: forall (m :: * -> *) a a1 b.
              (Simpler a1 b, Simpler a b, HasLogger m, WithDoms m) =>
              (a, a1) -> b -> m Ordering
simplerTu2 (a,b) c = do
    ac <- simplerImp a c
    bc <- simplerImp b c
    return $ compareChain [ac,bc]

simplerTu3 :: forall (m :: * -> *) a a1 a2 b.
              (Simpler a2 b, Simpler a1 b, Simpler a b, HasLogger m,
               WithDoms m) =>
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
