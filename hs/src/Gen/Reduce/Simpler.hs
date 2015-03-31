{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses,
             TypeSynonymInstances, QuasiQuotes #-}
module Gen.Reduce.Simpler where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Conjure.Language.Expression.Op
import Gen.AST.TH
import Gen.Prelude
import Gen.Reduce.Inners
import Gen.Classify.Meta(maximum')

-- True if a1 is simpler then a2
class (Pretty a, Eq a, Show a, Pretty b, Eq b, Show b
      )
    => Simpler a b where
  simpler  :: (HasLogger m) => a -> b -> m Ordering
  simpler1 :: (HasLogger m) => a -> b -> m Bool

  simplerImp :: (HasLogger m) => a -> b -> m Ordering

  simpler a b = do
    -- addLog "simplerStart" [nn "a" a, nn "b" b]
    res <- simplerImp a b
    addLog "simpler" [nn "a" a, nn "b" b
                     , nn "res" (show res)
                     -- , nn "ga" (groom a), nn "gb" (groom b)
                     -- , nn "ga" (groom na), nn "gb" (groom nb)
                     ]
    return $ res

  simpler1 a b= simpler a b >>= (return . (== LT))

instance Simpler Expr Expr where
    simplerImp (EVar a)               (EVar b)      = simplerImp a b
    simplerImp (EDom a)               (EDom b)      = simplerImp a b
    simplerImp (ECon a)               (ECon b)      = simplerImp a b
    simplerImp (ELit a)               (ELit b)      = simplerImp a b
    simplerImp (ETyped _ a)           (ETyped _ b)  = simplerImp a b
    simplerImp (EOp a)                (EOp b)       = simplerImp a b
    simplerImp (EMetaVar _)           (EMetaVar _)  = return EQ

    simplerImp EEmptyGuard            EEmptyGuard            = return EQ


    simplerImp (ECon a) (EOp b)  = simplerImp a b
    simplerImp (EOp a)  (ECon b) = simplerImp a b

    simplerImp (ELit a) (EOp b)  = simplerImp a b
    simplerImp (EOp a)  (ELit b) = simplerImp a b


    simplerImp (ETyped _ a) b  = simplerImp a b
    simplerImp a (ETyped _ b)  = simplerImp a b

    simplerImp (EVar a) b  = simplerImp a b
    simplerImp a (EVar b)  = simplerImp a b

    simplerImp _ EComp{} = return LT
    simplerImp EComp{} _ = return GT

    simplerImp (EComp i1 _ cs1)  (EComp i2 _ cs2) = do
         let c1Depth = maximum' 0 $ map depthOf cs1
         let c2Depth = maximum' 0 $ map depthOf cs2

         case compare c1Depth c2Depth of
           EQ -> case compare (length cs1) (length cs2) of
               EQ -> return $ compare (depthOf i1) (depthOf i2)

               o  -> return o
           o  -> return o


    simplerImp a b = simplerImpError "Expr" a b



instance Simpler TType TType where
    simplerImp TBool TBool = return EQ
    simplerImp TBool _     = return LT
    simplerImp TInt  TInt  = return EQ
    simplerImp TInt  _     = return LT

    simplerImp TAny TAny   = return EQ
    simplerImp TAny _      = return LT

    simplerImp a b = return $ compare (depthOf a) (depthOf b)


instance Simpler Var Var where
    simplerImp (Var _ a) (Var _ b) = return EQ


instance Simpler Constant Constant where
    simplerImp a b = do
      ta <- ttypeOf a
      tb <- ttypeOf b
      simplerImp ta tb


instance Simpler Literal Literal where
    simplerImp a@(AbsLitPartition as) b@(AbsLitPartition bs) =
      case compare (depthOf a) (depthOf b) of
        EQ -> return $ case (sum $ map length as,sum $ map length bs) of
                        (ca,cb) -> compare ca cb
        x  -> return x

    simplerImp a b = case compare (depthOf a) (depthOf b) of
     EQ -> return $ case ((innersReduce length a), (innersReduce length b)) of
                      (ca,cb) -> compare ca cb
     x  -> return x


instance Simpler (Domainn Expr) (Domainn Expr) where
    simplerImp a b = return $ compare (depthOf a) (depthOf b)


instance Simpler (Op Expr) (Op Expr) where
    simplerImp a b = return $ compare (depthOf a) (depthOf b)


-- Mixed

instance Simpler Literal Constant where simplerImp = negSimplerImp
instance Simpler Constant Literal where
    simplerImp _ _  = return LT

instance Simpler (Op Expr) Constant where simplerImp = negSimplerImp
instance Simpler Constant (Op Expr) where
    simplerImp _ _  = return LT

instance Simpler Expr Constant where simplerImp = negSimplerImp
instance Simpler Constant Expr where
    simplerImp _ _  = return LT

instance Simpler Expr (Var) where simplerImp = negSimplerImp
instance Simpler (Var) Expr where
        simplerImp _ _  = return LT


instance Simpler (Op Expr) Expr where simplerImp = negSimplerImp
instance Simpler Expr (Op Expr) where

    simplerImp (EVar _) _     = return LT
    simplerImp (ECon _) _     = return LT
    simplerImp (EMetaVar _) _ = return LT
    simplerImp EEmptyGuard _  = return LT

    simplerImp (EOp a) b        = simplerImp a b
    simplerImp (ETyped _ a) b   = simplerImp a b
    simplerImp (ELit a) b       = simplerImp a b

    -- simplerImp (EDom a) b       = _h
    simplerImp EComp{} _ = return GT

    simplerImp a b = simplerImpError "Expr Op" a b


instance Simpler (Literal) Expr where simplerImp = negSimplerImp
instance Simpler Expr (Literal) where

    simplerImp (EVar _) _     = return LT
    simplerImp (ECon _) _     = return LT
    simplerImp (EMetaVar _) _ = return LT
    simplerImp EEmptyGuard _  = return LT

    simplerImp (ELit a) b       = simplerImp a b
    simplerImp (ETyped _ a) b   = simplerImp a b

    simplerImp (EOp a) b        = simplerImp a b
    -- simplerImp (EDom a) b       = simplerImp a b

    simplerImp EComp{} _  = return GT

    simplerImp a b = simplerImpError "Expr Literal" a b

instance Simpler (Op Expr) Literal where simplerImp = negSimplerImp
instance Simpler Literal (Op Expr) where
    simplerImp a b = do
      return $ case compare (depthOf a) (depthOf b) of
        EQ -> LT
        c  -> c


simplerImpError :: (Simpler a b, HasLogger m) => String -> a -> b -> m Ordering
simplerImpError t a b = rrError ("simplerImp " ++ t)
                      [ nn "a" a
                      , nn  "b" b
                      , nn "a" (groom a)
                      , nn "b" (groom b)
                      ]



negSimplerImp :: forall (m :: * -> *) a b. (Simpler a b, HasLogger m)
              => b -> a -> m Ordering
negSimplerImp a b =  do
  oo <- simplerImp b a
  return . negOrder $ oo

negOrder :: Ordering -> Ordering
negOrder LT = GT
negOrder EQ = EQ
negOrder GT = LT
