{-# LANGUAGE FlexibleInstances, KindSignatures, MultiParamTypeClasses,
             TypeSynonymInstances, QuasiQuotes #-}
module Gen.Reduce.Simpler where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Conjure.Language.Expression.Op
import Gen.Imports
import Gen.Reduce.Inners
import Gen.Classify.Meta(maximum')
import Gen.Helpers.SizeOf
import Gen.Helpers.TypeOf
import Conjure.Language.TypeOf
import Conjure.Language.Domain
import Gen.Reduce.Data(rrError,addLog)

import qualified Data.Foldable as F
import qualified Data.Text as T

-- True if a1 is simpler then a2
class (Pretty a, Eq a, Show a, Pretty b, Eq b, Show b
      )
    => Simpler a b where
  simpler  :: (MonadLog m) => a -> b -> m Ordering
  simpler1 :: (MonadLog m) => a -> b -> m Bool

  simplerImp :: (MonadLog m) => a -> b -> m Ordering

  simpler a b = do
    -- addLog "simplerStart" [nn "a" a, nn "b" b]
    res <- simplerImp a b
    addLog "simpler" [nn "a" a, nn "b" b
                     , nn "res" (show res)
                     -- , nn "ga" (groom a), nn "gb" (groom b)
                     ]
    return $ res

  simpler1 a b= simpler a b >>= (return . (== LT))

-- compareDepthThenAll a b = return $
--   case compare (depthOf a) (depthOf b) of
--     EQ -> compare (length $ universe a) (length $  universe b)
--     o -> o


instance Simpler Expr Expr where
    simplerImp (EVar a)               (EVar b)      = simplerImp a b
    simplerImp (EDom a)               (EDom b)      = simplerImp a b
    simplerImp (ECon a)               (ECon b)      = simplerImp a b
    simplerImp (ELit a)               (ELit b)      = simplerImp a b
    simplerImp a@ETyped{}           b@ETyped{}      = return $ compare (depthOf a) (depthOf b)
    simplerImp (EOp a)                (EOp b)       = simplerImp a b
    simplerImp (EMetaVar _)           (EMetaVar _)  = return EQ
    simplerImp EEmptyGuard            EEmptyGuard   = return EQ


    simplerImp (ECon a) (EOp b)  = simplerImp a b
    simplerImp (EOp a)  (ECon b) = simplerImp a b

    simplerImp (ELit a) (EOp b)  = simplerImp a b
    simplerImp (EOp a)  (ELit b) = simplerImp a b

    simplerImp (ELit a) (ECon b)  = simplerImp a b
    simplerImp (ECon a) (ELit b)  = simplerImp a b

    simplerImp a@ETyped{} b  = return $ compare (depthOf a) (depthOf b)
    simplerImp a b@ETyped{}  = return $ compare (depthOf a) (depthOf b)

    simplerImp (EVar a) b  = simplerImp a b
    simplerImp a (EVar b)  = simplerImp a b

    simplerImp x1@(EComp i1 g1 cs1)  x2@(EComp i2 g2 cs2) = do
      case compare (depthOf x1) (depthOf x2) of
        EQ -> do
          let c1Depth = maximum' 0 $ map depthOf cs1
          let c2Depth = maximum' 0 $ map depthOf cs2

          case compare (length cs1) (length cs2) of
            EQ -> case compare (length g1) (length g2) of
               EQ -> case compare (depthOf i1) (depthOf i2) of
                 EQ -> return $ compare c1Depth c2Depth

                 o  -> return o
               o  -> return o
            o  -> return o
        o  -> return o

    simplerImp a@EComp{} b = return $ compare (depthOf a) (depthOf b)
    simplerImp a b@EComp{} = return $ compare (depthOf a) (depthOf b)
    simplerImp a b = simplerImpError "Expr" a b



instance Simpler Type Type where
  simplerImp TypeInt   TypeInt  = return EQ
  simplerImp TypeBool  TypeBool = return EQ

  simplerImp TypeBool _        = return LT
  simplerImp _        TypeBool = return GT
  simplerImp TypeInt  _        = return LT
  simplerImp _        TypeInt  = return GT

  simplerImp TypeAny _        = return LT
  simplerImp _        TypeAny = return GT

  simplerImp a b = return $ compare (depthOf a) (depthOf b)


instance Simpler Var Var where
    simplerImp (Var _ _) (Var _ _) = return EQ


instance Simpler Constant Constant where
    simplerImp (ConstantAbstract a) (ConstantAbstract b) =
        simplerImp a b
    simplerImp a b = do
      ta <- ttypeOf a
      tb <- ttypeOf b
      simplerImp ta tb


instance (DepthOf c, IntRange c, DepthOf d, IntRange d, Simpler c d)
    => Simpler (AbstractLiteral c) (AbstractLiteral d) where
    simplerImp a@(AbsLitPartition as) b@(AbsLitPartition bs) =
      case compare (depthOf a) (depthOf b) of
        EQ -> return $ case (sum $ map length as,sum $ map length bs) of
                        (ca,cb) -> compare ca cb
        x  -> return x

    simplerImp a b = case compare (depthOf a) (depthOf b) of
     EQ -> return $ case ((innersReduce length a), (innersReduce length b)) of
                      (ca,cb) -> compare ca cb
     x  -> return x


instance Simpler (Domain () Expr) (Domain () Expr) where
  simplerImp a b =
    case compare (depthOf a) (depthOf b) of
      EQ -> case compare
            (length [ x :: Domain () Expr | x <- universe a ])
            (length [ x :: Domain () Expr | x <- universe b ]) of
              EQ -> return $ compareSameDomain a b
              x  -> return x

      x  -> return x

compareSameDomain :: Domain () Expr -> Domain () Expr -> Ordering
compareSameDomain (DomainMatrix _ a2)        (DomainMatrix _ b2) =
    compareSameDomain a2 b2

compareSameDomain (DomainSet _ a2 a3)         (DomainSet _ b2 b3) =
  case compare (attrCount a2) (attrCount b2) of
    EQ -> compareSameDomain a3 b3
    x  -> x

compareSameDomain (DomainMSet _ a2 a3)        (DomainMSet _ b2 b3) =
  case compare (attrCount a2) (attrCount b2) of
    EQ -> compareSameDomain a3 b3
    x  -> x

compareSameDomain (DomainFunction _ a2 a3 a4) (DomainFunction _ b2 b3 b4) =
  case compare (attrCount a2) (attrCount b2) of
    EQ -> compareSameDomain (DomainTuple [a3,a4] ) (DomainTuple [b3,b4] )
    x  -> x

compareSameDomain (DomainSequence _ a2 a3)    (DomainSequence _ b2 b3) =
  case compare (attrCount a2) (attrCount b2) of
    EQ -> compareSameDomain a3 b3
    x  -> x

compareSameDomain (DomainRelation _ a2 a3)    (DomainRelation _ b2 b3) =
  case compare (attrCount a2) (attrCount b2) of
    EQ ->
      let os = zipWith compareSameDomain a3 b3
          la = length (filter (== LT) os)
          lb = length (filter (== GT) os) in
      if      la > lb then LT
      else if lb > la then GT
      else EQ
    x  -> x

compareSameDomain (DomainPartition _ a2 a3)   (DomainPartition _ b2 b3) =
  case compare (attrCount a2) (attrCount b2) of
    EQ -> compareSameDomain a3 b3
    x  -> x

compareSameDomain (DomainTuple a)             (DomainTuple b)  =
  let os = zipWith compareSameDomain a b
      la = length (filter (== LT) os)
      lb = length (filter (== GT) os) in
  if      la > lb then LT
  else if lb > la then GT
  else EQ

compareSameDomain _ _ = EQ

-- FIXME hackly way of counting the number of attrs
attrCount :: Pretty a => a -> Int
attrCount a = case T.split (==',') $  stringToText $ show $  pretty a of
  [x] | T.null (T.strip x) -> 0
  xs  -> length xs


instance Simpler (Op Expr) (Op Expr) where
    simplerImp a b | [ELit la@AbsLitMatrix{}] <- F.toList a
                   , [ELit lb@AbsLitMatrix{}] <- F.toList b = simpler la lb
    simplerImp a b | [la@EComp{} ] <- F.toList a
                   , [lb@EComp{} ] <- F.toList b = simpler la lb
    simplerImp a b = case compare (depthOf a) (depthOf b) of
                       EQ -> do
                         let x1 = F.toList a
                         let x2 = F.toList b
                         case compare (length x1) (length x2) of
                           EQ -> do
                             os <-zipWithM simpler x1 x2
                             let la = length (filter (== LT) os)
                             let lb = length (filter (== GT) os)
                             if la > lb then
                                 return LT
                             else if lb > la then
                                 return GT
                             else
                                 return EQ
                           o  -> return o
                       o  -> return o



-- Mixed

instance (Simpler c Constant, Simpler Constant c, IntRange c, DepthOf c)
    => Simpler (AbstractLiteral c) Constant where simplerImp = negSimplerImp
instance (Simpler Constant c, IntRange c, DepthOf c )
    => Simpler Constant (AbstractLiteral c) where
    simplerImp (ConstantAbstract x) l  = simpler x l
    simplerImp _ _  = return LT

instance Simpler (Op Expr) Constant where simplerImp = negSimplerImp
instance Simpler Constant (Op Expr) where
  simplerImp a b = return $ compare (depthOf a) (depthOf b)

instance Simpler Expr Constant where simplerImp = negSimplerImp
instance Simpler Constant Expr where
  simplerImp a b = return $ compare (depthOf a) (depthOf b)

instance Simpler Expr (Var) where simplerImp = negSimplerImp
instance Simpler (Var) Expr where
  simplerImp _ _  = return LT


instance Simpler (Op Expr) Expr where simplerImp = negSimplerImp
instance Simpler Expr (Op Expr) where

    simplerImp (EVar _) _     = return LT
    simplerImp (EMetaVar _) _ = return LT
    simplerImp EEmptyGuard _  = return LT

    simplerImp (EOp a) b      = simplerImp a b
    simplerImp (ECon a) b     = simplerImp a b
    simplerImp (ELit a) b     = simplerImp a b
    simplerImp (ETyped a _) b = return $ compare (depthOf a) (depthOf b)
    simplerImp a@EComp{} b    = return $ compare (depthOf a) (depthOf b)

    simplerImp a b = simplerImpError "Expr Op" a b


instance Simpler (AbstractLiteral Expr) Expr where simplerImp = negSimplerImp
instance (Simpler c Expr, Simpler Expr c, Simpler Constant c, IntRange c, DepthOf c, TTypeOf c, TypeOf c)
    => Simpler Expr (AbstractLiteral c) where

    simplerImp (EVar _) _     = return LT
    simplerImp (EMetaVar _) _ = return LT
    simplerImp EEmptyGuard _  = return LT

    simplerImp (EOp a) b      = simplerImp a b
    simplerImp (ECon a) b     = simplerImp a b
    simplerImp (ELit a) b     = simplerImp a b
    simplerImp (ETyped a _) b = return $ compare (depthOf a) (depthOf b)
    simplerImp a@EComp{} b    = return $ compare (depthOf a) (depthOf b)

    simplerImp a b = simplerImpError "Expr Literal" a b


instance (Simpler c Expr, Simpler Expr c, IntRange c, DepthOf c)
       => Simpler (Op Expr) (AbstractLiteral c) where simplerImp = negSimplerImp
instance (Simpler c Expr, Simpler Expr c, IntRange c, DepthOf c)
    => Simpler (AbstractLiteral c) (Op Expr) where
    simplerImp a b = do
      return $ case compare (depthOf a) (depthOf b) of
        EQ -> LT
        c  -> c


simplerImpError :: (Simpler a b, MonadLog m) => String -> a -> b -> m Ordering
simplerImpError t a b = rrError ("simplerImp " ++ t)
                      [ nn "a" a
                      , nn  "b" b
                      , nn "a" (groom a)
                      , nn "b" (groom b)
                      ]



negSimplerImp :: forall (m :: * -> *) a b. (Simpler a b, MonadLog m)
              => b -> a -> m Ordering
negSimplerImp a b =  do
  oo <- simplerImp b a
  return . negOrder $ oo

negOrder :: Ordering -> Ordering
negOrder LT = GT
negOrder EQ = EQ
negOrder GT = LT
