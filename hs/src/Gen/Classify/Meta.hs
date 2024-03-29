{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}
module Gen.Classify.Meta where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Expression.Op
import Gen.Classify.DomTypes
import Gen.Helpers.SizeOf
import Gen.Imports

import qualified Data.Foldable as F
import qualified Data.Map      as M

data Feature = Fquan
              | FexprInLiteral
              | Findexing       -- tuple indexing, matrix slices etc
              | Fliterals       -- literal apart from int and bool
              | FnoConstraints
              | Fop
              | Fref            -- references variables
              | Fsum            -- quan sum
              | Ftyped
              | FComp
                deriving(Show, Generic, Typeable, Eq)

data SpecMeta = SpecMeta
    {
        constraint_depth_ :: Integer
      , constraint_count_ :: Int
      , dom_count_        :: Int
      , dom_depth_        :: Integer
      , dom_most_complex_ ::Type
      , dom_types_        :: [Type]
      , features_         :: [Feature]
    }  deriving(Show, Generic, Typeable, Eq)


instance FromJSON Feature
instance ToJSON Feature

instance FromJSON SpecMeta
instance ToJSON SpecMeta

instance Hashable Feature
instance Hashable SpecMeta

mkMeta :: (Monad m, Applicative m) => Spec ->  m SpecMeta
mkMeta sp@(Spec ds cs obj) = do
  let doms              = map (domOfGF . snd . snd ) .  M.toList $ ds
      constraint_depth_ = maximum' 0 (depthOf obj : map depthOf cs)
      dom_depth_        = maximum' 0 $ (map depthOf) doms
      constraint_count_ = length cs
      dom_count_        = M.size ds

  dom_types_        <- domTypes sp
  features_         <- findFeatures sp
  dom_most_complex_ <- maximumByM complex1 dom_types_


  let meta = SpecMeta{..}
  return meta


complex1 ::(Monad m, Applicative m) =>Type ->Type -> m Ordering
complex1 t1 t2 = do
  let a = depthOf t1
      b = depthOf t2

  if | a == b -> return EQ
     | a <  b -> return LT
     | a >  b -> return GT




maximumByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m a
maximumByM _ [] = error "maximumByM empty list"
maximumByM cmp l@(c:_) = F.foldrM max' c l
  where max' x y = cmp x y >>= \case
                        GT -> return x
                        _  -> return y


maximum' :: Ord a => a -> [a] -> a
maximum' a [] = a
maximum' _ xs = maximum xs

findFeatures ::(Monad m, Applicative m) => Spec -> m [Feature]
findFeatures (Spec _ cs obj) = do
    let objs = case obj of
                 Nothing               -> []
                 (Just (_, a)) -> getFeatures a

    let fs = concatMap getFeatures cs
             ++
               objs
             ++
               case cs of
                 [] -> [FnoConstraints]
                 _  -> []

    return $ nub fs

class HasFeature e where
    getFeatures :: e -> [Feature]

instance HasFeature Expr where
    getFeatures (ELit e)         = Fliterals : getFeatures e
    getFeatures (ECon _)         = []
    getFeatures (EVar _)         = [Fref]
    getFeatures (EOp e)          = Fop : getFeatures e
    getFeatures (EDom e)         = getFeatures e
    getFeatures (ETyped _ e2)    = Ftyped : getFeatures e2
    getFeatures EEmptyGuard      = []
    getFeatures (EComp _ _ cons) = FComp : concatMap getFeatures cons
    getFeatures (EMetaVar _)     = []

    getFeatures (EQuan Sum _ e2 e3 e4) = Fsum : (concat
          [getFeatures  e2, getFeatures  e3, getFeatures e4] )
    getFeatures (EQuan _ _ e2 e3 e4)  = Fquan : (concat
          [getFeatures  e2, getFeatures  e3, getFeatures e4] )



instance HasFeature (Op Expr) where
  getFeatures x = F.foldl (\y e -> y ++ getFeatures e ) [] x

instance HasFeature (AbstractLiteral Expr) where
    getFeatures (AbsLitTuple l)     = concatMap getFeatures l
    getFeatures (AbsLitMatrix _ l1) = concatMap getFeatures l1
    getFeatures (AbsLitSet l)       = concatMap getFeatures l
    getFeatures (AbsLitMSet l)      = concatMap getFeatures l
    getFeatures (AbsLitFunction l)  = concatMap (\(a,b) ->  getFeatures a ++ getFeatures b) l
    getFeatures (AbsLitRelation l)  = concatMap (concatMap getFeatures) l
    getFeatures (AbsLitPartition l) = concatMap (concatMap getFeatures) l

    getFeatures (AbsLitSequence x) = concatMap getFeatures x
    getFeatures (AbsLitRecord x)   = concatMap (getFeatures . snd)  x
    getFeatures (AbsLitVariant x1 _ x3) = getFeatures x3 ++ fromMaybe []
                                            (fmap ( concatMap (getFeatures . snd ) ) x1)



instance HasFeature (Domain () Expr) where
  getFeatures _ = []
