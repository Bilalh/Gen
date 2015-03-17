{-# LANGUAGE FlexibleInstances, RankNTypes #-}
module Gen.Reduce.Inners where

import Conjure.Language.AbstractLiteral
import Gen.Prelude


class Inners x where
    innersReduce  :: (forall a.  [a] -> b)      -> x -> b
    innersExpand  :: (forall a.  [a] -> [[a]] ) -> x -> [x]
    innersMap     :: (Monad m, Applicative m )
                  => (forall a.  [a] -> m [a] ) -> x -> m x

instance Inners (Literal) where
  innersReduce f (AbsLitFunction xs)      = (f xs)
  innersReduce f (AbsLitTuple x)          = (f x)
  innersReduce f (AbsLitSet x)            = (f x)
  innersReduce f (AbsLitMSet x)           = (f x)
  innersReduce f (AbsLitFunction x)       = (f x)
  innersReduce f (AbsLitSequence x)       = (f x)
  innersReduce f (AbsLitRelation x)       = (f x)
  innersReduce f (AbsLitPartition x)      = (f x)

  innersExpand f (AbsLitFunction xs)      = map AbsLitFunction (f xs)
  innersExpand f (AbsLitTuple x)          = map AbsLitTuple (f x)
  innersExpand f (AbsLitSet x)            = map AbsLitSet (f x)
  innersExpand f (AbsLitMSet x)           = map AbsLitMSet (f x)
  innersExpand f (AbsLitFunction x)       = map AbsLitFunction (f x)
  innersExpand f (AbsLitSequence x)       = map AbsLitSequence (f x)
  innersExpand f (AbsLitRelation x)       = map AbsLitRelation (f x)
  innersExpand f (AbsLitPartition x)      = map AbsLitPartition (f x)

  innersMap f (AbsLitFunction xs) = AbsLitFunction  <$> (f xs)
  innersMap f (AbsLitTuple x)     = AbsLitTuple     <$> (f x)
  innersMap f (AbsLitSet x)       = AbsLitSet       <$> (f x)
  innersMap f (AbsLitMSet x)      = AbsLitMSet      <$> (f x)
  innersMap f (AbsLitFunction x)  = AbsLitFunction  <$> (f x)
  innersMap f (AbsLitSequence x)  = AbsLitSequence  <$> (f x)
  innersMap f (AbsLitRelation x)  = AbsLitRelation  <$> (f x)
  innersMap f (AbsLitPartition x) = AbsLitPartition <$> (f x)


  -- innersExpand f (AbsLitRecord x)         = map _x (f x)
  -- innersExpand f (AbsLitVariant x1 x2 x3) = map _x (f x)
  -- innersExpand f (AbsLitMatrix x1 x2)     = map _x (f x)
