{-# LANGUAGE FlexibleInstances, RankNTypes #-}
module Gen.Reduce.Inners where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Domain
import Gen.Imports


class Inners x where
    innersReduce  :: (forall a.  [a] -> b)      -> x -> b
    innersExpand  ::  (forall a. Eq a => [a] -> [[a]] ) -> x -> [x]

instance (IntRange c, Pretty c, Eq c) => Inners (AbstractLiteral c) where
  innersReduce f (AbsLitFunction x)       = (f x)
  innersReduce f (AbsLitTuple x)          = (f x)
  innersReduce f (AbsLitSet x)            = (f x)
  innersReduce f (AbsLitMSet x)           = (f x)
  innersReduce f (AbsLitSequence x)       = (f x)
  innersReduce f (AbsLitRelation x)       = (f x)
  innersReduce f (AbsLitPartition x)      = (f x)
  innersReduce f (AbsLitMatrix _ x )      = (f x)


  innersExpand f (AbsLitFunction xs)      = map AbsLitFunction (f xs)
  innersExpand f (AbsLitTuple x)          = map AbsLitTuple (f x)
  innersExpand f (AbsLitSet x)            = map AbsLitSet (f x)
  innersExpand f (AbsLitMSet x)           = map AbsLitMSet (f x)
  innersExpand f (AbsLitSequence x)       = map AbsLitSequence (f x)
  innersExpand f (AbsLitRelation x)       = map AbsLitRelation (f x)
  innersExpand f (AbsLitPartition x)      = map AbsLitPartition (f x)
  innersExpand f (AbsLitMatrix d x)       = map doMatrix (f x)
    where
      doMatrix nx | length nx == length x = AbsLitMatrix d nx
      --TODO could remove/add to made the old range fit
      doMatrix nx = AbsLitMatrix ( intRange 1 (length nx)) nx

instance (IntRange c, Pretty c, Eq c) => Inners (Domain () c) where

  innersReduce _ _ = $notDone

  innersExpand f (DomainInt xs) = map DomainInt (f xs)
  innersExpand _ DomainBool     = []
  innersExpand f (DomainSet () a x)  = map (DomainSet () a) (concat $ f [x])

  -- innersExpand _ _ = $notDone
  innersExpand _ _ = []


class IntRange a where
    intRange :: Int -> Int -> Domainn a

instance IntRange Expr where
    intRange i j = dintRange i j

instance IntRange Constant where
    intRange i j = cintRange i j
