{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Domain where

import Conjure.Language.Domain
import Gen.Essence.Literal         ()
import Gen.Essence.Range           ()
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type            ()
import Gen.Helpers.StandardImports

instance (Generate a, WrapConstant a) => Generate (Domain () a) where
  give GNone = do
      -- ty <- give GNone
      give (GType TypeInt)

  give (GType TypeBool)           = return DomainBool
  give (GType TypeInt)            = pure DomainInt <*> vectorOf3 2 (give GNone)
  -- give (GType (TypeMatrix ty))     = _x
  -- give (GType (TypeSet ty))       = _x
  -- give (GType (TypeMSet ty))      = _x
  -- give (GType (TypeFunction ty1 ty2)) = _x
  -- give (GType (TypeTuple ty))     = _x
  -- give (GType (TypeRelation ty))       = _x
  -- give (GType (TypePartition ty))       = _x
  -- give (GType (TypeUnnamed ty))    = _x
  -- give (GType (TEnum ty))      = _x
  -- give (GType TypeAny)            = _x

  give t = giveUnmatched "Generate (Domain () a)" t

  possiblePure _ _ _ = True
