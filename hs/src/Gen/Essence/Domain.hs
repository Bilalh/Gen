{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Domain where

import Conjure.Language.Domain
import Conjure.Language.Constant
import Gen.Essence.Constant        ()
import Gen.Essence.Literal         ()
import Gen.Essence.Range           ()
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type            ()
import Gen.Imports

instance (Generate a, WrapConstant a) => Generate (Domain () a) where
  give GNone = give GNone >>= \ty -> give (GType ty)

  give (GType TypeBool)     = pure DomainBool
  give (GType TypeInt)      = DomainInt <$> vectorOf3 2 (give GNone)
  give (GType (TypeSet ty)) = DomainSet <$> pure () <*>  give GNone <*> give (GType ty)

  -- give (GType (TypeMatrix ty))     = _x

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

instance Generate a => Generate (SetAttr a) where
  give GNone         = SetAttr <$> give (GNone)
  give t             = giveUnmatched "Generate (SetAttr a)" t
  possiblePure _ _ _ = True

instance Generate a => Generate (SizeAttr a)  where
  give GNone = do
    oneof3 [
       pure SizeAttr_None
     , SizeAttr_Size    <$> give (GType TypeInt)
     , SizeAttr_MinSize <$> give (GType TypeInt)
     , SizeAttr_MaxSize <$> give (GType TypeInt)
     -- FIXME ensure b >= a?
     -- , SizeAttr_MinMaxSize <$> give GNone <*> give GNone
     ]

  give t = giveUnmatched "Generate (SetAttr a)" t

  possiblePure _ _ _ = True

--  Overlapping instances
-- instance Generate (SizeAttr Constant) where
--   give GNone = do
--     oneof3 [
--        pure SizeAttr_None
--      , SizeAttr_Size    <$> give (GType TypeInt)
--      , SizeAttr_MinSize <$> give (GType TypeInt)
--      , SizeAttr_MaxSize <$> give (GType TypeInt)
--      , do
--        a@(ConstantInt i) <- give (GNone)
--        b <- give (GGTE i)
--        return $ SizeAttr_MinMaxSize a b
--      ]

--   give t = giveUnmatched "Generate (SetAttr a)" t

--   possiblePure _ _ _ = True
