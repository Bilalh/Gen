{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.ToSet(Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpToSet a) where
  give GNone = give . GType =<< give (GOnlyTopLevel [K_TypeSet])

  give (GType (TypeSet a)) = do
    fty <- elemFreq3 =<< getWeights tys
    OpToSet <$> give (GType (fty a))

  give t = giveUnmatched "Generate OpToSet" t

  possiblePure _ (Just ty@TypeSet{}) d = (fromIntegral d) >= depthOf ty
  possiblePure _ Nothing d             = d >=1
  possiblePure _ _ _                   = False

  requires _ _  = [RAny (map fst tys) ]

tys :: [(Key, Type -> Type)]
tys = [ (K_TypeMSet, TypeMSet) ]
