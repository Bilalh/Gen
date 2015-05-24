{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.ToRelation (Generate(..)) where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.Id
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpToRelation a) where
  give (GType (TypeRelation [a,b])) = do
    OpToRelation <$> give (GType $ TypeFunction a b )

  give t = giveUnmatched "Generate OpToRelation" t

  possiblePure _ (Just ty@(TypeRelation [_,_])) d = fromIntegral d >= depthOf ty
  possiblePure _ Just{} _                         = False
  possiblePure _ Nothing _                        = False

  requires _ (Just (TypeRelation [a,b] )) = [ RAll (keyList a)
                                            , RAll (keyList b)
                                            , RAll [K_TypeFunction]]
  requires _ _                            = []
