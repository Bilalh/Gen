{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Union where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Imports

import qualified Data.Set as S


instance Generate a => Generate (OpUnion a) where
  give GNone = do
    let allowed = S.fromList [K_TypeSet, K_TypeMSet, K_TypeFunction, K_TypeRelation]
    let ws = [ (k,0) | k <- typeKeys, k `S.notMember` allowed ]
    ty <- withWeights ws (give GNone)
    give (GType ty)

  give ty@GType{} = do
    pure OpUnion <*> give ty <*> give ty

  give t = giveUnmatched "Generate OpUnion" t

  possiblePure _ ty _ | not (allow ty) = False
    where
      allow :: Type -> Bool
      allow TypeAny        = True
      allow TypeSet{}      = True
      allow TypeMSet{}     = True
      allow TypeFunction{} = True
      allow TypeRelation{} = True
      allow _              = False

  possiblePure _ ty d = depthOf ty + 1 <= (fromIntegral d)
