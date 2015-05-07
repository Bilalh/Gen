{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Union where

import Conjure.Language.Expression.Op
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.SizeOf
import Gen.Helpers.StandardImports

import qualified Data.Set as S


instance Generate a => Generate (OpUnion a) where
  give GNone = do
    let allowed = S.fromList ["TypeSet", "TypeMSet", "TypeFunction", "TypeRelation"]
    let ws = [ (k,0) | k <- typeKeys, k `S.notMember` allowed ]
    ty <- withWeights ws (give GNone)
    give (GType ty)

  give (GType ty) = $notDone

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
