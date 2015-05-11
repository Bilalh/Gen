{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Union where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports
import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpUnion a) where

  give GNone = do
      ty <- give (GOnlyTopLevel Types.unionLike)
      give (GType ty)

  give ty@GType{} = pure OpUnion <*> give ty <*> give ty
  give t          = giveUnmatched "Generate OpUnion" t

  possiblePure _ ty _ | not (Types.isUnionLike ty) = False

  possiblePure a TypeBool d = possibleNoType a d
  possiblePure _ _ _        = False
  possibleNoType _ d        = (2 :: Integer) <= (fromIntegral d)
