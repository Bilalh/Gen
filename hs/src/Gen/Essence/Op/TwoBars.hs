{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.TwoBars where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types

instance (Generate a, ExpressionLike a) => Generate (OpTwoBars a) where
  give GNone = give (GType TypeInt)

  give (GType TypeInt) = do
      ty <- give (GOnlyTopLevel Types.hasLength)
      pure OpTwoBars <*> give (GType ty)

  give t             = giveUnmatched "Generate OpTwoBars" t

  possiblePure _ (Just ty)  _ | ty /= TypeInt = False
  possiblePure _ _ d                          = d >=2

  requires _ _       = [RAny Types.hasLength]
