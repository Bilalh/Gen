{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Mod where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports


instance (Generate a, ExpressionLike a) => Generate (OpMod a) where
  give GNone = give (GType TypeInt)

  give ty@GType{} = do
    -- OpMod <$> give ty <*> withWeights [(K_Int_0,0)] (give ty)
    OpMod <$> give ty <*> give ty

  give t = giveUnmatched "Generate OpMod" t

  possiblePure _ (Just ty)  _ | ty /= TypeInt = False
  possiblePure _ _ d = d >= 1

  requires _ _ = [RAll [K_TypeInt]]
