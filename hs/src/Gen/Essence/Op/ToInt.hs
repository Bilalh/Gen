{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.ToInt where

import Conjure.Language.AdHoc
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types

instance (Generate a, ExpressionLike a) => Generate (OpToInt a) where
  give GNone           = give (GType TypeInt)
  give (GType TypeInt) = pure OpToInt <*> give (GType TypeBool)
  give t               = giveUnmatched "Generate OpToInt" t

  possiblePure _ (Just TypeInt ) _ = True
  possiblePure _ Just{} _          = False
  possiblePure _ _ d               = d >= 0

  requires _ _ = [RAll [K_TypeBool]]
