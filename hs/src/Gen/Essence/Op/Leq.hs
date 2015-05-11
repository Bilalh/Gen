{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Op.Leq where

import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Imports
import qualified Gen.Essence.Data.Types as Types


instance Generate a => Generate (OpLeq a) where
  give GNone = give (GType TypeBool)

  give (GType TypeBool) = do
    ty <- GType <$> giveOnly GNone Types.ordered
    pure OpLeq <*> give ty <*> give ty

  give t = giveUnmatched "Generate OpLeq" t

  possiblePure a TypeBool d = possibleNoType a d
  possiblePure _ _ _        = False
  possibleNoType _ d        = (1 :: Integer) <= (fromIntegral d)
