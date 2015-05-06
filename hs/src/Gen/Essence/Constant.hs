{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Constant where

import Gen.Essence.St
import Gen.Helpers.StandardImports
import Gen.Essence.Rnd
import Gen.Essence.Type()
import Conjure.Language.Definition
import Gen.Helpers.SizeOf
import Gen.Essence.Literal()

instance Generate Constant where
  give GNone = do
    ty <- give GNone
    give (GType ty)

  give (GType TInt)      = pure ConstantInt      <*> choose3 (0,5)
  give (GType TBool)     = pure ConstantBool     <*> choose3 (True,False)
  give (GType ty@TSet{}) = pure ConstantAbstract <*> give (GType ty)

  give t = giveUnmatched "Generate Constant" t

  possiblePure _ ty d = (depthOf ty) <= (fromIntegral d)
