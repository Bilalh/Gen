{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Literal where

import Conjure.Language.Definition
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type            ()
import Gen.Helpers.SizeOf
import Gen.Helpers.StandardImports


instance Generate a => Generate (AbstractLiteral a) where
  give GNone = do
      ty <- give GOnlyLiteralTypes
      give (GType ty)

  give (GType (TSet ty)) = do
      es <- vectorOf3 2 (withDepthDec $ give (GType ty))
      return $ AbsLitSet es

  give t = giveUnmatched "Generate (AbstractLiteral a)" t

  possiblePure _ TBool _  = False
  possiblePure _ TInt  _  = False
  possiblePure _ ty d = (depthOf ty) <= (fromIntegral d)
