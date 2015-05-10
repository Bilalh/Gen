{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Literal where

import Conjure.Language.Definition
import Gen.Essence.Domain
import Gen.Essence.St
import Gen.Essence.Rnd
import Gen.Essence.Type            ()
import Gen.Helpers.SizeOf
import Gen.Imports


instance (Generate a, WrapConstant a) => Generate (AbstractLiteral a) where
  give GNone = do
    ty <- give GOnlyLiteralTypes
    give (GType ty)

  give (GType (TypeSet ty)) = do
    es <- vectorOf3 2 (withDepthDec $ give (GType ty))
    return $ AbsLitSet es

  give (GType (TypeMatrix TypeInt ty)) = do
    numElems <- return 2
    es <- vectorOf3 numElems (withDepthDec $ give (GType ty))
    idx <- intDomainOfSize (fromIntegral numElems)
    return $ AbsLitMatrix idx es

  give t = giveUnmatched "Generate (AbstractLiteral a)" t

  possiblePure _ TypeBool _  = False
  possiblePure _ TypeInt  _  = False
  possiblePure _ ty d = (depthOf ty) <= (fromIntegral d)

  possibleNoType _ _ = True
