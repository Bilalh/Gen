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
  give con = do
   (res, _ :: Type) <- give con
   return res

  possiblePure _ TypeBool _  = False
  possiblePure _ TypeInt  _  = False
  possiblePure _ ty d = (depthOf ty) <= (fromIntegral d)

  possibleNoType _ _ = True


instance (Generate a, WrapConstant a) => Generate (AbstractLiteral a, Type) where
  give GNone = do
    ty <- give GOnlyLiteralTypes
    give (GType ty)

  give (GType r@(TypeSet ty)) = do
    num <- choose3 (0,5)
    es <- vectorOf3 num (withDepthDec $ give (GType ty))
    return $ (AbsLitSet es, r)

  give (GType r@(TypeMatrix TypeInt ty)) = do
    numElems <- choose3 (0,5)
    es <- vectorOf3 numElems (withDepthDec $ give (GType ty))
    idx <- intDomainOfSize (fromIntegral numElems)
    return $ (AbsLitMatrix idx es, r)

  give t = giveUnmatched "Generate (AbstractLiteral a)" t

  possiblePure _ TypeBool _  = False
  possiblePure _ TypeInt  _  = False
  possiblePure _ ty d = (depthOf ty) <= (fromIntegral d)

  possibleNoType _ _ = True
