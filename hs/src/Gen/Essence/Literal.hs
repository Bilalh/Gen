{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Literal where

import Conjure.Language.Definition
import Gen.Essence.Id
import Gen.Essence.Ints
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type            ()
import Gen.Helpers.SizeOf
import Gen.Imports

instance (Generate a, WrapConstant a) => Generate (AbstractLiteral a) where
  give con = do
   (res, _ :: Type) <- give con
   return res

  possiblePure _ (Just TypeBool) _ = False
  possiblePure _ (Just TypeInt ) _ = False
  possiblePure _ (Just ty) d       = (depthOf ty) <= (fromIntegral d)
  possiblePure _ Nothing d         = d >= 1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []

instance (Generate a, WrapConstant a) => Generate (AbstractLiteral a, Type) where
  give GNone = do
    ty <- give GOnlyLiteralTypes
    give (GType ty)

  give (GType r@(TypeSet ty)) = do
    es <- bounded3 (0,3)  (dgive (GType ty))
    return $ (AbsLitSet es, r)

  give (GType r@(TypeMSet ty)) = do
    es <- bounded3 (0,3)  (dgive (GType ty))
    return $ (AbsLitMSet es, r)

  give (GType r@(TypeMatrix TypeInt ty)) = do
    numElems <- choose3 (0,5)
    es <- vectorOf3 numElems (dgive (GType ty))
    idx <- intDomainOfSize (fromIntegral numElems)
    return $ (AbsLitMatrix idx es, r)

  give (GType r@(TypeFunction t1 t2)) = do
    numElems <- choose3 (0,5)
    e1 <- vectorOf3 numElems (dgive (GType t1))
    e2 <- vectorOf3 numElems (dgive (GType t2))
    let es = zip e1 e2
    return $ (AbsLitFunction es, r)

  give (GType r@(TypeRelation ts)) = do
    ee :: [[a]] <- bounded3 (0,5) $ forM ts $ dgive . GType
    return $ (AbsLitRelation ee, r)

  give (GType r@(TypePartition t)) = do
    n  <- choose3 (0,5)
    es <- replicateM n $ do
            bounded3 (0,5) (dgive (GType t))

    return $ (AbsLitPartition es, r)

  give (GType r@(TypeTuple ts)) = do
    es <- forM ts $ \t -> dgive (GType t)

    return $ (AbsLitTuple es, r)

  give t = giveUnmatched "Generate (AbstractLiteral a)" t

  possiblePure _ (Just TypeBool) _ = False
  possiblePure _ (Just TypeInt ) _ = False
  possiblePure _ (Just ty) d       = (depthOf ty) <= (fromIntegral d)
  possiblePure _ Nothing d         = d >= 1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []
