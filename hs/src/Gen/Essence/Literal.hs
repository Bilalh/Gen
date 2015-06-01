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

import qualified Gen.Essence.Data.Types as Types

instance (Generate a, WrapConstant a) => Generate (AbstractLiteral a) where
  give con = do
   (res, _ :: Type) <- give con
   return res

  possiblePure _ (Just TypeBool) _ = False
  possiblePure _ (Just TypeInt ) _ = False
  possiblePure _ (Just ty) d       = fromIntegral d >= depthOf ty
  possiblePure _ Nothing d         = d >= 1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []

instance (Generate a, WrapConstant a) => Generate (AbstractLiteral a, Type) where
  give GNone = do
    sanityn 1 "Generate AbstractLiteral "
    ty <- give $ GOnlyTopLevel Types.literals
    give (GType ty)

  give (GType r@(TypeSet ty)) = do
    es <- boundedChecked (Proxy :: Proxy a) (0,3)  (dgive (GType ty))
    return $ (AbsLitSet es, r)

  give (GType r@(TypeMSet ty)) = do
    es <- boundedChecked (Proxy :: Proxy a) (0,3)  (dgive (GType ty))
    return $ (AbsLitMSet es, r)

  give (GType r@(TypeMatrix TypeInt ty)) = do
    numElems <- chooseChecked (Proxy :: Proxy a) (0,5)
    es <-  vectorOf3 numElems (dgive (GType ty))
    idx <- intDomainOfSize (fromIntegral numElems)
    return $ (AbsLitMatrix idx es, r)

  give (GType r@(TypeFunction t1 t2)) = do
    numElems <- chooseChecked (Proxy :: Proxy a) (0,5)
    e1 <- vectorOf3 numElems (dgive (GType t1))
    e2 <- vectorOf3 numElems (dgive (GType t2))
    let es = zip e1 e2
    return $ (AbsLitFunction es, r)

  give (GType r@(TypeRelation ts)) = do
    numElems <- chooseChecked (Proxy :: Proxy a) (0,5)
    ee :: [[a]] <- vectorOf3 numElems $ forM ts $ dgive . GType
    return $ (AbsLitRelation ee, r)

  give (GType r@(TypePartition t)) = do
    n <- chooseChecked (Proxy :: Proxy a) (0,5)
    es <- replicateM n $ do
            boundedChecked (Proxy :: Proxy a) (1,5) (dgive (GType t))

    if all (null) es then
        logDebug2 $line  ["is Empty " <+> pretty (AbsLitPartition es)]
    else
        logDebug2 $line  ["not Empty " <+> pretty (AbsLitPartition es)]

    return $ (AbsLitPartition es, r)

  give (GType r@(TypeTuple ts)) = do
    es <- forM ts $ \t -> dgive (GType t)
    return $ (AbsLitTuple es, r)

  give t = giveUnmatched "Generate (AbstractLiteral a)" t

  possiblePure _ (Just TypeBool) _ = False
  possiblePure _ (Just TypeInt ) _ = False
  possiblePure _ (Just ty) d       = fromIntegral d >= depthOf ty
  possiblePure _ Nothing d         = d >= 1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []

boundedChecked :: (WrapConstant a) => Proxy a -> (Int, Int) -> GenSt a -> GenSt [a]
boundedChecked proxy tu as = do
  n <- chooseChecked proxy tu
  vectorOf3 n as

chooseChecked :: (WrapConstant a) => Proxy a -> (Int, Int) -> GenSt Int
chooseChecked proxy tu  | allowEmpty proxy = choose3 tu
chooseChecked _  (a,b)  =  choose3 (max 1 a, b)
