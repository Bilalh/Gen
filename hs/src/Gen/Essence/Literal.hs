{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}
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

instance (Generate a, GenInfo a) => Generate (AbstractLiteral a) where
  give con = do
   (res, _ :: Type) <- give con
   return res

  possiblePure _ (Just TypeBool) _ = False
  possiblePure _ (Just TypeInt ) _ = False
  possiblePure _ (Just ty) d       = fromIntegral d >= depthOf ty
  possiblePure _ Nothing d         = d >= 1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []


instance (Generate a, GenInfo a) => Generate (AbstractLiteral a, Type) where
  give GNone = do
    sanityn 1 "Generate AbstractLiteral "
    ty <- give $ GOnlyTopLevel Types.literals
    give (GType ty)

  give (GType top) = addTypeKey top $ give1 (GType top)

    where
    give1 (GType r@(TypeSet ty)) = do
      es <- boundedChecked (Proxy :: Proxy a) (0,3)  (dgive (GType ty))
      return $ (AbsLitSet es, r)

    give1 (GType r@(TypeMSet ty)) = do
      es <- boundedChecked (Proxy :: Proxy a) (0,3)  (dgive (GType ty))
      return $ (AbsLitMSet es, r)

    give1 (GType r@(TypeMatrix TypeInt ty)) = do
      let alwaysRegular = True -- Set to False to allow irregular matrixes
      ((numElems,idx):rest) <- case alwaysRegular of
        False -> do
          val <- chooseChecked (Proxy :: Proxy a) (0,5)
          idx <- intDomainOfSize (fromIntegral val)
          return [(val,idx)]
        True  -> gets matrixInfo >>= \case
          (viewMatrixInfo -> Just (xs@(_:_) )) -> return xs
          _ -> calcSizes r

      es  <- withMatrixInfo (mkMatrixInfo rest) $ vectorOf3 numElems (dgive (GType ty))
      -- logStats $line (numElems, r , (vcat . map pretty $ es))
      return (AbsLitMatrix idx es, r)

      where
        calcSizes (TypeMatrix _ inn) = do
          cur  <- chooseChecked (Proxy :: Proxy a) (0,5)
          idx <- intDomainOfSize (fromIntegral cur)
          rest <- calcSizes inn
          return ((cur,idx) : rest)

        calcSizes _ = return []

    give1 (GType r@(TypeFunction t1 t2)) = do
      numElems <- chooseChecked (Proxy :: Proxy a) (0,5)
      e1 <- noVars $ vectorOf3 numElems (dgive (GType t1))
      e2 <- noVars $ vectorOf3 numElems (dgive (GType t2))
      let es = zip e1 e2
      return $ (AbsLitFunction es, r)

    give1 (GType r@(TypeRelation ts)) = do
      numElems <- chooseChecked (Proxy :: Proxy a) (0,5)
      ee :: [[a]] <- vectorOf3 numElems $ forM ts $ dgive . GType
      return $ (AbsLitRelation ee, r)

    give1 (GType r@(TypePartition t)) = do
      n <- chooseChecked (Proxy :: Proxy a) (0,5)
      es <- replicateM n $ do
              noVars $ boundedChecked (Proxy :: Proxy a) (1,5) (dgive (GType t))

      if all (null) es then
          logDebug2 $line  ["is Empty " <+> pretty (AbsLitPartition es)]
      else
          logDebug2 $line  ["not Empty " <+> pretty (AbsLitPartition es)]

      return $ (AbsLitPartition es, r)

    give1 (GType r@(TypeTuple ts)) = do
      es <- forM ts $ \t -> dgive (GType t)
      return $ (AbsLitTuple es, r)

    give1 t = giveUnmatched "Generate (AbstractLiteral a)" t

  give t = giveUnmatched "Generate (AbstractLiteral a)" t

  possiblePure _ (Just TypeBool) _ = False
  possiblePure _ (Just TypeInt ) _ = False
  possiblePure _ (Just ty) d       = fromIntegral d >= depthOf ty
  possiblePure _ Nothing d         = d >= 1

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []


boundedChecked :: (GenInfo a) => Proxy a -> (Int, Int) -> GenSt a -> GenSt [a]
boundedChecked proxy tu as = do
  n <- chooseChecked proxy tu
  vectorOf3 n as

chooseChecked :: (GenInfo a) => Proxy a -> (Int, Int) -> GenSt Int
chooseChecked proxy tu  | allowEmpty proxy = choose3 tu
chooseChecked _  (a,b)  =  choose3 (max 1 a, b)
