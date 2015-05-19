 {-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Domain where

import Conjure.Language.Domain
import Gen.Essence.Ints
import Gen.Essence.Range
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type        ()
import Gen.Imports
import Gen.Essence.EvalToInt

import qualified Data.Set as S

instance (Generate a, WrapConstant a, EvalToInt a) => Generate (Domain () a) where
  give GNone = give GNone >>= \ty -> give (GType ty)

  give (GType TypeBool)             = pure DomainBool
  give (GType TypeInt)              = DomainInt   <$> vectorOf3 2 (give GNone)
  give (GType (TypeTuple t))        = DomainTuple <$> mapM (give <$> GType) t

  give (GType (TypeSet ty))         = DomainSet       <$> pure () <*> give GNone      <*> give (GType ty)
  give (GType (TypeMSet ty))        = DomainMSet      <$> pure () <*> give GNone      <*> give (GType ty)
  give (GType (TypeMatrix t1 t2))   = DomainMatrix    <$>              give (GType t1) <*> give (GType t2)
  give (GType (TypeFunction t1 t2)) = DomainFunction  <$> pure () <*> give GNone
                                                                  <*> give (GType t1) <*> give (GType t2)
  give (GType (TypeRelation t))     = DomainRelation  <$> pure () <*> give GNone
                                                                  <*> mapM (give <$> GType) t
  give (GType (TypePartition t))    = DomainPartition <$> pure () <*> give (GNone)
                                                                  <*> give (GType t)

  -- give (GType (TypeEnum t))         = _d
  -- give (GType (TypeUnnamed t))      = _d
  -- give (GType (TypeRecord t))       = _d
  -- give (GType (TypeVariant t))      = _d
  -- give (GType (TypeSequence t))     = _d
  give t = giveUnmatched "Generate (Domain () a)" t

  possiblePure _ _ _ = True
  possibleNoType _ _ = True


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (SetAttr a) where
  give GNone         = SetAttr <$> give (GNone)
  give t             = giveUnmatched "Generate (SetAttr a)" t
  possiblePure _ _ _ = True
  possibleNoType _ _ = True


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (MSetAttr a) where
  give GNone         = MSetAttr <$> give (GNone) <*> give (GNone)
  give t             = giveUnmatched "Generate (SetAttr a)" t
  possiblePure _ _ _ = True
  possibleNoType _ _ = True


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (SizeAttr a)  where
  give GNone = do
    let defs =
            [ (K_SizeAttr_None, pure SizeAttr_None)
            , (K_SizeAttr_Size,    SizeAttr_Size    <$> give (GType TypeInt))
            , (K_SizeAttr_MinSize, SizeAttr_MinSize <$> give (GType TypeInt))
            , (K_SizeAttr_MaxSize, SizeAttr_MaxSize <$> give (GType TypeInt))
            , (K_SizeAttr_MinMaxSize, (uncurry SizeAttr_MinMaxSize )  <$> minMax)
            ]

    parts <- getWeights defs
    frequency3 parts

    where
    minMax = do
      (IntAsc a b) <- give GNone
      return $ (a,b)

  give t = giveUnmatched "Generate (SetAttr a)" t

  possiblePure _ _ _ = True
  possibleNoType _ _ = True

instance (Generate a, WrapConstant a, EvalToInt a) => Generate (OccurAttr a) where
  give GNone = do
    let defs =
            [ (K_OccurAttr_None, pure OccurAttr_None)
            , (K_OccurAttr_MinOccur, OccurAttr_MinOccur <$> give (GType TypeInt))
            , (K_OccurAttr_MaxOccur, OccurAttr_MaxOccur <$> give (GType TypeInt))
            , (K_OccurAttr_MinMaxOccur, (uncurry OccurAttr_MinMaxOccur ) <$> minMax)
            ]

    parts <- getWeights defs
    frequency3 parts

    where
    minMax = do
      (IntAsc a b) <- give GNone
      return $ (a,b)

  give t = giveUnmatched "Generate (OccurAttr a)" t

  possiblePure _ _ _ = True
  possibleNoType _ _ = True

instance (Generate a, WrapConstant a, EvalToInt a) => Generate (FunctionAttr a) where
  give GNone         = FunctionAttr <$> give GNone <*> give GNone <*> give GNone
  give t             = giveUnmatched "Generate (FunctionAttr a)" t

  possiblePure _ _ _ = True
  possibleNoType _ _ = True


instance Generate PartialityAttr where
  give GNone = frequency3 =<< getWeights
    [ (K_PartialityAttr_Partial, pure PartialityAttr_Partial)
    , (K_PartialityAttr_Total,   pure PartialityAttr_Total)
    ]

  give t = giveUnmatched "Generate (PartialityAttr a)" t

  possiblePure _ _ _ = True
  possibleNoType _ _ = True



instance Generate JectivityAttr where
  give GNone = frequency3 =<< getWeights
    [ (K_JectivityAttr_None,        pure JectivityAttr_None)
    , (K_JectivityAttr_Injective,   pure JectivityAttr_Injective)
    , (K_JectivityAttr_Surjective,  pure JectivityAttr_Surjective)
    , (K_JectivityAttr_Bijective,   pure JectivityAttr_Bijective)
    ]

  give t = giveUnmatched "Generate (JectivityAttr a)" t

  possiblePure _ _ _ = True
  possibleNoType _ _ = True



instance (Generate a, WrapConstant a, EvalToInt a) => Generate (RelationAttr a) where
  give GNone         = RelationAttr <$> give (GNone) <*> give (GNone)
  give t             = giveUnmatched "Generate (RelationAttr a)" t
  possiblePure _ _ _ = True
  possibleNoType _ _ = True


instance Generate (BinaryRelationAttrs) where
  give GNone = BinaryRelationAttrs <$> f

    where
    f ::  GenSt (Set BinaryRelationAttr)
    f = (elemFreq3 =<< getWeights defs)  >>= \case
        Nothing  -> return S.empty
        (Just (k,v)) -> do
          vs <- withWeights [(k,0)] f
          return $  (S.singleton v) `S.union` vs

    kv k v = (k, Just (k,v))
    defs = [ (K_BinRelAttrStop, Nothing)
           , kv K_BinRelAttr_Reflexive      BinRelAttr_Reflexive
           , kv K_BinRelAttr_Irreflexive    BinRelAttr_Irreflexive
           , kv K_BinRelAttr_Coreflexive    BinRelAttr_Coreflexive
           , kv K_BinRelAttr_Symmetric      BinRelAttr_Symmetric
           , kv K_BinRelAttr_AntiSymmetric  BinRelAttr_AntiSymmetric
           , kv K_BinRelAttr_ASymmetric     BinRelAttr_ASymmetric
           , kv K_BinRelAttr_Transitive     BinRelAttr_Transitive
           , kv K_BinRelAttr_Total          BinRelAttr_Total
           , kv K_BinRelAttr_Connex         BinRelAttr_Connex
           , kv K_BinRelAttr_Euclidean      BinRelAttr_Euclidean
           , kv K_BinRelAttr_Serial         BinRelAttr_Serial
           , kv K_BinRelAttr_Equivalence    BinRelAttr_Equivalence
           , kv K_BinRelAttr_PartialOrder   BinRelAttr_PartialOrder
           ]


  give t             = giveUnmatched "Generate (Set BinaryRelationAttrs)" t
  possiblePure _ _ _ = True
  possibleNoType _ _ = True

instance (Generate a, WrapConstant a, EvalToInt a) => Generate (PartitionAttr a) where
  give GNone = do
      s1 <- give GNone
      s2 <- give GNone
      b  <- elements3 [True,False]
      return def{isRegular=b, partsNum = s1, partsSize=s2 }

  give t             = giveUnmatched "Generate (PartitionAttr a)" t
  possiblePure _ _ _ = True
  possibleNoType _ _ = True
