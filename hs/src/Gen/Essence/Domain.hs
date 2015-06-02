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
import Gen.Essence.Id

import qualified Data.Set as S

instance (Generate a, WrapConstant a, EvalToInt a) => Generate (Domain () a) where
  give GNone = give GNone >>= \ty -> give (GType ty)

  give (GType TypeBool)             = pure DomainBool
  give (GType TypeInt)              = DomainInt   <$> vectorOf3 2 (dgive GNone)
  give (GType (TypeTuple t))        = DomainTuple <$> mapM (dgive <$> GType) t

  give (GType (TypeSet ty))         = DomainSet       <$> pure () <*> dgive GNone
                                                                  <*> dgive (GType ty)
  give (GType (TypeMSet ty))        = DomainMSet      <$> pure () <*> dgive GNone
                                                                  <*> dgive (GType ty)
  give (GType (TypeMatrix t1 t2))   = DomainMatrix    <$>             dgive (GType t1)
                                                                  <*> dgive (GType t2)
  give (GType (TypeFunction t1 t2)) = DomainFunction  <$> pure () <*> dgive GNone
                                                                  <*> dgive (GType t1)
                                                                  <*> dgive (GType t2)
  give (GType (TypePartition t))    = DomainPartition <$> pure () <*> dgive (GNone)
                                                                  <*> dgive (GType t)


  give (GType (TypeRelation t@[a,b])) | a ==b = do
   elements3 [True, False] >>= \case
     False -> DomainRelation <$> pure () <*> dgive GNone <*> mapM (dgive <$> GType) t
     True -> do
       dom <- dgive (GType a)
       DomainRelation <$> pure () <*> dgive GBinRel <*> pure [dom,dom]


  give (GType (TypeRelation t)) = DomainRelation <$>  pure ()
                                                 <*>  dgive GNone
                                                 <*>  mapM (dgive <$> GType) t

  -- give (GType (TypeEnum t))         = _d
  -- give (GType (TypeUnnamed t))      = _d
  -- give (GType (TypeRecord t))       = _d
  -- give (GType (TypeVariant t))      = _d
  -- give (GType (TypeSequence t))     = _d
  give t = giveUnmatched "Generate (Domain () a)" t

  possiblePure _ _ _ = True

  requires _ (Just TypeBool) = [RAll $ [K_TypeBool]]
  requires _ (Just ty)       = [RAll $ keyList ty, RAll [K_TypeInt]]
  requires _ _               = []


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (SetAttr a) where
  give GNone         = SetAttr <$> give (GNone)
  give t             = giveUnmatched "Generate (SetAttr a)" t
  possiblePure _ _ _ = True
  requires _ _        = [RAll [K_TypeInt]]

instance (Generate a, WrapConstant a, EvalToInt a) => Generate (MSetAttr a) where
  give GNone = do
    (a,b) <- elements3 [ (GNone,GMsetAtrr), (GMsetAtrr,GNone)  ]
    MSetAttr <$> give a <*> give b

  give t             = giveUnmatched "Generate (SetAttr a)" t
  possiblePure _ _ _ = True
  requires _ _       = [RAll [K_TypeInt]]


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (SizeAttr a)  where
  give GNone     = doSizeAttr True
  give GMsetAtrr = doSizeAttr False

  give t = giveUnmatched "Generate (SetAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = [RAll [K_TypeInt]]

doSizeAttr :: forall a . (Generate a, WrapConstant a, EvalToInt a)
           => Bool -> GenSt (SizeAttr a)
doSizeAttr allowNone = do
    sanity "Generate (SizeAttr a)"

    let defs = if allowNone then
                   [ (K_SizeAttr_None,    pure SizeAttr_None)
                   , (K_SizeAttr_Size,    SizeAttr_Size    <$> give (GType TypeInt))
                   , (K_SizeAttr_MinSize, SizeAttr_MinSize <$> give (GType TypeInt))
                   , (K_SizeAttr_MaxSize, SizeAttr_MaxSize <$> give (GType TypeInt))
                   , (K_SizeAttr_MinMaxSize, (uncurry SizeAttr_MinMaxSize )  <$> minMax)
                   ]
               else
                   [ (K_SizeAttr_Size,    SizeAttr_Size    <$> give (GType TypeInt))
                   , (K_SizeAttr_MaxSize, SizeAttr_MaxSize <$> give (GType TypeInt))
                   , (K_SizeAttr_MinMaxSize, (uncurry SizeAttr_MinMaxSize )  <$> minMax)
                   ]

    parts <- getWeights defs
    frequency3 parts

    where
    minMax = do
      (IntAsc a b) <- give GNone
      return $ (a,b)


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (OccurAttr a) where
  give GNone     = doOccurAttr True
  give GMsetAtrr = doOccurAttr False

  give t = giveUnmatched "Generate (OccurAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = [RAll [K_TypeInt]]


doOccurAttr :: forall a . (Generate a, WrapConstant a, EvalToInt a)
           => Bool -> GenSt (OccurAttr a)
doOccurAttr allowNone = do
  let defs = if allowNone then
                 [ (K_OccurAttr_None,     pure OccurAttr_None)
                 , (K_OccurAttr_MinOccur, OccurAttr_MinOccur <$> give (GType TypeInt))
                 , (K_OccurAttr_MaxOccur, OccurAttr_MaxOccur <$> give (GType TypeInt))
                 , (K_OccurAttr_MinMaxOccur, (uncurry OccurAttr_MinMaxOccur ) <$> minMax)
                 ]
               else
                 [ (K_OccurAttr_MaxOccur, OccurAttr_MaxOccur <$> give (GType TypeInt))
                 , (K_OccurAttr_MinMaxOccur, (uncurry OccurAttr_MinMaxOccur ) <$> minMax)
                 ]


  parts <- getWeights defs
  frequency3 parts

  where
  minMax = do
    (IntAsc a b) <- give GNone
    return $ (a,b)




instance (Generate a, WrapConstant a, EvalToInt a) => Generate (FunctionAttr a) where
  give GNone         = FunctionAttr <$> give GNone <*> give GNone <*> give GNone
  give t             = giveUnmatched "Generate (FunctionAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = []

instance Generate PartialityAttr where
  give GNone = frequency3 =<< getWeights
    [ (K_PartialityAttr_Partial, pure PartialityAttr_Partial)
    , (K_PartialityAttr_Total,   pure PartialityAttr_Total)
    ]

  give t = giveUnmatched "Generate (PartialityAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = []


instance Generate JectivityAttr where
  give GNone = frequency3 =<< getWeights
    [ (K_JectivityAttr_None,        pure JectivityAttr_None)
    , (K_JectivityAttr_Injective,   pure JectivityAttr_Injective)
    , (K_JectivityAttr_Surjective,  pure JectivityAttr_Surjective)
    , (K_JectivityAttr_Bijective,   pure JectivityAttr_Bijective)
    ]

  give t = giveUnmatched "Generate (JectivityAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = []


instance (Generate a, WrapConstant a, EvalToInt a) => Generate (RelationAttr a) where
  give GNone         = RelationAttr <$> give (GNone) <*> give (GNone)
  give GBinRel       = RelationAttr <$> give (GNone) <*> give (GBinRel)
  give t             = giveUnmatched "Generate (RelationAttr a)" t
  possiblePure _ _ _ = True
  requires _ _       = [RAll [K_TypeInt]]

instance Generate (BinaryRelationAttrs) where
  give GNone   = BinaryRelationAttrs <$> pure def
  give GBinRel = BinaryRelationAttrs <$> f

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
  requires _ _       = []

instance (Generate a, WrapConstant a, EvalToInt a) => Generate (PartitionAttr a) where
  give GNone = do
      s1 <- give GNone
      s2 <- give GNone
      b  <- elements3 [True,False]
      return def{isRegular=b, partsNum = s1, partsSize=s2 }

  give t             = giveUnmatched "Generate (PartitionAttr a)" t
  possiblePure _ _ _ = True
  requires _ _       = [RAll [K_TypeInt]]
