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
import Gen.Helpers.SizeOf

import qualified Data.Set as S


instance (Generate a, GenInfo a, EvalToInt a) => Generate (Domain () a) where
  give GNone = do
    k_int <- weightingForKey K_TypeInt
    ty <- if k_int <= 0 then
      withWeights [(K_TypeMSet,0), (K_TypeMatrix,0)] $ give GNone
    else
      give GNone
    logDepthCon $line (GType ty)
    give (GType ty)

  give (GType top) = addTypeKey top $ give1 (GType top)
    where
    give1 (GType TypeBool)             = pure DomainBool
    give1 (GType TypeInt)              = DomainInt   <$> vectorOf3 2 (dgive GNone)
    give1 (GType (TypeTuple t))        = DomainTuple <$> mapM (dgive <$> GType) t

    give1 (GType (TypeSet ty))         = DomainSet       <$> pure () <*> dgive GNone
                                                                     <*> dgive (GType ty)
    give1 (GType (TypeMSet ty))        = DomainMSet      <$> pure () <*> dgive GNone
                                                                     <*> dgive (GType ty)
    give1 (GType (TypeMatrix t1 t2))   = DomainMatrix    <$>             dgive (GType t1)
                                                                     <*> dgive (GType t2)
    give1 (GType (TypeFunction t1 t2)) = DomainFunction  <$> pure () <*> dgive GNone
                                                                     <*> dgive (GType t1)
                                                                     <*> dgive (GType t2)
    give1 (GType (TypePartition t))    = DomainPartition <$> pure () <*> dgive (GNone)
                                                                     <*> dgive (GType t)


    give1 ty@(GType (TypeRelation t@[a,b])) | a ==b = do
     elements3 $line [True, False] >>= \case
       False -> DomainRelation <$> pure () <*> dgive GNone <*> mapM (dgive <$> GType) t
       True -> do
         dom <- dgive (GType a)
         res <- DomainRelation <$> pure () <*> dgive GBinRel <*> pure [dom,dom]
         logInfo2 $line ["Binary Relation", nn "ty" ty, nn "dom" dom, nn "res" res ]
         return res


    give1 (GType (TypeRelation t)) = DomainRelation <$>  pure ()
                                                   <*>  dgive GNone
                                                   <*>  mapM (dgive <$> GType) t

    -- give1 (GType (TypeEnum t))         = _d
    -- give1 (GType (TypeUnnamed t))      = _d
    -- give1 (GType (TypeRecord t))       = _d
    -- give1 (GType (TypeVariant t))      = _d
    -- give1 (GType (TypeSequence t))     = _d
    give1 t = giveUnmatched "Generate (Domain () a)" t

  give top = giveUnmatched "Generate (Domain () a)" top

  possiblePure _ (Just TypeBool) _ = True
  possiblePure _ (Just ty)  d      = fromIntegral d >=  depthOf ty
  possiblePure _ Nothing _         = True

  requires _ (Just (TypeBool)) = [RAll $ [K_TypeBool]]
  requires _ (Just ty)         = [RAll $ keyList ty, RAll [K_TypeInt]]
  requires _ _                 = []


instance (Generate a, GenInfo a, EvalToInt a) => Generate (SetAttr a) where
  give GNone         = withKey K_SetAttr $  SetAttr <$> give (GNone)
  give t             = giveUnmatched "Generate (SetAttr a)" t
  possiblePure _ _ _ = True
  requires _ _        = []

instance (Generate a, GenInfo a, EvalToInt a) => Generate (MSetAttr a) where
  give GNone = do
    logDepthCon $line GNone
    (a,b) <- elements3 $line [ (GNone,GMsetAtrr), (GMsetAtrr,GNone)  ]
    withKey K_MSetAttr $  MSetAttr <$> give a <*> give b

  give t             = giveUnmatched "Generate (SetAttr a)" t
  possiblePure _ _ _ = True
  requires _ _       = []


instance (Generate a, GenInfo a, EvalToInt a) => Generate (SizeAttr a)  where
  give GNone     = withKey K_SizeAttr $ doSizeAttr True
  give GMsetAtrr = withKey K_SizeAttr $ doSizeAttr False

  give t = giveUnmatched "Generate (SetAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = [RAll [K_TypeInt]]


instance (Generate a, GenInfo a, EvalToInt a) => Generate (OccurAttr a) where
  give GNone     = withKey K_OccurAttr $ doOccurAttr True
  give GMsetAtrr = withKey K_OccurAttr $ doOccurAttr False

  give t = giveUnmatched "Generate (OccurAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = []


doSizeAttr :: forall a . (Generate a, GenInfo a, EvalToInt a)
           => Bool -> GenSt (SizeAttr a)
doSizeAttr allowNone = do
  sanity "Generate (SizeAttr a)"
  logInfo2 $line ["doSizeAttr", nn "allowNone" allowNone]

  k_int <- weightingForKey K_TypeInt
  let need _ = pure $ k_int > 0
      none _ = pure True

  let defs =
       if allowNone then
        [ ( none, (K_SizeAttr_None,    pure SizeAttr_None))
        , ( need, (K_SizeAttr_Size,    SizeAttr_Size <$> give (GType TypeInt)))
        , ( need, (K_SizeAttr_MinSize, SizeAttr_MinSize <$> give (GType TypeInt)))
        , ( need, (K_SizeAttr_MaxSize, SizeAttr_MaxSize <$> give (GType TypeInt)))
        , ( need, (K_SizeAttr_MinMaxSize,  (uncurry SizeAttr_MinMaxSize ) <$> minMax ))
        ]
        else
        [ ( need, (K_SizeAttr_Size,    SizeAttr_Size <$> give (GType TypeInt)))
        , ( need, (K_SizeAttr_MaxSize, SizeAttr_MaxSize <$> give (GType TypeInt)))
        , ( need, (K_SizeAttr_MinMaxSize,  (uncurry SizeAttr_MinMaxSize ) <$> minMax ))
        ]

  parts <- getPossibilitiesKeyed GNone defs


  res <- frequency3 parts
  logInfo2 $line ["doSizeAttr"]
  return res

  where
  minMax = do
    (IntAsc a b) <- give GNone
    return $ (a,b)

doOccurAttr :: forall a . (Generate a, GenInfo a, EvalToInt a)
           => Bool -> GenSt (OccurAttr a)
doOccurAttr allowNone = do
  logInfo2 $line ["doOccurAttr", nn "allowNone" allowNone]

  k_int <- weightingForKey K_TypeInt
  let need _ = pure $ k_int > 0
      none _ = pure True

  let defs =
       if allowNone then
        [ ( none, (K_OccurAttr_None,     pure OccurAttr_None))
        , ( need, (K_OccurAttr_MinOccur, OccurAttr_MinOccur <$> give (GType TypeInt)))
        , ( need, (K_OccurAttr_MaxOccur, OccurAttr_MaxOccur <$> give (GType TypeInt)))
        , ( need, (K_OccurAttr_MinMaxOccur,  (uncurry OccurAttr_MinMaxOccur ) <$> minMax ))
        ]
        else
        [ ( need, (K_OccurAttr_MaxOccur, OccurAttr_MaxOccur <$> give (GType TypeInt)))
        , ( need, (K_OccurAttr_MinMaxOccur,  (uncurry OccurAttr_MinMaxOccur ) <$> minMax ))
        ]

  parts <- getPossibilitiesKeyed GNone defs
  res <- frequency3 parts
  logInfo2 $line ["doOccurAttr"]
  return res

  where
  minMax = do
    (IntAsc a b) <- give GNone
    return $ (a,b)


instance (Generate a, GenInfo a, EvalToInt a) => Generate (FunctionAttr a) where
  give GNone         = withKey K_FunctionAttr $ FunctionAttr
                         <$> give GNone <*> give GNone <*> give GNone
  give t             = giveUnmatched "Generate (FunctionAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = []


instance Generate PartialityAttr where
  give GNone = withKey K_PartialityAttr $ frequency3 =<< getWeights
    [ (K_PartialityAttr_Partial, pure PartialityAttr_Partial)
    , (K_PartialityAttr_Total,   pure PartialityAttr_Total)
    ]

  give t = giveUnmatched "Generate (PartialityAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = []

instance Generate JectivityAttr where
  give GNone = withKey K_PartialityAttr $ frequency3 =<< getWeights
    [ (K_JectivityAttr_None,        pure JectivityAttr_None)
    , (K_JectivityAttr_Injective,   pure JectivityAttr_Injective)
    , (K_JectivityAttr_Surjective,  pure JectivityAttr_Surjective)
    , (K_JectivityAttr_Bijective,   pure JectivityAttr_Bijective)
    ]

  give t = giveUnmatched "Generate (JectivityAttr a)" t

  possiblePure _ _ _ = True
  requires _ _       = []


instance (Generate a, GenInfo a, EvalToInt a) => Generate (RelationAttr a) where
  give GNone         = withKey K_RelationAttr $ RelationAttr
                         <$> give (GNone) <*> give (GNone)
  give GBinRel       = withKey K_RelationAttr $ RelationAttr
                         <$> give (GNone) <*> give (GBinRel)
  give t             = giveUnmatched "Generate (RelationAttr a)" t
  possiblePure _ _ _ = True
  requires _ _       = []

instance Generate (BinaryRelationAttrs) where
  give GNone   = withKey K_BinaryRelationAttrs $
                   BinaryRelationAttrs <$> pure def
  give GBinRel = withKey K_BinaryRelationAttrs $
                   BinaryRelationAttrs <$> f

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

instance (Generate a, GenInfo a, EvalToInt a) => Generate (PartitionAttr a) where
  give GNone = withKey K_PartitionAttr $ do
      s1 <- give GNone
      s2 <- give GNone
      b  <- elements3 $line [True,False]
      return def{isRegular=b, partsNum = s1, partsSize=s2 }

  give t             = giveUnmatched "Generate (PartitionAttr a)" t
  possiblePure _ _ _ = True
  requires _ _       = []
