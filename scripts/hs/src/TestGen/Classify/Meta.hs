{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, LambdaCase, MultiWayIf #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module TestGen.Classify.Meta where

import TestGen.Prelude
import TestGen.Classify.DomTypes
import TestGen.Reduce.Simpler

import GHC.Generics
import Data.Typeable

import qualified Data.Map as M

import Data.Aeson(FromJSON(..),ToJSON(..))

import qualified Data.Foldable as F

data Feature = Fquan
              | FexprInLiteral
              | Findexing       -- tuple indexing, matrix slices etc
              | Fliterals       -- literal apart from int and bool
              | FnoConstraints
              | Fproc
              | Fref            -- references variables
              | Fsum            -- quan sum
              | Ftyped
                deriving(Show, Generic, Typeable, Eq, Read)

data SpecMeta = SpecMeta
    {
        constraint_depth_ :: Integer
      , constraint_count_ :: Int
      , dom_count_        :: Int
      , dom_depth_        :: Integer
      , dom_most_complex_ :: Type
      , dom_types_        :: [Type]
      , features_         :: [Feature]
    }  deriving(Show, Generic, Typeable, Eq, Read)


instance FromJSON Feature
instance ToJSON Feature

instance FromJSON SpecMeta
instance ToJSON SpecMeta

instance Hashable Feature
instance Hashable SpecMeta

mkMeta :: (WithDoms m) => m SpecMeta
mkMeta = do
  (SpecE ds cs) <- getSpecEWithDoms

  let doms              = map (domOfFG . snd) .  M.toList $ ds
      constraint_depth_ = maximum' 0 (map depthOf cs)
      dom_depth_        = maximum' 0 $ (map depthOf) doms
      constraint_count_ = length cs
      dom_count_        = M.size ds

  dom_types_        <- domTypes
  features_         <- findFeatures
  dom_most_complex_ <- maximumByM complex1 dom_types_


  let sp = SpecMeta{..}
  return sp


complex1 :: (WithDoms m) => Type -> Type -> m Ordering
complex1 t1 t2 = do
  let a = depthOf t1
      b = depthOf t2

  if | a == b -> return EQ
     | a <  b -> return LT
     | a >  b -> return GT


complex :: (WithDoms m) => Type -> Type -> m Ordering
complex t1 t2 = do
  a <- nullLogs $ simpler t1 t1
  b <- nullLogs $ simpler t2 t1
  case (a, b) of
    (False, False) -> return EQ
    (True,  True)  -> return EQ

    (False, True)  -> return GT
    (True,  False) -> return LT


maximumByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m a
maximumByM _ [] = error "maximumByM empty list"
maximumByM cmp l@(c:_) = F.foldrM max' c l
  where max' x y = cmp x y >>= \case
                        GT -> return x
                        _  -> return y


maximum' :: Ord a => a -> [a] -> a
maximum' a [] = a
maximum' _ xs = maximum xs

specE1 :: SpecE
specE1= read $
    "SpecE (fromList [(\"var1\",Find (DSet {size = Nothing, minSize = Nothing, maxSize = Nothing, inner = DBool}))]) [EBinOp (BOr (EBinOp (BEQ (ELit (ESet [EExpr (ELit (EB True))])) (ELit (ESet [EExpr (ELit (EB True)),EExpr (ELit (EB True))])))) (ELit (EB False)))]"


findFeatures :: (WithDoms m) => m [Feature]
findFeatures = do
    (SpecE _ cs)  <- getSpecEWithDoms
    cs' <- mapM standardise cs
    let fs = concatMap getFeatures cs' ++
              case cs of
                [] -> [FnoConstraints]
                _  -> []

    return $ nub fs

class HasFeature e where
    getFeatures :: e -> [Feature]

instance HasFeature Expr where
    getFeatures (ELit e)             = Fliterals : getFeatures e
    getFeatures (EVar _)             = [Fref]
    getFeatures (EQVar _)            = [Fref]
    getFeatures (EBinOp e)           = getFeatures e
    getFeatures (EUniOp e)           = getFeatures e
    getFeatures (EProc e)            = Fproc : getFeatures e
    getFeatures (EDom e)             = getFeatures e
    getFeatures (ETyped _ e2)        = Ftyped : getFeatures e2
    getFeatures EEmptyGuard          = []
    getFeatures (EQuan Sum e2 e3 e4) = Fsum : (concat
          [getFeatures  e2, getFeatures  e3, getFeatures e4] )
    getFeatures (EQuan _  e2 e3 e4)  = Fquan : (concat
          [getFeatures  e2, getFeatures  e3, getFeatures e4] )



instance HasFeature UniOp where
  getFeatures (UBar e) = getFeatures e
  getFeatures (UNeg e) = getFeatures e

instance HasFeature BinOp where
    getFeatures (BIn e1 e2)        = getFeatures e1 ++ getFeatures e2
    getFeatures (BOver e1 e2)      = getFeatures e1 ++ getFeatures e2
    getFeatures (BEQ e1 e2)        = getFeatures e1 ++ getFeatures e2
    getFeatures (BNEQ e1 e2)       = getFeatures e1 ++ getFeatures e2
    getFeatures (BLT e1 e2)        = getFeatures e1 ++ getFeatures e2
    getFeatures (BLTE e1 e2)       = getFeatures e1 ++ getFeatures e2
    getFeatures (BGT e1 e2)        = getFeatures e1 ++ getFeatures e2
    getFeatures (BGTE e1 e2)       = getFeatures e1 ++ getFeatures e2
    getFeatures (BDiff e1 e2)      = getFeatures e1 ++ getFeatures e2
    getFeatures (BPlus e1 e2)      = getFeatures e1 ++ getFeatures e2
    getFeatures (BMult e1 e2)      = getFeatures e1 ++ getFeatures e2
    getFeatures (BDiv e1 e2)       = getFeatures e1 ++ getFeatures e2
    getFeatures (BPow e1 e2)       = getFeatures e1 ++ getFeatures e2
    getFeatures (BMod e1 e2)       = getFeatures e1 ++ getFeatures e2
    getFeatures (BAnd e1 e2)       = getFeatures e1 ++ getFeatures e2
    getFeatures (BOr e1 e2)        = getFeatures e1 ++ getFeatures e2
    getFeatures (Bimply e1 e2)     = getFeatures e1 ++ getFeatures e2
    getFeatures (Biff e1 e2)       = getFeatures e1 ++ getFeatures e2
    getFeatures (Bsubset e1 e2)    = getFeatures e1 ++ getFeatures e2
    getFeatures (BsubsetEq e1 e2)  = getFeatures e1 ++ getFeatures e2
    getFeatures (Bsupset e1 e2)    = getFeatures e1 ++ getFeatures e2
    getFeatures (BsupsetEq e1 e2)  = getFeatures e1 ++ getFeatures e2
    getFeatures (Bintersect e1 e2) = getFeatures e1 ++ getFeatures e2
    getFeatures (Bunion e1 e2)     = getFeatures e1 ++ getFeatures e2
    getFeatures (BlexLT e1 e2)     = getFeatures e1 ++ getFeatures e2
    getFeatures (BlexLTE e1 e2)    = getFeatures e1 ++ getFeatures e2
    getFeatures (BlexGT e1 e2)     = getFeatures e1 ++ getFeatures e2
    getFeatures (BlexGTE e1 e2)    = getFeatures e1 ++ getFeatures e2



instance HasFeature Proc where
    getFeatures (PallDiff p)         = getFeatures p
    getFeatures (Pindex p1 p2)       = getFeatures p1 ++ getFeatures p2
    getFeatures (Papply p1 p2)       = getFeatures p1 ++ (concatMap getFeatures p2)
    getFeatures (Pfreq p1 p2)        = getFeatures p1 ++ getFeatures p2
    getFeatures (Phist p1 p2)        = getFeatures p1 ++ getFeatures p2
    getFeatures (Pmax p)             = getFeatures p
    getFeatures (Pmin p)             = getFeatures p
    getFeatures (PtoInt p)           = getFeatures p
    getFeatures (PtoMSet p)          = getFeatures p
    getFeatures (PtoRelation p)      = getFeatures p
    getFeatures (PtoSet p)           = getFeatures p
    getFeatures (Pdefined p)         = getFeatures p
    getFeatures (Pimage p1 p2)       = getFeatures p1 ++ getFeatures p2
    getFeatures (Pinverse p1 p2)     = getFeatures p1 ++ getFeatures p2
    getFeatures (PpreImage p1 p2)    = getFeatures p1 ++ getFeatures p2
    getFeatures (Prange p)           = getFeatures p
    getFeatures (Papart p1 p2 p3)    = getFeatures p1 ++ getFeatures p2
                                    ++ getFeatures p3
    getFeatures (Pparts p)           = getFeatures p
    getFeatures (Pparty p1 p2)       = getFeatures p1 ++ getFeatures p2
    getFeatures (Pparticipants p)    = getFeatures p
    getFeatures (Ptogether p1 p2 p3) = getFeatures p1 ++ getFeatures p2
                                    ++ getFeatures p3

instance HasFeature Literal where
    getFeatures (EB _)         = []
    getFeatures (EI _)         = []
    getFeatures (ETuple l)     = concatMap getFeatures l
    getFeatures (EMatrix l1 _) = concatMap getFeatures l1
    getFeatures (ESet l)       = concatMap getFeatures l
    getFeatures (EMSet l)      = concatMap getFeatures l
    getFeatures (EFunction l)  = concatMap (\(a,b) ->  getFeatures a ++ getFeatures b) l
    getFeatures (ERelation l)  = concatMap getFeatures l
    getFeatures (EPartition l) = concatMap (concatMap getFeatures) l
    getFeatures (EExpr l)      = FexprInLiteral : getFeatures l


instance HasFeature Domain where
  getFeatures _ = []
