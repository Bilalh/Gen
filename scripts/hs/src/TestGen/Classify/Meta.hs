{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module TestGen.Classify.Meta where

import TestGen.Prelude
import TestGen.Classify.DomTypes

import TestGen.Reduce.Data

import GHC.Generics
import Data.Typeable

import qualified Data.Map as M

import Data.Aeson(FromJSON(..),ToJSON(..))

data Feature = Fquan
              | Fliterals   -- literal apart from int and bool
              | Fsum        -- quan sum
              | Findexing   -- tuple indexing, matrix slices etc
              | Fref        -- references variables
              | Ftyped
              | Fproc
              | FexprInLiteral
                deriving(Show, Generic, Typeable, Eq, Read)

data SpecMeta = SpecMeta
    {
      constraint_depth_ :: Integer
    , dom_depth_        :: Integer
    -- , error_kind     :: KindI
    -- , error_status_  :: StatusI
       , dom_types_     :: [Type]
    , features_         :: [Feature]
    }  deriving(Show, Generic, Typeable, Eq, Read)


instance FromJSON Feature
instance ToJSON Feature

instance FromJSON SpecMeta
instance ToJSON SpecMeta


makeMeta :: (WithDoms m) => m SpecMeta
makeMeta = do
  (SpecE ds cs) <- getSpecEWithDoms

  let doms              = map (domOfFG . snd) .  M.toList $ ds
      constraint_depth_ = maximum (map depthOf cs)
      dom_depth_        = maximum . (map depthOf) $ doms

  dom_types_ <- domTypes
  features_   <- findFeatures

  let sp = SpecMeta{..}
  return sp

specE1 :: SpecE
specE1= read $
    "SpecE (fromList [(\"var1\",Find (DSet {size = Nothing, minSize = Nothing, maxSize = Nothing, inner = DBool}))]) [EBinOp (BOr (EBinOp (BEQ (ELit (ESet [EExpr (ELit (EB True))])) (ELit (ESet [EExpr (ELit (EB True)),EExpr (ELit (EB True))])))) (ELit (EB False)))]"


findFeatures :: (WithDoms m) => m [Feature]
findFeatures = do
    (SpecE _ cs)  <- getSpecEWithDoms
    cs' <- mapM standardise cs
    let fs =  concatMap getFeatures cs'
    return $ nub fs  -- TODO use nub2

class HasFeature e where
    getFeatures :: e -> [Feature]

instance HasFeature Expr where
    getFeatures (ELit e)            = Fliterals : getFeatures e
    getFeatures (EVar _)            = [Fref]
    getFeatures (EQVar _)           = [Fref]
    getFeatures (EBinOp e)          = getFeatures e
    getFeatures (EUniOp e)          = getFeatures e
    getFeatures (EProc e)           = Fproc : getFeatures e
    getFeatures (EDom e)            = getFeatures e
    getFeatures (ETyped _ e2)      = Ftyped : getFeatures e2
    getFeatures EEmptyGuard         = []
    getFeatures (EQuan _ e2 e3 e4) = Fquan : (concat
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
