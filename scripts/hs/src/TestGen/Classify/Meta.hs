{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module TestGen.Classify.Meta where

import TestGen.Prelude
import TestGen.Helpers.Runner(StatusI,KindI)

import GHC.Generics
import Data.Typeable

import qualified Data.Map as M

import Data.Aeson(FromJSON(..),ToJSON(..))
import qualified Data.Aeson as A

data Features = Fquan
              | Fliterals   -- literal apart from int and bool
              | Fsum        -- quan sum
              | Findexing   -- tuple indexing, matrix slices etc
                deriving(Show, Generic, Typeable, Eq, Read)

data SpecMeta = SpecMeta
    {
      constraint_depth_ :: Integer
    , dom_depth_        :: Integer
    -- , error_kind     :: KindI
    -- , error_status_  :: StatusI
       , dom_types_     :: [Type]
    -- , features_      :: Features
    }  deriving(Show, Generic, Typeable, Eq, Read)



instance FromJSON Type
instance ToJSON Type
instance FromJSON SpecMeta
instance ToJSON SpecMeta


makeMeta :: Monad m =>  SpecE -> m SpecMeta
makeMeta (SpecE ds cs) = do

  let constraint_depth_ = maximum (map depthOf cs)
      dom_depth_        = maximum . (map depthOf) .  map (domOfFG . snd) .  M.toList $ ds

  let sp = SpecMeta{..}
  return sp

specE1 :: SpecE
specE1= read $
    "SpecE (fromList [(\"var1\",Find (DSet {size = Nothing, minSize = Nothing, maxSize = Nothing, inner = DBool}))]) [EBinOp (BOr (EBinOp (BEQ (ELit (ESet [EExpr (ELit (EB True))])) (ELit (ESet [EExpr (ELit (EB True)),EExpr (ELit (EB True))])))) (ELit (EB False)))]"
