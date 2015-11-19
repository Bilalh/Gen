{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.ModeMeta where

import Gen.Imports

import qualified Data.Aeson as A
import qualified Data.Set   as S

data ModeMeta = ModeMeta{
      no_chan :: S.Set String
    , compact :: S.Set String
    } deriving (Generic, Show, Eq)

instance A.FromJSON ModeMeta
instance A.ToJSON ModeMeta
instance Pretty ModeMeta where pretty = pretty . show
