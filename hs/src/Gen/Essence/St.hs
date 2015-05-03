{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric #-}

module Gen.Essence.St where

import Conjure.Prelude
import Data.Data                hiding (Proxy)
import Data.Map                 (Map)
import Gen.AST.Imports
import Gen.Helpers.Placeholders (neverNote)
import Test.QuickCheck          (Gen)

import qualified Data.Map as M

class Data a => Generate a where
  give  :: GenerateConstraint -> GenSt a

  getId ::  a -> Key
  getId  = dataTypeName . dataTypeOf

  getWeighting :: MonadState St m => a -> m Int
  getWeighting a = do
      let key = getId a
      gets weighting >>= \kv ->
          case key `M.lookup` kv  of
            Nothing  -> return 100
            (Just x) -> return x



type GenSt a = StateT St Gen a
type Key = String
data GenerateConstraint = GNone
                        | GType TType
 deriving (Eq, Ord, Show, Data, Typeable, Generic)

data St = St{
       weighting :: Map String Int
    ,  depth     :: Int
    }
 deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Default St where
    def = St{
            weighting = def
          , depth = $(neverNote "No depth specified")
          }



weightingForKey :: MonadState St m => Key -> m Int
weightingForKey key = do
    gets weighting >>= \kv ->
        case key `M.lookup` kv  of
          Nothing  -> return 100
          (Just x) -> return x
