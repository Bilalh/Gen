{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable, FlexibleInstances, KindSignatures,
             MultiParamTypeClasses, QuasiQuotes #-}
module Gen.Essence.Info2 where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Expression.Op

import Test.QuickCheck
import Gen.AST.Imports
import Gen.AST.TH

data St = St
 deriving (Eq, Ord, Show, Data, Typeable, Generic)

type GenSt a = StateT St Gen a

class ArbitrarySt a where
  arbitrarySt :: GenSt a
  -- shrinkSt :: a -> [a]

instance ArbitrarySt (Op Expr) where
  arbitrarySt = do
      cons <- lift $ choose (0 :: Int, 0)
      case cons of
          -- 2 -> MkOpAnd <$> arbitrarySt
          0 -> MkOpGeq <$> arbitrarySt
          _ -> bug "no such constructor"

instance ArbitrarySt (OpGeq Expr) where
  arbitrarySt = pure OpGeq <*> arbitrarySt <*> arbitrarySt

instance ArbitrarySt Expr where
  arbitrarySt  = return $ ECon (ConstantInt 1)
