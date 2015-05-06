{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Range where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Helpers.StandardImports


instance (Generate a, WrapConstant a) => Generate (Range a) where
  give GNone = do
      parts <- getWeights [("RangeSingle", single),("RangeBounded", bounded) ]
      frequency3 parts

    where
      single  = do
        a <- choose3 (0,5 :: Integer)
        return $ RangeSingle (wrapConstant . ConstantInt $ a)
      bounded = do
        a <- choose3 (0,5 :: Integer)
        b <- choose3 (a,5)
        return $ RangeBounded (wrapConstant . ConstantInt $ a)
                              (wrapConstant . ConstantInt $ b)

  give t = giveUnmatched "Generate (Range a)" t

  possiblePure _ _ _ = True
