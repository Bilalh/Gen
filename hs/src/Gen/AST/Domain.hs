{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.AST.Domain where

import Conjure.Prelude
import Conjure.Language.Domain
import Conjure.Language.Definition

import Gen.AST.Data
import {-# SOURCE #-} Gen.AST.Expr()

instance Translate (Domainn Expr) (Domain () Expression) where
    fromConjure x =  mapM f x
        where
          f y = fromConjure y

    toConjure x = mapM toConjure x

dintRange :: Int -> Int -> Domainn Expr
dintRange a b = DomainInt [RangeBounded (ELit . EI $ fromIntegral a)
                                        (ELit . EI $ fromIntegral b)]
