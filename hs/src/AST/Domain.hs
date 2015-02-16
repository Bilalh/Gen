{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AST.Domain where

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Domain
import Conjure.Language.Definition
import Conjure.Prelude

import AST.Data
import {-# SOURCE #-} AST.Expr()

instance Translate (Domainn Expr) (Domain () Expression) where
    fromConjure x =  mapM f (Domainn  x)
        where
          f y = fromConjure y

    toConjure (Domainn x) = mapM toConjure x

instance Pretty (Domainn Expr) where
    pretty (Domainn x) = pretty x
