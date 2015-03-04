{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Gen.AST.Expr where

import Conjure.Language.Pretty
import Conjure.Language.Definition

import Gen.AST.Data


instance Pretty Expr
instance Pretty BinOp
instance Pretty UniOp

instance Translate Expr Expression
instance Translate BinOp Expression where
