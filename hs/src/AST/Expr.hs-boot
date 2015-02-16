{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AST.Expr where

import Conjure.Language.Pretty
import Conjure.Language.Definition

import AST.Data


instance Pretty Expr
instance Pretty BinOp
instance Pretty UniOp

instance Translate Expr Expression
instance Translate BinOp Expression where
