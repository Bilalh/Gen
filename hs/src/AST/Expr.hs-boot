{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AST.Expr where

import Conjure.Language.Pretty
import Conjure.Language.Definition

import AST.ToEssence
import AST.FromEssence
import AST.Data


instance ToEssence Expr Expression
instance FromEssence Expression Expr

instance Pretty Expr
instance Pretty BinOp
instance Pretty UniOp