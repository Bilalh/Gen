{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Gen.AST.Expr where

import Conjure.Language.Pretty
import Conjure.Language.Definition
import Gen.AST.Data

instance Translate Expr Expression
instance Pretty Expr

instance ExpressionLike Expr