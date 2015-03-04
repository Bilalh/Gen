{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Gen.AST.Literal where

import Gen.AST.Data

import Conjure.Language.Pretty
import Conjure.Language.Definition


instance Translate Literal Constant
instance Translate Literal Expression
instance Translate Literal (AbstractLiteral Expression)
instance Pretty Literal
