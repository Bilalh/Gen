{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AST.Literal where

import AST.Data

import Conjure.Language.Pretty
import Conjure.Language.Definition


instance Translate Literal Constant
instance Translate Literal Expression
instance Translate Literal (AbstractLiteral Expression)
instance Pretty Literal
