{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AST.Literal where

import AST.Data

import Conjure.Language.Pretty
import Conjure.Language.Domain
import Conjure.Language.Definition


instance ToEssence Literal (AbstractLiteral Expression)
instance FromEssence (AbstractLiteral Expression) Literal
instance Pretty Literal
