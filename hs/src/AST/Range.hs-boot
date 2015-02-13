{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module AST.Range where

import AST.Data

import Conjure.Language.Pretty
import Conjure.Language.Domain
import Conjure.Language.Definition

instance ToEssence (RRange Expr) (Range Expression)
instance FromEssence (Range Expression) (RRange Expr)
instance Pretty (RRange Expr)
