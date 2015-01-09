{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module AST.Range where

import AST.ToEssence(ToEssence(..))
import AST.FromEssence(FromEssence(..))
import AST.Data
import {-# SOURCE #-} AST.Expr

import Language.E

instance ToEssence (Range Expr)
instance FromEssence (Range Expr)
instance Pretty (Range Expr)
