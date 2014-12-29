{-# OPTIONS_GHC -fno-warn-orphans #-}
module AST.Literal where

import AST.ToEssence(ToEssence(..))
import AST.FromEssence(FromEssence(..))
import AST.Types

import Language.E

instance ToEssence Literal
instance FromEssence Literal
instance Pretty Literal
