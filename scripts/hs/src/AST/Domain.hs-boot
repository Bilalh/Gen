{-# OPTIONS_GHC -fno-warn-orphans #-}
module AST.Domain where

import AST.ToEssence(ToEssence(..))
import AST.FromEssence(FromEssence(..))
import AST.Data

import Language.E

instance ToEssence Domain
instance FromEssence Domain
instance Pretty Domain
