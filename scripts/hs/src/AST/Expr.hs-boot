{-# OPTIONS_GHC -fno-warn-orphans #-}
module AST.Expr where


import AST.ToEssence
import AST.FromEssence
import AST.Types

import Language.E

instance ToEssence Expr
instance FromEssence Expr

instance Pretty Expr
instance Pretty BinOp
instance Pretty UniOp