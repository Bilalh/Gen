module AST.ToEssence where

import Language.E

class ToEssence a where
  toEssence :: a -> E
