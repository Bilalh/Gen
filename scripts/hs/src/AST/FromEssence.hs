module AST.FromEssence where

import Language.E

class FromEssence a where
  fromEssence :: E -> Either E a
