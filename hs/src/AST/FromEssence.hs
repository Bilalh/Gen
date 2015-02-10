module AST.FromEssence where

import Conjure.Prelude
import Conjure.Language.Definition(Expression)

class FromEssence a where
  fromEssence :: Expression -> Either Expression a
