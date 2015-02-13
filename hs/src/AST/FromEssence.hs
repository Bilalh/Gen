{-# LANGUAGE MultiParamTypeClasses #-}
module AST.FromEssence where

import Conjure.Prelude

class FromEssence conjure ast where
  fromEssence :: conjure -> Either conjure ast
