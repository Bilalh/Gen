{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NamedFieldPuns #-}
module AST.ToEssence where


import Language.E

class ToEssence a where
  toEssence :: a -> E
