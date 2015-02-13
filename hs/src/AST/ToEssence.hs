{-# LANGUAGE MultiParamTypeClasses #-}
module AST.ToEssence where

class ToEssence ast conjure where
  toEssence :: ast -> conjure
