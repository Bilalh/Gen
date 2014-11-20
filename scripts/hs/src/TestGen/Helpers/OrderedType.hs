{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGen.Helpers.OrderedType where

import AST.Imports
import TestGen.Arbitrary.Data

class Ordered a where
    isOrdered :: a -> Bool
    
instance Ordered Type where
    isOrdered TInt          = True
    isOrdered TBool         = True
    isOrdered (TSet inner)  = isOrdered inner
    isOrdered (TMSet inner) = isOrdered inner
    isOrdered _             = False
