{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module TestGen.Reduce.Simpler where

import TestGen.Prelude

-- True if a1 is less simpler then a2
class Simpler a b where
    simpler :: (ToEssence a, Eq a, ToEssence b, Eq b) => a -> b -> Bool

instance Simpler a b where
    simpler _ _ = True

-- instance Simpler Expr Expr where
--     simpler (ELit a ) (ELit b)   = simpler a b
--     simpler (ELit a)  (EBinOp b) = simpler a b
--
--     -- simpler _ _ = False
--     simpler a b = error . show . vcat . map (pretty)  $ [toEssence a, toEssence b ]
--
-- instance Simpler Literal Literal where
--     simpler (EB _) (EB _) = False
--     simpler (EB _) _      = True
--
--     -- simpler _ _ = False
--     simpler a b = error . show . vcat . map (pretty)  $ [toEssence a, toEssence b ]
--
-- instance Simpler Literal BinOp where
--     simpler (EB _) _ = True
--     simpler (EI _) _ = True
--
--     -- simpler _ _ = False
--     simpler a b = error . show . vcat . map (pretty)  $ [toEssence a, toEssence b ]

