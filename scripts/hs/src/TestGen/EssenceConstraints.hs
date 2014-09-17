{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module TestGen.EssenceConstraints where

import Common.Helpers
import TestGen.ToEssence

import Language.E

data Eexpr =
       Egt  Eexpr Eexpr
     | Eneq Eexpr Eexpr
     | Evar Text
     | Elit EssenceLiteral
     deriving (Show,Eq)


instance ToEssence Eexpr where
    toEssence (Egt a b) = [eMake| &aa > &bb |]
        where
            aa = toEssence a
            bb = toEssence b

    toEssence (Eneq a b) = [eMake| &aa != &bb |]
        where
            aa = toEssence a
            bb = toEssence b

    toEssence (Evar (name) ) = mkName name
    toEssence (Elit lit )    = toEssence lit

mkConstraints :: [Eexpr] -> E
mkConstraints cons = [xMake| topLevel.suchThat := map toEssence cons |]
