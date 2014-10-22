{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Old.EssenceConstraints where

import Common.Helpers
import AST.ToEssence

import Language.E


instance ToEssence EssenceLiteral where
    toEssence lit = fromEssenceLiteral lit


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