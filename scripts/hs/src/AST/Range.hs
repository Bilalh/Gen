{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module AST.Range where

import AST.FromEssence(FromEssence(..))
import AST.ToEssence(ToEssence(..))
import AST.Types
import {-# Source #-} AST.Expr
import {-# Source #-} AST.Literal

import Language.E


instance ToEssence (Range Expr) where
    toEssence (RFromTo l u) = [xMake| range.fromTo := [toEssence l, toEssence u  ] |]
    toEssence (RSingle i  ) = [xMake| range.single  := [toEssence i ] |]

instance FromEssence (Range Expr) where
    fromEssence [xMatch| [l,u] := range.fromTo |] = do
        l' <- fromEssence l
        u' <- fromEssence u
        return $ RFromTo l' u'
    fromEssence [xMatch| [i] := range.single |] = fromEssence i >>= return . RSingle
    fromEssence x = Left x

instance Pretty (Range Expr) where
    pretty p = pretty $ toEssence p
