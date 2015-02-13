{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AST.Range where

-- import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Domain
import Conjure.Language.Definition

import AST.Data
import {-# Source #-} AST.Expr()
import {-# Source #-} AST.Literal()



instance ToEssence (RRange Expr) (Range Expression) where
    toEssence (RFromTo l u) = RangeBounded (toEssence l) (toEssence u)
    toEssence (RSingle i ) = RangeSingle (toEssence i)

instance FromEssence (Range Expression) (RRange Expr) where
--     fromEssence [xMatch| [l,u] := range.fromTo |] = do
--         l' <- fromEssence l
--         u' <- fromEssence u
--         return $ RFromTo l' u'
--     fromEssence [xMatch| [i] := range.single |] = fromEssence i >>= return . RSingle
--     fromEssence x = Left x

instance Pretty (RRange Expr) where
    -- pretty p = pretty $ toEssence p
