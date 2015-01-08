{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module AST.SpecE where

import qualified Data.Map as M
import Data.Map (Map)

import AST.Types
import AST.Domain
import AST.Expr
import AST.Helper
import AST.ToEssence
import AST.FromEssence

import Language.E

import GHC.Generics
import Data.Typeable


type Doms = Map Text FG
data SpecE = SpecE Doms [Expr]
    deriving(Show, Generic, Typeable, Read, Eq)



data FG = Find Domain
        | Given Domain
    deriving(Show, Generic, Typeable, Read, Eq)

instance Hashable FG

instance FromEssence (Text,FG) where
    fromEssence [xMatch| [Prim (S n)] := find.name.reference
                       | [dom]        := find.domain |] =
                           (\x -> (n,Find x)) <$> fromEssence dom
    fromEssence [xMatch| [Prim (S n)] := given.name.reference
                       | [dom]        := given.domain |] =
                           (\x -> (n,Given x)) <$> fromEssence dom
    fromEssence x = Left x

fromSpec :: Spec -> Either E SpecE
fromSpec (Spec _ x) = do
  decs' <- mapM fromEssence decs
  cons' <- mapM fromEssence cons
  return $ SpecE (M.fromList decs') cons'
  where decs = mapMaybe df $ statementAsList x
        cons = concat . mapMaybe cf $ statementAsList x
        df [xMatch| [y] := topLevel.declaration |] = Just y
        df _ = Nothing
        cf [xMatch| y := topLevel.suchThat |] = Just y
        cf _ = Nothing

domOfFG :: FG -> Domain
domOfFG (Find d) = d
domOfFG (Given d) = d

toSpec :: SpecE -> Spec
toSpec (SpecE fg exprs ) =
    let
        constraints = map mkConstraint exprs
        domains     = mkDomains fg
    in
        mkSpec $  domains ++ constraints

mkDomains :: Map Text FG -> [E]
mkDomains = map f .  M.toList
    where
    f (name, Find  dom) = mkFind  (mkName name, toEssence dom)
    f (name, Given dom) = mkGiven (mkName name, toEssence dom)

mkConstraint :: Expr -> E
mkConstraint expr =  [xMake| topLevel.suchThat := [toEssence expr ] |]

getFinds :: SpecE -> [(Text,Domain)]
getFinds (SpecE ds _) = mapMaybe f $ M.assocs ds where
    f (n,Find d) = Just (n,d)
    f _        = Nothing

getGivens :: SpecE -> [(Text,Domain)]
getGivens (SpecE ds _) = mapMaybe f $ M.assocs ds where
    f (n,Given d) = Just (n,d)
    f _         = Nothing


instance Pretty SpecE where
    pretty = pretty . toSpec

instance Pretty (M.Map Text FG) where
    pretty = vcat . map pretty . M.toList

instance Pretty FG where
    pretty (Find  d) = "Find"  <+>  pretty d
    pretty (Given d) = "Given" <+>  pretty d
