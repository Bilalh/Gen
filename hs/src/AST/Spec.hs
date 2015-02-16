{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module AST.Spec where

import qualified Data.Map as M
import Data.Map (Map)

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Definition
import Conjure.Language.Domain

import AST.Data
import AST.Expr()


data Spec = Spec Domains [Expr] (Maybe OObjective)
    deriving(Show, Generic, Typeable, Eq)

data GF = Givenn (Domainn Expr)
        | Findd  (Domainn Expr)
    deriving(Show, Generic, Typeable, Eq)

type Domains = Map Text GF

data OObjective = Maximising Expr
                | Minimising Expr
    deriving(Eq, Ord, Show, Data, Typeable, Generic)


instance Pretty GF where
    pretty (Givenn x) = "Givenn " <+> pretty x
    pretty (Findd x)  = "Findd "  <+> pretty x

instance Translate Spec Model where
    toConjure = toModel
    fromConjure = fromModel

instance Pretty Spec where
    pretty = pretty . (toConjureNote "Pretty Spec" :: Spec -> Model )


instance Translate (Text, GF) (FindOrGiven, Name, Domain () Expression)  where
    fromConjure (x,(Name t),cdom) | x == Find || x == Given = do
      dom <- fromConjure cdom
      let kind = if x == Find then Findd else Givenn
      return $ (t, kind dom)

    fromConjure x = fail ("fromConjure Expr " <+>  pretty x <+> (pretty . groom) x)

    toConjure (t,(Findd dom)) = do
      cdom <- toConjure dom
      return (Find, Name t, cdom)

    toConjure (t,(Givenn dom)) = do
      cdom <- toConjure dom
      return (Find, Name t, cdom)


fromModel :: MonadFail m => Model -> m Spec
fromModel Model{mStatements} = do
  doms <- mapM fromConjure (mapMaybe getDoms mStatements)
  cs :: [Expr] <- mapM fromConjure . concat .  mapMaybe getCs $ mStatements
  return $ Spec (M.fromList doms) cs Nothing

    where
      getDoms (Declaration (FindOrGiven a b c) ) = Just (a, b, c)
      getDoms _ = Nothing

      getCs :: Statement -> Maybe [Expression]
      getCs (SuchThat xs) = Just (xs)
      getCs _ = Nothing


toModel :: MonadFail m => Spec -> m Model
toModel (Spec doms exprs obj) = do
    tuples   <- mapM toConjure (M.toList doms)
    let cdoms = map toDom tuples
    return $ def{mStatements=cdoms }

    where
      toDom (x,t,cdom) = Declaration $ FindOrGiven x t cdom

instance Pretty (Map Text GF) where
    pretty =  vcat . map pretty .  M.toList

domOfGF :: GF -> (Domainn Expr)
domOfGF (Givenn x) = x
domOfGF (Findd x)  = x
