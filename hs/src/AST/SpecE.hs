{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module AST.SpecE where

import qualified Data.Map as M
import Data.Map (Map)

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Definition
import Conjure.Language.Domain

import AST.Data
import AST.Expr()

import TestGen.Helpers.Placeholders (todo)

data GF = Givenn (Domainn Expr)
        | Findd  (Domainn Expr)
    deriving(Show, Generic, Typeable, Eq)

type Domains = Map Text GF

data Spec = Spec Domains [Expr] (Maybe OObjective)
    deriving(Show, Generic, Typeable, Eq)

instance Pretty GF where
    pretty (Givenn x) = "Givenn " <+> pretty x
    pretty (Findd x)  = "Findd "  <+> pretty x



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

instance Pretty Spec where
    pretty = pretty . (toConjureNote "Pretty Spec" :: Spec -> Model )

instance Translate Spec Model where
    toConjure = toModel
    fromConjure = fromModel

fromModel :: MonadFail m => Model -> m Spec
fromModel Model{mStatements} = do
  let cDoms = mapMaybe getDoms mStatements
  doms <- mapM fromConjure cDoms
  return $ Spec (M.fromList doms) [] Nothing

    where
      getDoms (Declaration (FindOrGiven a b c) ) = Just (a, b, c)
      getDoms _ = Nothing

toModel :: MonadFail m => Spec -> m Model
toModel (Spec doms exprs obj) = do
    tuples   <- mapM toConjure (M.toList doms)
    let doms = map toDom tuples
    return $ def{mStatements=doms }

    where
      toDom (x,t,cdom) = Declaration $ FindOrGiven x t cdom


type Doms = Map Text FG
data SpecE = SpecE Doms [Expr] (Maybe OObjective)
    deriving(Show, Generic, Typeable, Read, Eq)



data FG = FFind DDomain
        | GGiven DDomain
    deriving(Show, Generic, Typeable, Read, Eq)

instance Hashable FG

instance FromEssence (Text,FG) FindOrGiven where
    -- fromEssence [xMatch| [Prim (S n)] := find.name.reference
    --                    | [dom]        := find.domain |] =
    --                        (\x -> (n,Find x)) <$> fromEssence dom

    -- fromEssence [xMatch| [Prim (S n)] := given.name.reference
    --                    | [dom]        := given.domain |] =
    --                        (\x -> (n,Given x)) <$> fromEssence dom

    -- fromEssence x = Left x

fromSpec :: MonadFail m => Model -> m SpecE
fromSpec Model{mStatements} = do
 -- let decs'  mapM fromEssence decs
 decs' <- $(todo "fromSpec")
 return $ SpecE (M.fromList decs') [] Nothing

  where
    decs = mapMaybe df mStatements

    df (Declaration ret@(FindOrGiven Find _ x)) = Just ret
    df _ = Nothing

-- fromSpec (Spec _ x) = do
--   decs' <- mapM fromEssence decs
--   cons' <- mapM fromEssence cons
--   return $ SpecE (M.fromList decs') cons' (toObj obj)

--   where
--     decs = mapMaybe df $ statementAsList x
--     cons = concat . mapMaybe cf $ statementAsList x

--     df [xMatch| [y] := topLevel.declaration |] = Just y
--     df _ = Nothing

--     cf [xMatch| y := topLevel.suchThat |] = Just y
--     cf _ = Nothing

--     toObj Nothing  = Nothing
--     toObj (Just v) = case fromEssence v of
--                        Right o -> Just (o :: OObjective)
--                        Left  _ -> Nothing

--     obj = listToMaybe . mapMaybe f $ statementAsList x
--         where
--           f y@[xMatch| _ := topLevel.objective |] = Just y
--           f _ = Nothing

domOfFG :: FG -> DDomain
domOfFG (FFind d) = d
domOfFG (GGiven d) = d

toSpec :: SpecE -> Model
toSpec = error "toSpec"
-- toSpec (SpecE fg exprs obj) =
--     let
--         constraints = map mkConstraint exprs
--         domains     = mkDDomains fg
--         obje        = case obj of
--                         Nothing -> []
--                         Just x  -> [toEssence x]
--     in
--         mkSpec $  domains ++ constraints ++ obje

mkDomains :: Map Text FG -> [Statement]
mkDomains = error "mkDomains"
-- mkDomains = map f .  M.toList
--     where
--     f (name, Find  dom) = mkFind  (mkName name, toEssence dom)
--     f (name, Given dom) = mkGiven (mkName name, toEssence dom)

mkConstraint :: Expr -> Expression
mkConstraint = error "mkConstraint"
-- mkConstraint expr =  [xMake| topLevel.suchThat := [toEssence expr ] |]

getFinds :: SpecE -> [(Text,DDomain)]
getFinds (SpecE ds _ _) = mapMaybe f $ M.assocs ds where
    f (n,FFind d) = Just (n,d)
    f _        = Nothing

getGivens :: SpecE -> [(Text,DDomain)]
getGivens (SpecE ds _ _) = mapMaybe f $ M.assocs ds where
    f (n,GGiven d) = Just (n,d)
    f _         = Nothing


instance Pretty SpecE where
    -- pretty = pretty . toSpec

instance Pretty (M.Map Text FG) where
    -- pretty = vcat . map pretty . M.toList

instance Pretty FG where
    -- pretty (Find  d) = "Find"  <+>  pretty d
    -- pretty (Given d) = "Given" <+>  pretty d
