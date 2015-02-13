{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module AST.SpecE where

import qualified Data.Map as M
import Data.Map (Map)

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Definition

import AST.Data
import AST.Expr()
-- import AST.Helper
-- import AST.ToEssence
import AST.FromEssence




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
fromSpec = error "fromSpec"
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
