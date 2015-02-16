{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module AST.Literal where

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Domain
import Conjure.Language.Definition


import AST.Data
import {-# SOURCE #-} AST.Expr()




-- instance ToEssence Literal (AbstractLiteral Expression) where
    -- toEssence (EB x)      = [xMake| value.literal      := [Prim (B x)] |]
    -- toEssence (EI x)      = [xMake| value.literal      := [Prim (I x)] |]
    -- toEssence (ETuple xs) = [xMake| value.tuple.values := map toEssence xs |]

    -- toEssence (EMatrix xs r) = [xMake| value.matrix.values     := map toEssence xs
    --                                  | value.matrix.indexrange := [toEssence r]
    --                                  |]

    -- toEssence (ESet  xs) = [xMake| value.set.values  := map toEssence xs |]
    -- toEssence (EMSet xs) = [xMake| value.mset.values := map toEssence xs |]

    -- toEssence (EFunction xs) = [xMake| value.function.values := map helper xs |]
    --     where helper (a,b) = [xMake| mapping := [toEssence a, toEssence b] |]

    -- toEssence (ERelation xs)= [xMake| value.relation.values := map toEssence xs |]

    -- toEssence (EPartition xs) = [xMake| value.partition.values := mapMaybe helper xs |]
    --     where
    --           helper ys = Just [xMake| part :=  map toEssence ys |]

    -- toEssence (EExpr e) =  toEssence e


-- instance FromEssence (AbstractLiteral Expression) Literal where
    -- fromEssence (Prim (B x)) = return $ EB x

    -- fromEssence (Prim (I x)) = return $ EI x

    -- fromEssence [xMatch| [(Prim (I x))] := unaryOp.negate.value.literal |] =
    --     return $ EI (-x)
    -- fromEssence [xMatch| [x] := value.literal |] = fromEssence x
    -- fromEssence [xMatch| xs  := value.matrix.values
    --                    | [r] := value.matrix.indexrange |]
    --                    | Right jr <- fromEssence r  =
    --                      EMatrix <$> mapM fromEssence xs <*> return jr

    -- fromEssence [xMatch| xs  := value.matrix.values |] = do
    --                    EMatrix <$> mapM fromEssence xs <*> return (dintRange 1 (genericLength xs))

    -- fromEssence [xMatch| xs := value.tuple.values |] =
    --     ETuple <$> mapM fromEssence xs
    -- fromEssence [xMatch| xs := value.set.values |]   =
    --     ESet <$> mapM fromEssence xs
    -- fromEssence [xMatch| xs := value.mset.values |] =
    --     EMSet <$> mapM fromEssence xs

    -- fromEssence [xMatch| xs := value.function.values |] =
    --     EFunction <$> mapM helper xs
    --     where
    --         helper [xMatch| [a,b] := mapping |] =
    --             (,) <$> fromEssence a <*> fromEssence b
    --         helper x = Left x

    -- fromEssence [xMatch| xs := value.relation.values |] =
    --     ERelation  <$> mapM helper xs
    --     where
    --         helper :: E -> Either E Literal
    --         helper e@[xMatch| _ := value.tuple.values |] = fromEssence e
    --         helper x = Left x

    -- fromEssence [xMatch| xs  := value.partition.values |] =
    --     EPartition <$> mapM helper xs
    --     where
    --         helper [xMatch| ys  := part |] = mapM fromEssence ys
    --         helper x = Left x

    -- fromEssence x = EExpr <$> fromEssence x


instance Translate Literal Constant where
  fromConjure (ConstantBool r)        = return $ EB r
  fromConjure (ConstantInt r)         = return $ EI (fromIntegral r)
  fromConjure x = fail ("fromConjure Expr " <+>  pretty x <+> (pretty . groom) x)

  -- fromConjure (ConstantEnum r1 r2 r3) = _r
  -- fromConjure (ConstantAbstract r)    = _r
  -- fromConjure (DomainInConstant r)    = _r
  -- fromConjure (ConstantUndefined r)   = _r

  toConjure (EB x) = pure $ ConstantBool x
  toConjure (EI x) = pure $ ConstantInt (fromInteger x)
  toConjure x = error . renderNormal $ ("toConjure Expr " <+>  (pretty . groom) x)

instance Pretty Literal where
    pretty (EB x) = pretty $ ConstantBool x
    pretty (EI x) = pretty $ ConstantInt (fromInteger x)
    pretty x = error . renderNormal $ ("toConjure Expr " <+>  (pretty . groom) x)
