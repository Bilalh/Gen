{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AST.Literal where


import AST.ToEssence(ToEssence(..))
import AST.FromEssence(FromEssence(..))
import AST.Data
import {-# SOURCE #-} AST.Domain(dintRange)
import {-# SOURCE #-} AST.Expr()
import Language.E


instance ToEssence Literal where
    toEssence (EB x)      = [xMake| value.literal      := [Prim (B x)] |]
    toEssence (EI x)      = [xMake| value.literal      := [Prim (I x)] |]
    toEssence (ETuple xs) = [xMake| value.tuple.values := map toEssence xs |]

    toEssence (EMatrix xs r) = [xMake| value.matrix.values     := map toEssence xs
                                     | value.matrix.indexrange := [toEssence r]
                                     |]

    toEssence (ESet  xs) = [xMake| value.set.values  := map toEssence xs |]
    toEssence (EMSet xs) = [xMake| value.mset.values := map toEssence xs |]

    toEssence (EFunction xs) = [xMake| value.function.values := map helper xs |]
        where helper (a,b) = [xMake| mapping := [toEssence a, toEssence b] |]

    toEssence (ERelation xs)= [xMake| value.relation.values := map toEssence xs |]

    toEssence (EPartition xs) = [xMake| value.partition.values := mapMaybe helper xs |]
        where
              helper ys = Just [xMake| part :=  map toEssence ys |]

    toEssence (EExpr e) =  toEssence e

instance Pretty Literal where
    pretty = pretty . toEssence

instance FromEssence Literal where
    fromEssence (Prim (B x)) = return $ EB x

    fromEssence (Prim (I x)) = return $ EI x

    fromEssence [xMatch| [(Prim (I x))] := unaryOp.negate.value.literal |] =
        return $ EI (-x)
    fromEssence [xMatch| [x] := value.literal |] = fromEssence x
    fromEssence [xMatch| xs  := value.matrix.values
                       | [r] := value.matrix.indexrange |]
                       | Right jr <- fromEssence r  =
                         EMatrix <$> mapM fromEssence xs <*> return jr

    fromEssence [xMatch| xs  := value.matrix.values |] = do
                       EMatrix <$> mapM fromEssence xs <*> return (dintRange 1 (genericLength xs))

    fromEssence [xMatch| xs := value.tuple.values |] =
        ETuple <$> mapM fromEssence xs
    fromEssence [xMatch| xs := value.set.values |]   =
        ESet <$> mapM fromEssence xs
    fromEssence [xMatch| xs := value.mset.values |] =
        EMSet <$> mapM fromEssence xs

    fromEssence [xMatch| xs := value.function.values |] =
        EFunction <$> mapM helper xs
        where
            helper [xMatch| [a,b] := mapping |] =
                (,) <$> fromEssence a <*> fromEssence b
            helper x = Left x

    fromEssence [xMatch| xs := value.relation.values |] =
        ERelation  <$> mapM helper xs
        where
            helper :: E -> Either E Literal
            helper e@[xMatch| _ := value.tuple.values |] = fromEssence e
            helper x = Left x

    fromEssence [xMatch| xs  := value.partition.values |] =
        EPartition <$> mapM helper xs
        where
            helper [xMatch| ys  := part |] = mapM fromEssence ys
            helper x = Left x

    -- fromEssence x = case fromEssence x of
    --                       Right l -> return $ EExpr l
    --                       Left l  -> Left l
