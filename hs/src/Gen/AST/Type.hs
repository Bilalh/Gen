
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.AST.Type where

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Type
import Conjure.Language.Name

import Gen.AST.Data


instance Pretty TType where
    pretty = pretty . (toConjureNote "Pretty TType" :: TType -> Type)

instance Translate TType Type where
    toConjure TAny             = pure TypeAny
    toConjure TBool            = pure TypeBool
    toConjure TInt             = pure TypeInt
    toConjure (TMatix x)       = pure TypeMatrix    <*> return TypeInt
                                                    <*> toConjure x
    toConjure (TSet x)         = pure TypeSet       <*> toConjure x
    toConjure (TMSet x)        = pure TypeMSet      <*> toConjure x
    toConjure (TFunc x1 x2)    = pure TypeFunction  <*> toConjure x1
                                                    <*> toConjure x2
    toConjure (TTuple x)       = pure TypeTuple     <*> mapM toConjure x
    toConjure (TRel x)         = pure TypeRelation  <*> mapM toConjure x
    toConjure (TPar x)         = pure TypePartition <*> toConjure x
    toConjure (TUnamed x)      = pure TypeUnnamed   <*> toConjure x
    toConjure (TEnum x)        = pure TypeEnum      <*> toConjure x

    fromConjure TypeAny              = pure TAny
    fromConjure TypeBool             = pure TBool
    fromConjure TypeInt              = pure TInt
    fromConjure (TypeEnum x)         = pure TEnum   <*> fromConjure x
    fromConjure (TypeUnnamed x)      = pure TUnamed <*> fromConjure x
    fromConjure (TypeTuple x)        = pure TTuple  <*> mapM fromConjure x
    fromConjure (TypeList _)         = fail "whats a typeList?"
    fromConjure (TypeMatrix _ x2)    = pure TMatix  <*> fromConjure x2
    fromConjure (TypeSet x)          = pure TSet    <*> fromConjure x
    fromConjure (TypeMSet x)         = pure TMSet   <*> fromConjure x
    fromConjure (TypeFunction x1 x2) = pure TFunc   <*> fromConjure x1
                                                    <*> fromConjure x2
    fromConjure (TypeRelation x)     = pure TRel    <*> mapM fromConjure x
    fromConjure (TypePartition x)    = pure TPar    <*> fromConjure x


instance Translate Text Name where
    toConjure x          = pure $ Name x
    fromConjure (Name x) = pure x
