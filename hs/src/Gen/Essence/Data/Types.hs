module Gen.Essence.Data.Types where

import Gen.Essence.St
import Gen.Imports

ordered = [K_TypeInt, K_TypeBool, K_TypeSet, K_TypeMSet]

unionLike = [K_TypeSet, K_TypeMSet, K_TypeFunction, K_TypeRelation]
hasLength = [K_TypeSet, K_TypeMSet, K_TypeFunction, K_TypeRelation, K_TypePartition]

literals = [K_TypeSet, K_TypeMSet, K_TypeTuple, K_TypeMatrix, K_TypeFunction, K_TypePartition, K_TypeRelation]

isUnionLike :: Type -> Bool
isUnionLike TypeSet{}      = True
isUnionLike TypeMSet{}     = True
isUnionLike TypeFunction{} = True
isUnionLike TypeRelation{} = True
isUnionLike _              = False

isLiteral :: Type -> Bool
isLiteral TypeTuple{}     = True
isLiteral TypeSet{}       = True
isLiteral TypeMSet{}      = True
isLiteral TypeMatrix{}    = True
isLiteral TypeFunction{}  = True
isLiteral TypeRelation{}  = True
isLiteral TypePartition{} = True
isLiteral _               = False


ordered, unionLike, literals :: [Key]
