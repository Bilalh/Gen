module Gen.Essence.Data.Types where

import Gen.Essence.St
import Gen.Essence.Type ()
import Gen.Imports

ordered :: [Key]
ordered = [K_TypeInt, K_TypeBool, K_TypeSet, K_TypeMSet]

unionLike :: [Key]
unionLike = [K_TypeSet, K_TypeMSet, K_TypeFunction, K_TypeRelation]

isUnionLike :: Type -> Bool
isUnionLike TypeSet{}      = True
isUnionLike TypeMSet{}     = True
isUnionLike TypeFunction{} = True
isUnionLike TypeRelation{} = True
isUnionLike _              = False
