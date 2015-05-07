--This is an auto-generated file created by make keys
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Essence.Key where

import Gen.Helpers.StandardImports
import qualified Data.Aeson as A

data Key = K_Unused
         | K_AbsLitFunction
         | K_AbsLitMSet
         | K_AbsLitMatrix
         | K_AbsLitPartition
         | K_AbsLitRecord
         | K_AbsLitRelation
         | K_AbsLitSequence
         | K_AbsLitSet
         | K_AbsLitTuple
         | K_AbsLitVariant
         | K_AbstractLiteral
         | K_Comprehension
         | K_Constant
         | K_ConstantAbstract
         | K_ConstantBool
         | K_ConstantEnum
         | K_ConstantField
         | K_ConstantInt
         | K_ConstantUndefined
         | K_Domain
         | K_DomainAny
         | K_DomainBool
         | K_DomainEnum
         | K_DomainFunction
         | K_DomainInConstant
         | K_DomainInt
         | K_DomainIntEmpty
         | K_DomainMSet
         | K_DomainMatrix
         | K_DomainMetaVar
         | K_DomainOp
         | K_DomainPartition
         | K_DomainRecord
         | K_DomainReference
         | K_DomainRelation
         | K_DomainSequence
         | K_DomainSet
         | K_DomainTuple
         | K_DomainUnnamed
         | K_DomainVariant
         | K_EComp
         | K_ECon
         | K_EDom
         | K_EEmptyGuard
         | K_ELit
         | K_EMetaVar
         | K_EOp
         | K_EQuan
         | K_ETyped
         | K_EVar
         | K_ExpressionMetaVar
         | K_Op
         | K_RangeBounded
         | K_RangeLowerBounded
         | K_RangeOpen
         | K_RangeSingle
         | K_RangeUpperBounded
         | K_Reference
         | K_TypeAny
         | K_TypeBool
         | K_TypeEnum
         | K_TypeFunction
         | K_TypeInt
         | K_TypeList
         | K_TypeMSet
         | K_TypeMatrix
         | K_TypePartition
         | K_TypeRecord
         | K_TypeRelation
         | K_TypeSequence
         | K_TypeSet
         | K_TypeTuple
         | K_TypeUnnamed
         | K_TypeVariant
         | K_Typed
         | K_TypedConstant
         | K_WithLocals
     deriving (Eq, Ord, Show, Data, Typeable, Generic)


instance ToJSON Key where
  toJSON = A.String . stringToText . tail . tail . show

instance FromJSON Key where
  parseJSON (A.String s) = return $ fromString . textToString $ s
  parseJSON _            = mzero


instance Pretty Key where
    pretty = pretty . show

instance IsString Key where
    fromString "AbsLitFunction"          = K_AbsLitFunction
    fromString "AbsLitMSet"              = K_AbsLitMSet
    fromString "AbsLitMatrix"            = K_AbsLitMatrix
    fromString "AbsLitPartition"         = K_AbsLitPartition
    fromString "AbsLitRecord"            = K_AbsLitRecord
    fromString "AbsLitRelation"          = K_AbsLitRelation
    fromString "AbsLitSequence"          = K_AbsLitSequence
    fromString "AbsLitSet"               = K_AbsLitSet
    fromString "AbsLitTuple"             = K_AbsLitTuple
    fromString "AbsLitVariant"           = K_AbsLitVariant
    fromString "AbstractLiteral"         = K_AbstractLiteral
    fromString "Comprehension"           = K_Comprehension
    fromString "Constant"                = K_Constant
    fromString "ConstantAbstract"        = K_ConstantAbstract
    fromString "ConstantBool"            = K_ConstantBool
    fromString "ConstantEnum"            = K_ConstantEnum
    fromString "ConstantField"           = K_ConstantField
    fromString "ConstantInt"             = K_ConstantInt
    fromString "ConstantUndefined"       = K_ConstantUndefined
    fromString "Domain"                  = K_Domain
    fromString "DomainAny"               = K_DomainAny
    fromString "DomainBool"              = K_DomainBool
    fromString "DomainEnum"              = K_DomainEnum
    fromString "DomainFunction"          = K_DomainFunction
    fromString "DomainInConstant"        = K_DomainInConstant
    fromString "DomainInt"               = K_DomainInt
    fromString "DomainIntEmpty"          = K_DomainIntEmpty
    fromString "DomainMSet"              = K_DomainMSet
    fromString "DomainMatrix"            = K_DomainMatrix
    fromString "DomainMetaVar"           = K_DomainMetaVar
    fromString "DomainOp"                = K_DomainOp
    fromString "DomainPartition"         = K_DomainPartition
    fromString "DomainRecord"            = K_DomainRecord
    fromString "DomainReference"         = K_DomainReference
    fromString "DomainRelation"          = K_DomainRelation
    fromString "DomainSequence"          = K_DomainSequence
    fromString "DomainSet"               = K_DomainSet
    fromString "DomainTuple"             = K_DomainTuple
    fromString "DomainUnnamed"           = K_DomainUnnamed
    fromString "DomainVariant"           = K_DomainVariant
    fromString "EComp"                   = K_EComp
    fromString "ECon"                    = K_ECon
    fromString "EDom"                    = K_EDom
    fromString "EEmptyGuard"             = K_EEmptyGuard
    fromString "ELit"                    = K_ELit
    fromString "EMetaVar"                = K_EMetaVar
    fromString "EOp"                     = K_EOp
    fromString "EQuan"                   = K_EQuan
    fromString "ETyped"                  = K_ETyped
    fromString "EVar"                    = K_EVar
    fromString "ExpressionMetaVar"       = K_ExpressionMetaVar
    fromString "Op"                      = K_Op
    fromString "RangeBounded"            = K_RangeBounded
    fromString "RangeLowerBounded"       = K_RangeLowerBounded
    fromString "RangeOpen"               = K_RangeOpen
    fromString "RangeSingle"             = K_RangeSingle
    fromString "RangeUpperBounded"       = K_RangeUpperBounded
    fromString "Reference"               = K_Reference
    fromString "TypeAny"                 = K_TypeAny
    fromString "TypeBool"                = K_TypeBool
    fromString "TypeEnum"                = K_TypeEnum
    fromString "TypeFunction"            = K_TypeFunction
    fromString "TypeInt"                 = K_TypeInt
    fromString "TypeList"                = K_TypeList
    fromString "TypeMSet"                = K_TypeMSet
    fromString "TypeMatrix"              = K_TypeMatrix
    fromString "TypePartition"           = K_TypePartition
    fromString "TypeRecord"              = K_TypeRecord
    fromString "TypeRelation"            = K_TypeRelation
    fromString "TypeSequence"            = K_TypeSequence
    fromString "TypeSet"                 = K_TypeSet
    fromString "TypeTuple"               = K_TypeTuple
    fromString "TypeUnnamed"             = K_TypeUnnamed
    fromString "TypeVariant"             = K_TypeVariant
    fromString "Typed"                   = K_Typed
    fromString "TypedConstant"           = K_TypedConstant
    fromString "WithLocals"              = K_WithLocals
    fromString t = error $ "Unknown Key: " ++ t ++
                  "\n Add new keys to src/Gen/Essence/key_extra_names.txt if needed."
