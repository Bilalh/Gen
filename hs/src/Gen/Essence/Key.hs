--This is an auto-generated file created by make keys
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Essence.Key where

import Gen.Imports
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
         | K_JectivityAttr_Bijective
         | K_JectivityAttr_Injective
         | K_JectivityAttr_None
         | K_JectivityAttr_Surjective
         | K_LVar
         | K_OccurAttr_MaxOccur
         | K_OccurAttr_MinMaxOccur
         | K_OccurAttr_MinOccur
         | K_OccurAttr_None
         | K_Op
         | K_OpActive
         | K_OpAllDiff
         | K_OpAnd
         | K_OpApart
         | K_OpAttributeAsConstraint
         | K_OpDefined
         | K_OpDiv
         | K_OpDontCare
         | K_OpDotLeq
         | K_OpDotLt
         | K_OpEq
         | K_OpFactorial
         | K_OpFlatten
         | K_OpFreq
         | K_OpGeq
         | K_OpGt
         | K_OpHist
         | K_OpIff
         | K_OpImage
         | K_OpImageSet
         | K_OpImply
         | K_OpIn
         | K_OpIndexing
         | K_OpIntersect
         | K_OpInverse
         | K_OpLeq
         | K_OpLexLeq
         | K_OpLexLt
         | K_OpLt
         | K_OpMax
         | K_OpMin
         | K_OpMinus
         | K_OpMod
         | K_OpNegate
         | K_OpNeq
         | K_OpNot
         | K_OpOr
         | K_OpParticipants
         | K_OpParts
         | K_OpParty
         | K_OpPow
         | K_OpPowerSet
         | K_OpPreImage
         | K_OpPred
         | K_OpProduct
         | K_OpRange
         | K_OpRelationProj
         | K_OpRestrict
         | K_OpSlicing
         | K_OpSubsequence
         | K_OpSubset
         | K_OpSubsetEq
         | K_OpSubstring
         | K_OpSucc
         | K_OpSum
         | K_OpSupset
         | K_OpSupsetEq
         | K_OpTildeLeq
         | K_OpTildeLt
         | K_OpToInt
         | K_OpToMSet
         | K_OpToRelation
         | K_OpToSet
         | K_OpTogether
         | K_OpTrue
         | K_OpTwoBars
         | K_OpUnion
         | K_PartialityAttr_Partial
         | K_PartialityAttr_Total
         | K_PartitionAttr
         | K_RangeBounded
         | K_RangeLowerBounded
         | K_RangeOpen
         | K_RangeSingle
         | K_RangeUpperBounded
         | K_Reference
         | K_SizeAttr_MaxSize
         | K_SizeAttr_MinMaxSize
         | K_SizeAttr_MinSize
         | K_SizeAttr_None
         | K_SizeAttr_Size
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
  fromString "JectivityAttr_Bijective" = K_JectivityAttr_Bijective
  fromString "JectivityAttr_Injective" = K_JectivityAttr_Injective
  fromString "JectivityAttr_None"      = K_JectivityAttr_None
  fromString "JectivityAttr_Surjective" = K_JectivityAttr_Surjective
  fromString "LVar"                    = K_LVar
  fromString "OccurAttr_MaxOccur"      = K_OccurAttr_MaxOccur
  fromString "OccurAttr_MinMaxOccur"   = K_OccurAttr_MinMaxOccur
  fromString "OccurAttr_MinOccur"      = K_OccurAttr_MinOccur
  fromString "OccurAttr_None"          = K_OccurAttr_None
  fromString "Op"                      = K_Op
  fromString "OpActive"                = K_OpActive
  fromString "OpAllDiff"               = K_OpAllDiff
  fromString "OpAnd"                   = K_OpAnd
  fromString "OpApart"                 = K_OpApart
  fromString "OpAttributeAsConstraint" = K_OpAttributeAsConstraint
  fromString "OpDefined"               = K_OpDefined
  fromString "OpDiv"                   = K_OpDiv
  fromString "OpDontCare"              = K_OpDontCare
  fromString "OpDotLeq"                = K_OpDotLeq
  fromString "OpDotLt"                 = K_OpDotLt
  fromString "OpEq"                    = K_OpEq
  fromString "OpFactorial"             = K_OpFactorial
  fromString "OpFlatten"               = K_OpFlatten
  fromString "OpFreq"                  = K_OpFreq
  fromString "OpGeq"                   = K_OpGeq
  fromString "OpGt"                    = K_OpGt
  fromString "OpHist"                  = K_OpHist
  fromString "OpIff"                   = K_OpIff
  fromString "OpImage"                 = K_OpImage
  fromString "OpImageSet"              = K_OpImageSet
  fromString "OpImply"                 = K_OpImply
  fromString "OpIn"                    = K_OpIn
  fromString "OpIndexing"              = K_OpIndexing
  fromString "OpIntersect"             = K_OpIntersect
  fromString "OpInverse"               = K_OpInverse
  fromString "OpLeq"                   = K_OpLeq
  fromString "OpLexLeq"                = K_OpLexLeq
  fromString "OpLexLt"                 = K_OpLexLt
  fromString "OpLt"                    = K_OpLt
  fromString "OpMax"                   = K_OpMax
  fromString "OpMin"                   = K_OpMin
  fromString "OpMinus"                 = K_OpMinus
  fromString "OpMod"                   = K_OpMod
  fromString "OpNegate"                = K_OpNegate
  fromString "OpNeq"                   = K_OpNeq
  fromString "OpNot"                   = K_OpNot
  fromString "OpOr"                    = K_OpOr
  fromString "OpParticipants"          = K_OpParticipants
  fromString "OpParts"                 = K_OpParts
  fromString "OpParty"                 = K_OpParty
  fromString "OpPow"                   = K_OpPow
  fromString "OpPowerSet"              = K_OpPowerSet
  fromString "OpPreImage"              = K_OpPreImage
  fromString "OpPred"                  = K_OpPred
  fromString "OpProduct"               = K_OpProduct
  fromString "OpRange"                 = K_OpRange
  fromString "OpRelationProj"          = K_OpRelationProj
  fromString "OpRestrict"              = K_OpRestrict
  fromString "OpSlicing"               = K_OpSlicing
  fromString "OpSubsequence"           = K_OpSubsequence
  fromString "OpSubset"                = K_OpSubset
  fromString "OpSubsetEq"              = K_OpSubsetEq
  fromString "OpSubstring"             = K_OpSubstring
  fromString "OpSucc"                  = K_OpSucc
  fromString "OpSum"                   = K_OpSum
  fromString "OpSupset"                = K_OpSupset
  fromString "OpSupsetEq"              = K_OpSupsetEq
  fromString "OpTildeLeq"              = K_OpTildeLeq
  fromString "OpTildeLt"               = K_OpTildeLt
  fromString "OpToInt"                 = K_OpToInt
  fromString "OpToMSet"                = K_OpToMSet
  fromString "OpToRelation"            = K_OpToRelation
  fromString "OpToSet"                 = K_OpToSet
  fromString "OpTogether"              = K_OpTogether
  fromString "OpTrue"                  = K_OpTrue
  fromString "OpTwoBars"               = K_OpTwoBars
  fromString "OpUnion"                 = K_OpUnion
  fromString "PartialityAttr_Partial"  = K_PartialityAttr_Partial
  fromString "PartialityAttr_Total"    = K_PartialityAttr_Total
  fromString "PartitionAttr"           = K_PartitionAttr
  fromString "RangeBounded"            = K_RangeBounded
  fromString "RangeLowerBounded"       = K_RangeLowerBounded
  fromString "RangeOpen"               = K_RangeOpen
  fromString "RangeSingle"             = K_RangeSingle
  fromString "RangeUpperBounded"       = K_RangeUpperBounded
  fromString "Reference"               = K_Reference
  fromString "SizeAttr_MaxSize"        = K_SizeAttr_MaxSize
  fromString "SizeAttr_MinMaxSize"     = K_SizeAttr_MinMaxSize
  fromString "SizeAttr_MinSize"        = K_SizeAttr_MinSize
  fromString "SizeAttr_None"           = K_SizeAttr_None
  fromString "SizeAttr_Size"           = K_SizeAttr_Size
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
