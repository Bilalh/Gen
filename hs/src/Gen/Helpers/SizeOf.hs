{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}
module Gen.Helpers.SizeOf where

import Conjure.Language.Constant
import Conjure.Language.Expression.Op
import Gen.AST.TH
import Gen.Imports
import Conjure.Language.Domain

import qualified Data.Foldable as F
import qualified Data.Set      as S


class DepthOf a where
    depthOf :: a -> Integer


instance DepthOf Type where
    depthOf TypeInt  = 0
    depthOf TypeBool = 0
    depthOf TypeAny  = 0
    depthOf x = nonEmpty (maximum . map depthOf_p1) . children $ x


instance DepthOf Expr where
    depthOf (ELit e)      = depthOf e
    depthOf (ECon c)      = depthOf c
    depthOf (EOp e)       = depthOf e
    depthOf (EDom e)      = depthOf e
    depthOf (ETyped ty e) = max (depthOf ty) (depthOf e)

    depthOf (EVar _ )     = 0
    depthOf EEmptyGuard   = 0
    depthOf EMetaVar{}    = 0

    depthOf (EQuan _ _ e2 e3 e4) = 1 + maximum ([depthOf e2, depthOf e3, depthOf e4])

    depthOf (EComp inner gens cons) = 1 + (maximum $ depthOf inner : map depthOf cons ++ map depthOf gens)

instance DepthOf EGen where
    depthOf (GenDom _ x2) = depthOf x2
    depthOf (GenIn _ x2)  = depthOf x2

instance DepthOf x => DepthOf (AbstractLiteral x) where
    depthOf x = empty_p1 (maximum . map depthOf_p1) . F.toList $ x

instance DepthOf (Op Expr) where
    depthOf [opp| &a + &b |]  = nonEmpty (maximum . map depthOf_p1) [a,b]
    depthOf [opp| &a * &b |]  = nonEmpty (maximum . map depthOf_p1) [a,b]
    depthOf [opp| &a /\ &b |] = nonEmpty (maximum . map depthOf_p1) [a,b]
    depthOf [opp| &a \/ &b |] = nonEmpty (maximum . map depthOf_p1) [a,b]

    depthOf x = nonEmpty (maximum . map depthOf_p1) . F.toList $ x

instance DepthOf Constant where
    depthOf ConstantBool{}       = 0
    depthOf ConstantInt{}        = 0
    depthOf ConstantEnum{}       = 0
    depthOf (ConstantAbstract x) = depthOf x
    depthOf (TypedConstant _ x2) = depthOf x2
    depthOf x                    = notHandled $line "depthOf Constant" x


instance DepthOf (Domain () Expr) where
  depthOf DomainBool                  = 0
  depthOf (DomainEnum _ _ _)          = 0
  depthOf (DomainUnnamed _ _)         = 0
  depthOf (DomainInt x)               = nonEmpty maximum (map depthOf x)
  depthOf (DomainTuple x)             = 1 + maximum (map depthOf x)
  depthOf (DomainRecord x)            = 1 + maximum (map (depthOf . snd) x)
  depthOf (DomainVariant x)           = 1 + maximum (map (depthOf . snd) x)
  depthOf (DomainMatrix x1 x2)        = 1 + maximum (map depthOf [x1,x2])
  depthOf (DomainSet _ x2 x3)         = 1 + maximum [depthOf x2, depthOf x3]
  depthOf (DomainMSet _ x2 x3)        = 1 + maximum [depthOf x2, depthOf x3]
  depthOf (DomainFunction _ x2 x3 x4) = 1 + maximum [depthOf x2, depthOf x3, depthOf x4]
  depthOf (DomainSequence _ x2 x3)    = 1 + maximum [depthOf x2, depthOf x3]
  depthOf (DomainRelation _ x2 x3)    = 1 + maximum (depthOf x2 : map depthOf x3)
  depthOf (DomainPartition _ x2 x3)   = 1 + maximum [depthOf x2, depthOf x3]

  depthOf x = notHandled $line "depthOf Domain () Expr" x

instance DepthOf a => DepthOf (Range a) where
  depthOf RangeOpen             = 0
  depthOf (RangeSingle x)       = maximum [depthOf x]
  depthOf (RangeLowerBounded x) = maximum [depthOf x]
  depthOf (RangeUpperBounded x) = maximum [depthOf x]
  depthOf (RangeBounded x1 x2)  = maximum [depthOf x1, depthOf x2]


instance DepthOf a => DepthOf (FunctionAttr a) where
  depthOf (FunctionAttr d1 d2 d3) =  maximum [depthOf d1, depthOf d2, depthOf d3]

instance DepthOf a => DepthOf (MSetAttr a) where
  depthOf (MSetAttr d1 d2) = maximum [depthOf d1, depthOf d2]

instance DepthOf a => DepthOf (PartitionAttr a) where
  depthOf PartitionAttr{..} = maximum ([depthOf partsNum, depthOf partsSize])

instance DepthOf a => DepthOf (RelationAttr a) where
  depthOf (RelationAttr d1 d2) = maximum [depthOf d1, depthOf d2]

instance DepthOf a => DepthOf (SequenceAttr a) where
  depthOf (SequenceAttr d1 d2) = maximum [depthOf d1, depthOf d2]

instance DepthOf a => DepthOf (SetAttr a) where
  depthOf (SetAttr d1) = depthOf d1


instance DepthOf a => DepthOf (SizeAttr a) where
  depthOf d = nonEmpty maximum (map depthOf .F.toList $ d)

instance DepthOf a => DepthOf (OccurAttr a) where
  depthOf d = nonEmpty maximum (map depthOf .F.toList $ d)

instance DepthOf PartialityAttr where
  depthOf _ = 0

instance DepthOf JectivityAttr where
  depthOf _ = 0

instance DepthOf BinaryRelationAttrs where
  depthOf (BinaryRelationAttrs d1) = nonEmpty maximum (map depthOf $ S.toList d1)

instance DepthOf BinaryRelationAttr where
  depthOf _ = 0


instance DepthOf a => DepthOf (Maybe a) where
    depthOf Nothing  = 0
    depthOf (Just a) = depthOf a

instance DepthOf (OObjective, Expr) where
    depthOf (_, a) = depthOf a


nonEmpty :: ([t] -> Integer) -> [t] -> Integer
nonEmpty _ [] = 0
nonEmpty f xs = f xs

empty_p1 :: ([t] -> Integer) -> [t] -> Integer
empty_p1 _ [] = 1
empty_p1 f xs = f xs

depthOf_p1 :: (DepthOf x) => x -> Integer
depthOf_p1 x = depthOf x + 1

