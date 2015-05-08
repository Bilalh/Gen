{-# LANGUAGE FlexibleInstances, QuasiQuotes #-}
module Gen.Helpers.SizeOf where

import Conjure.Language.Constant
import Conjure.Language.Expression.Op
import Gen.AST.TH
import Gen.Helpers.TypeOf             (typeOfDom)
import Gen.Imports

import qualified Data.Foldable as F


class DepthOf a where
    depthOf :: a -> Integer


instance DepthOf Type where
    depthOf TypeInt  = 0
    depthOf TypeBool = 0
    --FIXME what should the depth of any be?
    depthOf TypeAny  = 0
    depthOf x = nonEmpty (maximum . map depthOf_p1) . children $ x


instance DepthOf Expr where
    depthOf (ELit e)      = depthOf e
    depthOf (ECon c)      = depthOf c
    depthOf (EOp e)       = depthOf e
    depthOf (EDom e)      = depthOf e
    depthOf (ETyped _ e2) = depthOf e2

    depthOf (EVar _ )     = 0
    depthOf EEmptyGuard   = 0
    depthOf EMetaVar{}    = 0

    depthOf (EQuan _ _ e2 e3 e4) = 1 + maximum ([depthOf e2, depthOf e3, depthOf e4])

    depthOf (EComp inner _ cons) = maximum $ depthOf inner : map depthOf cons

instance DepthOf Literal where
    depthOf x = empty_p1 (maximum . map depthOf_p1) . F.toList $ x

instance DepthOf (Op Expr) where
    depthOf [opp| &a + &b |]  = nonEmpty (maximum . map depthOf_p1) [a,b]
    depthOf [opp| &a * &b |]  = nonEmpty (maximum . map depthOf_p1) [a,b]
    depthOf [opp| &a /\ &b |] = nonEmpty (maximum . map depthOf_p1) [a,b]
    depthOf [opp| &a \/ &b |] = nonEmpty (maximum . map depthOf_p1) [a,b]

    depthOf x = nonEmpty (maximum . map depthOf_p1) . F.toList $ x

instance DepthOf Constant where
    depthOf (ConstantBool _)          = 0
    depthOf (ConstantInt _)           = 0
    depthOf (ConstantEnum _ _ _ )     = 0
    depthOf x = notHandled $line "DepthOf Constant" x


instance DepthOf (Domainn Expr) where
    depthOf =  depthOf .  typeOfDom

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


-- How many different unique values are in say a domain
-- return the given values if the a is large then it
class SizeOfLimited a where
    sizeOfLimited:: Integer -> a  -> Integer


instance SizeOfLimited Type where
    -- FIXME hardcoded over estimated (we only use -5 .. 5)
    sizeOfLimited _ TypeInt               = 20
    sizeOfLimited _ TypeBool              = 2
    sizeOfLimited m (TypeMatrix _ inner)  = minB m (sizeOfLimited m inner) (20 ^)
    sizeOfLimited m (TypeSet inner)       = minB m (sizeOfLimited m inner) (2 ^)
    sizeOfLimited m (TypeMSet inner)      = minB m (sizeOfLimited m inner) (2 ^)
    sizeOfLimited m (TypeTuple inners)    = min m ( product (map (sizeOfLimited m) inners ) )
    sizeOfLimited m (TypeRelation inners) = sizeOfLimited m (TypeSet (TypeTuple inners) )
    sizeOfLimited m (TypePartition inner) = sizeOfLimited m (TypeSet (TypeSet inner))

    sizeOfLimited m (TypeFunction from to) =
        let toSize   = sizeOfLimited m to
            fromSize = sizeOfLimited m from
        in if
           | (toSize + 1) >= m -> m
           | otherwise         -> min m ( (toSize + 1) ^ (fromSize) )

    sizeOfLimited _ t = error $ "SizeOf Type called with " ++ (show t)


minB :: Integer -> Integer -> (Integer -> Integer) -> Integer
minB (m :: Integer) v _ | v >= m = m
minB m v f          = min m (f v)
