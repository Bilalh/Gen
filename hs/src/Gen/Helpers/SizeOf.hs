{-# LANGUAGE FlexibleInstances #-}

module Gen.Helpers.SizeOf where

import Conjure.Language.Constant
import Conjure.Language.Expression.Op
import Gen.Helpers.Debug
import Gen.Helpers.StandardImports    as X
import Gen.Helpers.TypeOf             (typeOfDom)

import qualified Data.Foldable as F


class DepthOf a where
    depthOf :: a -> Integer


-- How many different unique values are in say a domain
-- return the given values if the a is large then it
class SizeOfLimited a where
    sizeOfLimited:: Integer -> a  -> Integer


instance SizeOfLimited TType where
    -- FIXME hardcoded over estimated (we only use -5 .. 5)
    sizeOfLimited _ TInt           = 20
    sizeOfLimited _ TBool          = 2
    sizeOfLimited m (TMatix inner) = minB m (sizeOfLimited m inner) (20 ^)
    sizeOfLimited m (TSet inner)   = minB m (sizeOfLimited m inner) (2 ^)
    sizeOfLimited m (TMSet inner)  = minB m (sizeOfLimited m inner) (2 ^)
    sizeOfLimited m (TTuple inners)= min m ( product (map (sizeOfLimited m) inners ) )
    sizeOfLimited m (TRel inners)  = sizeOfLimited m (TSet (TTuple inners) )
    sizeOfLimited m (TPar inner)   = sizeOfLimited m (TSet (TSet inner))
    sizeOfLimited _ TAny           = error "SizeOf Type called with TAny"
    sizeOfLimited _ (TUnamed _)    = error "SizeOf Type called with TUnamed"
    sizeOfLimited _ (TEnum _)      = error "SizeOf Type called with TUnamed"

    sizeOfLimited m (TFunc from to) =
        let toSize   = sizeOfLimited m to
            fromSize = sizeOfLimited m from
        in if
           | (toSize + 1) >= m -> m
           | otherwise         -> min m ( (toSize + 1) ^ (fromSize) )

minB :: Integer -> Integer -> (Integer -> Integer) -> Integer
minB (m :: Integer) v _ | v >= m = m
minB m v f          = min m (f v)


instance DepthOf TType where
    depthOf TInt  = 0
    depthOf TBool = 0

    depthOf (TMatix inner)   = 1 + depthOf inner
    depthOf (TSet inner)     = 1 + depthOf inner
    depthOf (TMSet inner)    = 1 + depthOf inner
    depthOf (TFunc from to ) = 1 + nonEmpty (maximum . map depthOf) [from, to]
    depthOf (TTuple inners ) = 1 + nonEmpty (maximum . map depthOf) inners
    depthOf (TRel inners )   = 2 + nonEmpty (maximum . map depthOf) inners
    depthOf (TPar inner)     = 1 + depthOf inner

    --TODO what should the depth of any be?
    depthOf TAny             = 0

    depthOf ty@(TUnamed _) = docError [ "depthOf not implemented", pretty . show $ ty ]
    depthOf ty@(TEnum _)   = docError [ "depthOf not implemented", pretty . show $ ty ]

instance DepthOf Expr where
    depthOf (ELit e)      = depthOf e
    depthOf (ECon c)      = depthOf c
    depthOf (EVar _ )     = 0
    depthOf (EOp e)       = depthOf e
    depthOf (EDom e)      = depthOf e
    depthOf (ETyped _ e2) = depthOf e2
    depthOf EEmptyGuard   = 0

    depthOf (EQuan _ _ e2 e3 e4) = 1 + maximum ([depthOf e2, depthOf e3, depthOf e4])


-- FIXME check if fold has the same effect as the old hand written one
instance DepthOf Literal where
    depthOf x = F.foldl (\y e -> y + depthOf e ) 0 x

instance DepthOf (Op Expr) where
    depthOf x = F.foldl (\y e -> y + depthOf e ) 0 x

instance DepthOf Constant where
    depthOf (ConstantBool _)          = 0
    depthOf (ConstantInt _)           = 0
    depthOf (ConstantEnum _ _ _ )     = 0
    depthOf x = error . show . vcat $ [pretty x]


instance DepthOf (Domainn Expr) where
    depthOf =  depthOf .  typeOfDom

instance DepthOf a => DepthOf (Maybe a) where
    depthOf Nothing  = 0
    depthOf (Just a) = depthOf a

instance DepthOf (OObjective, Expr) where
    depthOf (_, a) = depthOf a


depthOfOrZero :: (DepthOf x) => [x] -> Integer
depthOfOrZero []    = 0
depthOfOrZero (x:_) = depthOf x

nonEmpty :: ([t] -> Integer) -> [t] -> Integer
nonEmpty _ [] = 0
nonEmpty f xs = f xs
