{-# LANGUAGE FlexibleInstances #-}

module Gen.Helpers.SizeOf where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Gen.Helpers.Debug
import Gen.Helpers.StandardImports      as X
import Gen.Helpers.TypeOf               (typeOfDom)

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
    depthOf (EBinOp e)    = depthOf e
    depthOf (EUniOp e)    = depthOf e
    depthOf (EProc e)     = depthOf e
    depthOf (EDom e)      =  depthOf e
    depthOf (ETyped _ e2) = depthOf e2
    depthOf EEmptyGuard   = 0

    depthOf (EQuan _ e2 e3 e4) = 1 + maximum ([depthOf e2, depthOf e3, depthOf e4])


instance DepthOf Literal where
    depthOf x = F.foldl (\y e -> y + depthOf e ) 0 x

instance DepthOf Constant where
    depthOf (ConstantBool _)          = 0
    depthOf (ConstantInt _)           = 0
    depthOf (ConstantEnum _ _ _ )     = 0
    depthOf x = error . show . vcat $ [pretty x]


instance DepthOf UniOp where
    depthOf (UBar u) = 1 + depthOf u
    depthOf (UNeg u) = 1 + depthOf u

instance DepthOf BinOp where
    depthOf (BIn b1 b2)        = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BOver b1 b2)      = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BEQ b1 b2)        = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BNEQ b1 b2)       = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BLT b1 b2)        = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BLTE b1 b2)       = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BGT b1 b2)        = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BGTE b1 b2)       = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BDiff b1 b2)      = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BPlus b1 b2)      = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BMult b1 b2)      = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BDiv b1 b2)       = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BPow b1 b2)       = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BMod b1 b2)       = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BAnd b1 b2)       = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BOr b1 b2)        = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (Bimply b1 b2)     = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (Biff b1 b2)       = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (Bsubset b1 b2)    = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BsubsetEq b1 b2)  = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (Bsupset b1 b2)    = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BsupsetEq b1 b2)  = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (Bintersect b1 b2) = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (Bunion b1 b2)     = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BlexLT b1 b2)     = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BlexLTE b1 b2)    = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BlexGT b1 b2)     = 1 +  max (depthOf b1) (depthOf b2)
    depthOf (BlexGTE b1 b2)    = 1 +  max (depthOf b1) (depthOf b2)

instance DepthOf Proc where
    depthOf (PallDiff p)         = 1 + depthOf p
    depthOf (Pindex p1 p2)       = 1 + max  (depthOf p1) (depthOf p2)
    depthOf (Papply p1 p2)       = 1 + max  (depthOf p1) (depthOfOrZero p2)
    depthOf (Pfreq p1 p2)        = 1 + max  (depthOf p1) (depthOf p2)
    depthOf (Phist p1 p2)        = 1 + max  (depthOf p1) (depthOf p2)
    depthOf (Pmax p)             = 1 + depthOf p
    depthOf (Pmin p)             = 1 + depthOf p
    depthOf (PtoInt p)           = 1 + depthOf p
    depthOf (PtoMSet p)          = 1 + depthOf p
    depthOf (PtoRelation p)      = 1 + depthOf p
    depthOf (PtoSet p)           = 1 + depthOf p
    depthOf (Pdefined p)         = 1 + depthOf p
    depthOf (Pimage p1 p2)       = 1 + max  (depthOf p1) (depthOf p2)
    depthOf (Pinverse p1 p2)     = 1 + max  (depthOf p1) (depthOf p2)
    depthOf (PpreImage p1 p2)    = 1 + max  (depthOf p1) (depthOf p2)
    depthOf (Prange p)           = 1 + depthOf p
    depthOf (Papart p1 p2 p3)    = 1 + maximum (map depthOf [p1,p2,p3])
    depthOf (Pparts p)           = 1 + depthOf p
    depthOf (Pparty p1 p2)       = 1 + max  (depthOf p1) (depthOf p2)
    depthOf (Pparticipants p)    = 1 + depthOf p
    depthOf (Ptogether p1 p2 p3) = 1 + maximum (map depthOf [p1,p2,p3])

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
