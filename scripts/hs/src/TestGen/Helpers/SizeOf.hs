{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, MultiWayIf #-}

module TestGen.Helpers.SizeOf where

import AST.Imports
import TestGen.Arbitrary.Data
import TestGen.Helpers.Debug
import qualified Data.Set as S


-- How many different unique values are in say a domain
class SizeOf a where
    sizeOf :: a -> Integer

class DepthOf a where
    depthOf :: a -> Integer


-- How many different unique values are in say a domain
-- return the given values if the a is large then it
class SizeOfLimited a where
    sizeOfLimited:: Integer -> a  -> Integer



instance SizeOf Domain where
    sizeOf DInt{..} = fromIntegral .  S.size .  S.fromList . concatMap rangeInts $ ranges
    sizeOf r = docError [
        "sizeOf not matched",
        pretty $ show r, pretty r
        ]

-- instance SizeOf Type where
--     -- FIXME hardcoded
--     sizeOf TInt            = 20
--     sizeOf TBool           = 2
--     sizeOf (TMatix inner)  = 20 ^ (sizeOf inner)
--     sizeOf (TSet inner)    = 2  ^ (sizeOf inner)
--     sizeOf (TMSet inner)   = 2  ^ (sizeOf inner)
--     sizeOf (TFunc from to) = ((sizeOf to) + 1 )  ^ (sizeOf from)
--     sizeOf (TTuple inners) = product (map sizeOf inners )
--     sizeOf (TRel inners)   = sizeOf (TSet (TTuple inners) )
--     sizeOf (TPar inner)    = sizeOf (TSet (TSet inner))
--     sizeOf TAny            = error "SizeOf Type called with TAny"
--     sizeOf (TUnamed _)     = error "SizeOf Type called with TUnamed"
--     sizeOf (TEnum _)       = error "SizeOf Type called with TUnamed"

instance SizeOfLimited Type where
    -- FIXME hardcoded
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


instance DepthOf Type where
    depthOf TInt  = 0
    depthOf TBool = 0

    depthOf (TMatix inner)   = 1 + depthOf inner
    depthOf (TSet inner)     = 1 + depthOf inner
    depthOf (TMSet inner)    = 1 + depthOf inner
    depthOf (TFunc from to ) = 1 + nonEmpty (maximum . map depthOf) [from, to]
    depthOf (TTuple inners ) = 1 + nonEmpty (maximum . map depthOf) inners
    depthOf (TRel inners )   = 2 + nonEmpty (maximum . map depthOf) inners
    depthOf (TPar inner)     = 1 + depthOf inner

    --FIXME what should the depth of any be?
    depthOf TAny             = 0

    depthOf ty = docError [ "depthOf not implemented", pretty ty ]

nonEmpty :: ([t] -> Integer) -> [t] -> Integer
nonEmpty _ [] = 0
nonEmpty f xs = f xs

rangeInts :: Range Expr -> [Integer]
rangeInts (RSingle (ELit (EI a) ))     = [a]
rangeInts (RFromTo (ELit (EI a) )  (ELit (EI b) ) )  = [a..b]
rangeInts r = docError [
    "rangeInts not matched",
    pretty $ show r, pretty r
    ]
