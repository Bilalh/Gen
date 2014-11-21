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
class SizeOfBounded a where
    sizeOfBounded:: Integer -> a  -> Integer



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

instance SizeOfBounded Type where
    -- FIXME hardcoded
    sizeOfBounded m TInt           = 20   
    sizeOfBounded m TBool          = 2 
    sizeOfBounded m (TMatix inner) = minB m (sizeOfBounded m inner) (20 ^)
    sizeOfBounded m (TSet inner)   = minB m (sizeOfBounded m inner) (2 ^)
    sizeOfBounded m (TMSet inner)  = minB m (sizeOfBounded m inner) (2 ^)
    sizeOfBounded m (TTuple inners)= min m ( product (map (sizeOfBounded m) inners ) )
    sizeOfBounded m (TRel inners)  = sizeOfBounded m (TSet (TTuple inners) )
    sizeOfBounded m (TPar inner)   = sizeOfBounded m (TSet (TSet inner))
    sizeOfBounded m TAny           = error "SizeOf Type called with TAny"
    sizeOfBounded m (TUnamed _)    = error "SizeOf Type called with TUnamed"
    sizeOfBounded m (TEnum _)      = error "SizeOf Type called with TUnamed"
    
    sizeOfBounded m (TFunc from to) = 
        let toSize   = sizeOfBounded m to 
            fromSize = sizeOfBounded m from
        in if
           | (toSize + 1) >= m -> m
           | otherwise         -> min m ( (toSize + 1) ^ (fromSize) )
    


minB m v _ | v >= m = m 
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
