{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Domain
(
      dom
    , intDom
    , setDom
    , msetDom
    , matixDom
    , funcDom
    , relDom
    , parDom
    , rangeComp
    , intFromDint
) where

import TestGen.Arbitrary.Helpers.Prelude
import qualified Data.Set as S

-- import Text.PrettyPrint(parens)
-- instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b, c) where
--     pretty (a,b,c) = prettyListDoc parens "," [pretty a, pretty b, pretty c]

dom :: Depth -> Gen Domain
dom 0 = oneof [ intDom 0 , return DBool ]
dom n = oneof
    [
      return DBool
    , intDom n
    , setDom n
    , msetDom n
    , matixDom n
    , funcDom n
    , relDom n
    , parDom n
    ]
-- FIXME tuple dom

intDom :: Depth -> Gen Domain
intDom d = return DInt `ap` listOfB 1 4 (range d)

setDom :: Depth -> Gen Domain
setDom depth = do
    inner <- dom (depth - 1)
    return $ dset{inner}

msetDom :: Depth -> Gen Domain
msetDom depth = do
    inner <- dom (depth - 1)
    return $ dmset{inner}

matixDom :: Depth -> Gen Domain
matixDom depth = do
    numElems <- choose (1 :: Integer, min (fromIntegral $ depth * 3) 10 )
    numRanges <- choose (1 :: Integer, numElems)

    -- ranges <-trace (show ("aaa",numElems, numRanges) )  $
    ranges <- mkRanges numElems numElems numRanges S.empty
    let idx = DInt (sortBy  rangeComp ranges)
    innerDom <- dom (depth - 1)

    -- return $ (numElems,numRanges, DMat{innerIdx=idx, inner=innerDom })
    return  dmat{innerIdx=idx, inner=innerDom }

funcDom :: Depth -> Gen Domain
funcDom depth = do
    innerFrom <- dom (depth - 1)
    innerTo   <- dom (depth - 1)
    return dfunc{innerFrom,innerTo}

relDom :: Depth -> Gen Domain
relDom depth = do
    numElems <- choose (1, min (depth * 2) 10 )
    doms <- vectorOf numElems (dom (depth -1) )
    return drel{inners=doms}

parDom :: Depth -> Gen Domain
parDom depth = do
    inner <- dom (depth - 1)
    return dpar{inner}

mkRanges :: Integer ->  Integer -> Integer -> Set Integer -> Gen ( [Range Expr] )
mkRanges _ 0 0 _ = return []

mkRanges ub ns 1 used = do
    (l,u) <- chooseUnusedSized ub ns used
    return  [ RFromTo (ELit . EI $ l) (ELit . EI $ u) ]

mkRanges ub ns rs used | ns == rs = do
    i <- chooseUnused ub used
    rest <- mkRanges ub (ns - 1) (rs - 1) (S.union (S.singleton i )  used)
    return $ RSingle (ELit . EI $ i) : rest

mkRanges _ 1 rs _ | rs /= 1 = do
     error . show $ ("mkRanges invaild" :: String, 1 :: Integer, rs)

mkRanges ub ns rs used | ns >=2= do
    single :: Bool <- arbitrary
    if single then do
        i <- chooseUnused ub used
        rest <- mkRanges ub (ns - 1) (rs - 1) (S.union (S.singleton i )  used)
        return $ RSingle (ELit . EI $ i) : rest
    else do
        num <- choose (2, ns - rs + 1 )
        (l,u) <- chooseUnusedSized ub num used

        let used' = S.fromList [l..u]  `S.union` used
        rest <- mkRanges ub (ns - (u - l + 1) ) (rs - 1) used'

        return $ RFromTo (ELit . EI $ l) (ELit . EI $ u) : rest

mkRanges _ ns rs _  =
     error . show $ ("mkRanges unmatched" :: String, ns, rs)

chooseUnusedSized ::  Integer -> Integer -> Set Integer -> Gen (Integer, Integer)
chooseUnusedSized ub size used | S.null used  =  do
    lower <- choose (-ub*3,ub*3 - size)
    return (lower, lower + size - 1)

chooseUnusedSized ub size used =  do
    lower <- chooseUnused' (-ub*3,ub*3 - size) used
    let upper = lower + (size - 1)
    if  S.fromList [lower..upper] `S.isSubsetOf` used then
        chooseUnusedSized ub size used
    else
        return (lower,upper)


chooseUnused :: Integer -> Set Integer -> Gen Integer
chooseUnused ub = chooseUnused' (-ub*3, ub*3)

chooseUnused' :: (Integer,Integer) -> Set Integer -> Gen Integer
chooseUnused' (l,u) used | S.null used  = choose (l,u)
chooseUnused' (l,u) used = do
    i <- choose (l,u)
    if i `S.member` used then
        chooseUnused' (l,u) used
    else
        return i



range :: Depth -> Gen (Range Expr)
range _ = oneof
    [
      arbitrarySingle
    , arbitraryFromTo
    ]

    where
    arbitrarySingle :: Gen (Range Expr)
    arbitrarySingle = do
        a <- choose (-10,10 :: Integer)
        return $ RSingle (ELit . EI $ a)

    arbitraryFromTo :: Gen (Range Expr)
    arbitraryFromTo = do
        do
            a <- choose (-10,10 :: Integer)
            b <- choose (a,10)
            return $ RFromTo (ELit . EI $ a) (ELit . EI $  b)



rangeComp :: Range Expr -> Range Expr -> Ordering
rangeComp (RSingle (ELit (EI a) ))    (RSingle (ELit (EI b) ))   = compare a b
rangeComp (RSingle (ELit (EI a) ))    (RFromTo (ELit (EI b) ) _) = compare a b
rangeComp (RFromTo (ELit (EI a) ) _ ) (RFromTo (ELit (EI b) ) _) = compare a b
rangeComp (RFromTo (ELit (EI a) ) _)  (RSingle (ELit (EI b) ))   = compare a b
rangeComp a b  = docError [
    "rangeComp not matched",
    pretty $ show a, pretty $ show b,
    pretty a, pretty b
    ]

intFromDint :: Domain -> Gen Expr
intFromDint DInt{..} =  elements $ concatMap getInts ranges
    where
        getInts (RSingle x@(ELit (EI _))) =  [x]
        getInts (RFromTo (ELit (EI a) ) (ELit (EI b) ))  = map (ELit . EI) [a..b]
        getInts a = docError [
            "getInts not matched",
            pretty $ show a, pretty a
            ]

intFromDint a = docError [
    "intFromInt not matched",
    pretty $ show a, pretty a
    ]
