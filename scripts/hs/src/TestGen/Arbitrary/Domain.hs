{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Domain
(
      dom_def
    , dom_only
    
    , intDomChoice
    , setDomChoice
    , msetDomChoice
    , matixDomChoice
    , funcDomChoice
    , relDomChoice
    , parDomChoice
    , tupleDomChoice
    , boolDomChoice
    
    , intDom
    , setDom
    , msetDom
    , matixDom
    , funcDom
    , relDom
    , parDom
    , tupleDom
    
    , rangeComp
    , intFromDint
) where

import TestGen.Prelude
import qualified Data.Set as S

-- import Text.PrettyPrint(parens)
-- instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b, c) where
--     pretty (a,b,c) = prettyListDoc parens "," [pretty a, pretty b, pretty c]

intDomChoice, setDomChoice, msetDomChoice, matixDomChoice :: (Int, GG Domain)
funcDomChoice, relDomChoice, parDomChoice, tupleDomChoice :: (Int, GG Domain)
boolDomChoice :: (Int, GG Domain)

boolDomChoice  = (0, return DBool)
intDomChoice   = (0, intDom)
setDomChoice   = (1, setDom)
msetDomChoice  = (1, msetDom)
matixDomChoice = (1, matixDom)
funcDomChoice  = (1, funcDom)
relDomChoice   = (2, relDom)
parDomChoice   = (1, parDom)
tupleDomChoice = (1, parDom)


dom_only :: [(Int,GG Domain)] -> GG Domain
dom_only ds = do 
    addLog "dom_only" ["start"]
    
    d <- gets depth_
    if | d < 0  -> ggError "dom_only invalid depth" []
       | otherwise -> do 
    
        let inDepth = map snd . filter (\(i,_) -> i <= d ) $ ds
        choice <- oneof2 inDepth
        return choice


dom_def :: GG Domain
dom_def =  gets depth_ >>= \case
    0  -> oneof2 [ intDom, return DBool ]
    1  -> oneof2
        [ return DBool
        , intDom
        , setDom
        -- , msetDom
        , matixDom
        , funcDom
        , parDom
        , tupleDom
        ]
    _ -> oneof2
        [ return DBool
        , intDom
        , setDom
        -- , msetDom
        , matixDom
        , funcDom
        , relDom
        , parDom
        , tupleDom
        ]

intDom :: GG Domain
intDom = return DInt `ap` listOfBounds (1, 2) (range)

setDom :: GG Domain
setDom = do
    inner <- withDepthDec dom
    return $ dset{inner}

msetDom :: GG Domain
msetDom = do
    inner <- withDepthDec dom
    return $ dmset{inner}

matixDom :: GG Domain
matixDom = do
    d <- gets depth_
    numElems <- choose2 (1 :: Integer, min (fromIntegral $ d * 3) 10 )
    numRanges <- choose2 (1 :: Integer, numElems)

    -- ranges <-trace (show ("aaa",numElems, numRanges) )  $
    ranges <- mkRanges numElems numElems numRanges S.empty
    let idx = DInt (sortBy  rangeComp ranges)
    innerDom <- withDepthDec dom

    -- return $ (numElems,numRanges, DMat{innerIdx=idx, inner=innerDom })
    return  dmat{innerIdx=idx, inner=innerDom }

funcDom :: GG Domain
funcDom = do
    innerFrom <- withDepthDec dom
    innerTo   <- withDepthDec dom
    return dfunc{innerFrom,innerTo}

relDom :: GG Domain
relDom = do
    d <- gets depth_
    numElems <- choose2 (1, min (d * 2) 10 )
    doms <- vectorOf2 numElems (withDepthDec dom)
    return drel{inners=doms}

parDom :: GG Domain
parDom = do
    inner <- withDepthDec dom
    return dpar{inner}

tupleDom :: GG Domain
tupleDom = do
    d <- gets depth_
    numElems <- choose2 (1, min (d * 2) 5 )
    doms <- vectorOf2 numElems (withDepthDec dom)
    return dtuple{inners=doms}



mkRanges :: Integer ->  Integer -> Integer -> Set Integer -> GG ( [Range Expr] )
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
    single :: Bool <- lift arbitrary
    if single then do
        i <- chooseUnused ub used
        rest <- mkRanges ub (ns - 1) (rs - 1) (S.union (S.singleton i )  used)
        return $ RSingle (ELit . EI $ i) : rest
    else do
        num <- choose2 (2, ns - rs + 1 )
        (l,u) <- chooseUnusedSized ub num used

        let used' = S.fromList [l..u]  `S.union` used
        rest <- mkRanges ub (ns - (u - l + 1) ) (rs - 1) used'

        return $ RFromTo (ELit . EI $ l) (ELit . EI $ u) : rest

mkRanges _ ns rs _  =
     error . show $ ("mkRanges unmatched" :: String, ns, rs)

chooseUnusedSized ::  Integer -> Integer -> Set Integer -> GG (Integer, Integer)
chooseUnusedSized ub size used | S.null used  =  do
    lower <- choose2 (-ub*3,ub*3 - size)
    return (lower, lower + size - 1)

chooseUnusedSized ub size used =  do
    lower <- chooseUnused' (-ub*3,ub*3 - size) used
    let upper = lower + (size - 1)
    if  S.fromList [lower..upper] `S.isSubsetOf` used then
        chooseUnusedSized ub size used
    else
        return (lower,upper)


chooseUnused :: Integer -> Set Integer -> GG Integer
chooseUnused ub = chooseUnused' (-ub*3, ub*3)

chooseUnused' :: (Integer,Integer) -> Set Integer -> GG Integer
chooseUnused' (l,u) used | S.null used  = choose2 (l,u)
chooseUnused' (l,u) used = do
    i <- choose2 (l,u)
    if i `S.member` used then
        chooseUnused' (l,u) used
    else
        return i



range :: GG (Range Expr)
range = oneof2
    [
      arbitrarySingle
    , arbitraryFromTo
    ]

    where
    arbitrarySingle :: GG (Range Expr)
    arbitrarySingle = do
        a <- choose2 (-5,5 :: Integer)
        return $ RSingle (ELit . EI $ a)

    arbitraryFromTo :: GG (Range Expr)
    arbitraryFromTo = do
        do
            a <- choose2 (-5,5 :: Integer)
            b <- choose2 (a,5)
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

intFromDint :: Domain -> GG Expr
intFromDint DInt{..} =  elements2 $ concatMap getInts ranges
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
