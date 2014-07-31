{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module EssenceSolver.AllValues where

import Language.E
import Common.Helpers(mkInt)

allValues :: E -> [E]
allValues [xMatch| rs := domain.int.ranges |] =
    concatMap getIntVals rs

    where
        getIntVals [xMatch| [Prim (I j)] := range.single.value.literal |] =
            [mkInt j]
        getIntVals [xMatch| [Prim (I l), Prim(I u)] := range.fromTo.value.literal |] =
            map mkInt [l..u]
        getIntVals [xMatch| [Prim (I _)] := range.from.value.literal |]  =
            error "int unbounded"
        getIntVals _ = error "getIntVals"

allValues [dMatch| set of &inner |] =
    let
        allInners = allValues inner
        allSets =  subsequences allInners
    in
        map mkSet allSets

    where
        mkSet es =  [xMake| value.set.values := es |]

allValues e = error . show . vcat $ [ "Missing case in AllValues", pretty e, prettyAsTree e ]


