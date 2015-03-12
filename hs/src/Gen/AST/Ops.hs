{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Gen.AST.Ops where
import Gen.AST.TH


opPlus    x y = [essencee| &x + &y |]
opMinus   x y = [essencee| &x - &y |]
opMult    x y = [essencee| &x * &y |]

opDiv x y = [essencee| &x / &y |]
opMod x y = [essencee| &x % &y |]
opPow x y = [essencee| &x ** &y |]
opEq  x y = [essencee| &x = &y |]
opNeq x y = [essencee| &x != &y |]
opLt  x y = [essencee| &x < &y |]
opLeq x y = [essencee| &x <= &y |]
opGt  x y = [essencee| &x > &y |]
opGeq x y = [essencee| &x >= &y |]
opIn  x y = [essencee| &x in &y |]
opAnd x y = [essencee| &x /\ &y |]
opOr  x y = [essencee| &x \/ &y |]

opImply       x y = [essencee| &x -> &y |]
opIff         x y = [essencee| &x <-> &y |]
opSubset      x y = [essencee| &x subset &y |]
opSubsetEq    x y = [essencee| &x subsetEq &y |]
opSupset      x y = [essencee| &x supset &y |]
opSupsetEq    x y = [essencee| &x supsetEq &y |]
opSubsequence x y = [essencee| &x subsequence &y |]
opSubstring   x y = [essencee| &x substring &y |]
opIntersect   x y = [essencee| &x intersect &y |]
opUnion       x y = [essencee| &x union &y |]
opLexLt       x y = [essencee| &x <lex &y |]
opLexLeq      x y = [essencee| &x <=lex &y |]
opLexGt       y x = [essencee| &x >lex &y |]
opLexGeq      y x = [essencee| &x >=lex &y |]


opTrue         x     = [essencee| true(&x) |]
opToInt        x     = [essencee| toInt(&x) |]
opDefined      x     = [essencee| defined(&x) |]
opRange        x     = [essencee| range(&x) |]
opRestrict     x y   = [essencee| restrict(&x, &y) |]
opAllDiff      x     = [essencee| allDiff(&x) |]
opFlatten      x     = [essencee| flatten(&x) |]
opToSet        x     = [essencee| toSet(&x) |]
opToMSet       x     = [essencee| toMSet(&x) |]
opToRelation   x     = [essencee| toRelation(&x) |]
opMax          x     = [essencee| max(&x) |]
opMin          x     = [essencee| min(&x) |]
opPreImage     x y   = [essencee| preImage(&x, &y) |]
opFreq         x y   = [essencee| freq(&x, &y) |]
opHist         x y   = [essencee| hist(&x, &y) |]
opParts        x     = [essencee| parts(&x) |]
opTogether     x y z = [essencee| together(&x, &y, &z) |]
opApart        x y z = [essencee| apart(&x, &y, &z) |]
opParty        x y   = [essencee| party(&x, &y) |]
opParticipants x     = [essencee| participants(&x ) |]

-- opInverse x y = [essencee| inverse(&x, &y ) |]
opBar x = [essencee|  |&x| |]
