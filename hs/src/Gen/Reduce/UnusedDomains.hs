module Gen.Reduce.UnusedDomains(unusedDomains, unusedGenerators,usedGeneratorsChoices) where

import Conjure.Language.Expression
import Conjure.Language.Name
import Gen.Imports

import qualified Data.HashSet as S
import qualified Data.Map     as M

unusedDomains :: Spec -> [Text]
unusedDomains (Spec ds es obj)=
  let inExprs = S.fromList $ [y |  x <- (maybeToList (fmap snd obj) ++ es)
                                ,  EVar (Var y _) <- universe x ]

      inDoms     = S.fromList $ concatMap (checkDom . domOfGF . snd) (M.elems ds)
      checkDom :: Domain () Expr -> [Text]
      checkDom x = [ y | EVar (Var y _) <- universe (EDom x) ]

      used = S.union inExprs inDoms

      unused = M.filterWithKey (\k _ -> not $ k `S.member` used  ) ds
  in M.keys unused

unusedGenerators :: Expr -> [Text]
unusedGenerators (EComp inners gens cons)  =
  let inExprs = S.fromList $ [y |  x <- (inners : cons ++ map exprOfEGen gens )
                                ,  EVar (Var y _) <- universe x ]

      inDoms     = S.fromList $ concatMap checkDom $ concatMap domOfEGen gens
      checkDom :: Domain () Expr -> [Text]
      checkDom x = [ y | EVar (Var y _) <- universe (EDom x) ]

      used = S.union inExprs inDoms

      unused = concatMap (genUnused used) gens

  in unused

  where
    domOfEGen :: EGen -> [Domain () Expr]
    domOfEGen (GenDom _ dom) = [dom]
    domOfEGen _              = []

    exprOfEGen :: EGen -> Expr
    exprOfEGen (GenDom _ dom)  = (EDom dom)
    exprOfEGen (GenIn  _ expr) = expr

    genUnused :: S.HashSet Text -> EGen -> [Text]
    genUnused used (GenDom (Single (Name n)) _) | not $ n `S.member` used = [n]
    genUnused used (GenIn (Single (Name n)) _)  | not $ n `S.member` used = [n]

    genUnused used (GenDom pat _) =
      [ n | (Single (Name n)) <- universe pat,  not $ n `S.member` used ]

    genUnused used (GenIn pat _) =
      [ n | (Single (Name n)) <- universe pat,  not $ n `S.member` used ]

unusedGenerators _ = []

-- Always keep atlest one gen
usedGeneratorsChoices :: [EGen] -> [Text] -> [[EGen]]
usedGeneratorsChoices ds ts =
    -- remove [] and reversing to get largest first
    -- meaning res would be [ [a], [b], [a,b],  ... ]
    let ways = reverse . tail . sortBy (comparing length) . subsequences $ ts
        res = fmap (\wy -> filter (keepUsed wy ) ds ) ways
    in res

  where
    keepUsed wy (GenDom (Single (Name n)) _) = n  `notElem` wy
    keepUsed wy (GenIn (Single (Name n)) _) = n  `notElem` wy

    keepUsed wy (GenDom pat _) =
      or [ True | (Single (Name n)) <- universe pat, n `notElem` wy ]

    keepUsed wy (GenIn pat _) =
      or [ True | (Single (Name n)) <- universe pat, n `notElem` wy ]
