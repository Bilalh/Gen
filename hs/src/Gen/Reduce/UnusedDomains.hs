module Gen.Reduce.UnusedDomains(unusedDomains) where

import Gen.Imports
import qualified Data.HashSet as S
import qualified Data.Map as M

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
