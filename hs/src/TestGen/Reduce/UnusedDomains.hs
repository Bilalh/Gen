{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module TestGen.Reduce.UnusedDomains(unusedDomains) where

import TestGen.Prelude
import qualified Data.HashSet as S
import qualified Data.Map as M

unusedDomains :: Spec -> [Text]
unusedDomains (Spec ds es obj)=
  let used = S.fromList $ [y |  x <- (es ++ maybeToList (fmap snd obj) )
                             ,  EVar y <- universe x ]
      unused = M.filterWithKey (\k _ -> not $ k `S.member` used  ) ds
  in M.keys unused
