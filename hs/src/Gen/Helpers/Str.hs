module Gen.Helpers.Str(str) where
{- [str| mutiline
   string |]
-}

import Language.Haskell.TH
import Language.Haskell.TH.Quote

str :: QuasiQuoter
str = QuasiQuoter { quoteExp = stringE }
