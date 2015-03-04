module Gen.Helpers.OrderedType where

import Gen.Helpers.StandardImports

class Ordered a where
    isOrdered :: a -> Bool

instance Ordered TType where
    isOrdered TInt        = True
    isOrdered TBool       = True
    isOrdered (TSet inn)  = isOrdered inn
    isOrdered (TMSet inn) = isOrdered inn
    isOrdered _           = False
