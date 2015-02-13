module TestGen.Helpers.OrderedType where

import TestGen.Helpers.StandardImports

class Ordered a where
    isOrdered :: a -> Bool

instance Ordered TType where
    isOrdered TInt        = True
    isOrdered TBool       = True
    isOrdered (TSet inn)  = isOrdered inn
    isOrdered (TMSet inn) = isOrdered inn
    isOrdered _           = False
