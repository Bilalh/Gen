module TestGen.Helpers.OrderedType where

import AST.Imports

class Ordered a where
    isOrdered :: a -> Bool

instance Ordered Type where
    isOrdered TInt        = True
    isOrdered TBool       = True
    isOrdered (TSet inn)  = isOrdered inn
    isOrdered (TMSet inn) = isOrdered inn
    isOrdered _           = False
