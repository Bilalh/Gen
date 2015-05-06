module Gen.Helpers.OrderedType where

import Gen.Helpers.StandardImports

class Ordered a where
    isOrdered :: a -> Bool

instance Ordered Type where
    isOrdered TypeInt        = True
    isOrdered TypeBool       = True
    isOrdered (TypeSet inn)  = isOrdered inn
    isOrdered (TypeMSet inn) = isOrdered inn
    isOrdered _              = False
