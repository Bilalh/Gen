module Gen.Arbitrary.OrderedType where

import Gen.Imports

class Ordered a where
    isOrdered :: a -> Bool

instance Ordered Type where
    isOrdered TypeInt        = True
    isOrdered TypeBool       = True
    isOrdered (TypeSet inn)  = isOrdered inn
    isOrdered (TypeMSet inn) = isOrdered inn
    isOrdered _              = False
