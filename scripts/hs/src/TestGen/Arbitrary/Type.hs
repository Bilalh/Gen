module TestGen.Arbitrary.Type where

import Language.E

data Type =
      TInt
    | TBool
    | TUnamed Int  -- each unamed type is unique
    | TEnum   Text -- as are enums
    | TMatix  Type
    | TSet    Type
    | TMSet   Type
    | TPar    Type
    | TRel    [Type]
    | TFunc   Type Type
    | TAny
