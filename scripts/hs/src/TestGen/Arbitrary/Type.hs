module TestGen.Arbitrary.Type where

import Language.E

data Type =
      TInt
    | TBool
    | TMatix  Type
    | TSet    Type
    | TMSet   Type
    | TPar    Type
    | TRel    [Type]
    | TFunc   Type Type
    | TUnamed Text  -- each unamed type is unique
    | TEnum   Text -- as are enums
    | TAny
    deriving(Show, Eq, Ord)
