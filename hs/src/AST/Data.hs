{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}


module AST.Data where

import Conjure.Prelude
import Conjure.Language.Domain
import Conjure.Language.Pretty


class (Pretty ast, Pretty conjure, Show ast) => Translate ast conjure where
  -- Should never cause an error but...
  toConjure   :: MonadFail m => ast -> m conjure
  -- Should never cause an error but...
  fromConjure :: MonadFail m => conjure -> m ast

  toConjureNote :: Doc -> ast -> conjure
  toConjureNote msg a = case toConjure a  of
                  Right x -> x
                  Left  d -> error . renderNormal . vcat $ [msg, d ]

  fromConjureNote :: Doc -> conjure -> ast
  fromConjureNote msg a =  case fromConjure a of
                    Right x -> x
                    Left  d -> error . renderNormal . vcat $ [msg, d ]


newtype Domainn x = Domainn (Domain () x)
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (Domainn x)
instance (Hashable  x) => Hashable  (Domainn x)
instance (ToJSON    x) => ToJSON    (Domainn x) where toJSON = genericToJSON jsonOptions
instance (FromJSON  x) => FromJSON  (Domainn x) where parseJSON = genericParseJSON jsonOptions


data Expr =
    ELit Literal
  | EVar Text
  | EBinOp BinOp
  | EUniOp UniOp
  | EProc Proc  -- e.g alldiff
  | EDom (Domainn Expr)
  | ETyped TType Expr
  | EEmptyGuard
  | EQuan QType BinOp Expr Expr
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize (Expr)
instance Hashable  (Expr)
instance ToJSON    (Expr) where toJSON = genericToJSON jsonOptions
instance FromJSON  (Expr) where parseJSON = genericParseJSON jsonOptions


data QType = ForAll
           | Exists
           | Sum
           deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize QType
instance Hashable  QType
instance ToJSON    QType where toJSON = genericToJSON jsonOptions
instance FromJSON  QType where parseJSON = genericParseJSON jsonOptions

data BinOp = BIn   Expr  Expr  -- TODO SR also has a `in`
           | BOver Expr  Expr

           | BEQ   Expr  Expr
           | BNEQ  Expr  Expr

           | BLT   Expr  Expr
           | BLTE  Expr  Expr
           | BGT   Expr  Expr
           | BGTE  Expr  Expr

           | BDiff Expr  Expr

           | BPlus Expr  Expr
           | BMult Expr  Expr
           | BDiv  Expr  Expr

           | BPow  Expr  Expr
           | BMod  Expr  Expr

           | BAnd  Expr  Expr
           | BOr   Expr  Expr

           | Bimply Expr Expr
           | Biff   Expr Expr

           | Bsubset   Expr Expr
           | BsubsetEq Expr Expr
           | Bsupset   Expr Expr
           | BsupsetEq Expr Expr

           | Bintersect Expr Expr
           | Bunion     Expr Expr

           | BlexLT   Expr  Expr
           | BlexLTE  Expr  Expr
           | BlexGT   Expr  Expr
           | BlexGTE  Expr  Expr

           deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize BinOp
instance Hashable  BinOp
instance ToJSON    BinOp where toJSON = genericToJSON jsonOptions
instance FromJSON  BinOp where parseJSON = genericParseJSON jsonOptions

data UniOp = UBar Expr
           | UNeg Expr
           deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize UniOp
instance Hashable  UniOp
instance ToJSON    UniOp where toJSON = genericToJSON jsonOptions
instance FromJSON  UniOp where parseJSON = genericParseJSON jsonOptions

data Proc =   PallDiff Expr

            | Pindex Expr Expr   -- ref, constant expression
            | Papply Expr [Expr] -- ref, args

            | Pfreq Expr Expr
            | Phist Expr Expr

            -- Has different meaning in SR?
            | Pmax Expr
            | Pmin Expr

            | PtoInt Expr
            | PtoMSet Expr
            | PtoRelation Expr
            | PtoSet Expr

            | Pdefined Expr
            | Pimage Expr Expr
            | Pinverse Expr Expr
            | PpreImage Expr Expr
            | Prange Expr

            | Papart Expr Expr Expr
            | Pparts Expr
            | Pparty Expr Expr
            | Pparticipants Expr
            | Ptogether Expr Expr Expr

          deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Proc
instance Hashable  Proc
instance ToJSON    Proc where toJSON = genericToJSON jsonOptions
instance FromJSON  Proc where parseJSON = genericParseJSON jsonOptions

data Literal
    = EB Bool
    | EI Integer
    | ETuple      [Literal]
    | EMatrix     [Literal] (Domainn Expr)
    | ESet        [Literal]
    | EMSet       [Literal]
    | EFunction   [(Literal, Literal)] -- list of mappings
    | ERelation   [Literal]            -- list of tuples
    | EPartition  [[Literal]]          -- list of parts
    | EExpr Expr
      deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Literal
instance Hashable  Literal
instance ToJSON    Literal where toJSON = genericToJSON jsonOptions
instance FromJSON  Literal where parseJSON = genericParseJSON jsonOptions

data TType =
      TAny
    | TBool
    | TInt
    | TMatix  TType
    | TSet    TType
    | TMSet   TType
    | TFunc   TType TType
    | TTuple  [TType]
    | TRel    [TType]
    | TPar    TType
    | TUnamed Text   -- each unamed type is unique
    | TEnum   Text   -- as are enums
      deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize TType
instance Hashable  TType
instance ToJSON    TType where toJSON = genericToJSON jsonOptions
instance FromJSON  TType where parseJSON = genericParseJSON jsonOptions


toConjureFail :: forall (m :: * -> *) a a1.
                 (MonadFail m, Show a1) =>
                 Doc -> a1 -> m a

fromConjureFail :: forall (m :: * -> *) a a1.
                   (Pretty a1, MonadFail m) =>
                   Doc -> a1 -> m a


fromConjureFail s x  = fail . vcat $ ["fromConjure failed: " <+> s
                                     ,pretty x
                                     ,pretty . groom $ x
                                     ]

toConjureFail s x    = fail . vcat $ ["toConjure failed: " <+> s
                                     ,pretty . groom $ x
                                     ]