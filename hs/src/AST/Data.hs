{-# LANGUAGE  FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module AST.Data where

import Conjure.Prelude

data DDomain
  = DSet
    { size        :: Maybe Integer
    , minSize     :: Maybe Integer
    , maxSize     :: Maybe Integer
    , inner       :: DDomain
    }
  | DMSet
    { size        :: Maybe Integer
    , minSize     :: Maybe Integer
    , maxSize     :: Maybe Integer
    , minOccur    :: Maybe Integer
    , maxOccur    :: Maybe Integer
    , inner       :: DDomain
    }
  | DFunc
    { size        :: Maybe Integer
    , maxSize     :: Maybe Integer
    , minSize     :: Maybe Integer
    , surjective  :: Bool
    , injective   :: Bool
    , total       :: Bool
    , innerFrom   :: DDomain
    , innerTo     :: DDomain
    }
  | DPar
    { size        :: Maybe Integer
    , maxSize     :: Maybe Integer
    , minSize     :: Maybe Integer
    , numParts    :: Maybe Integer
    , maxNumParts :: Maybe Integer
    , minNumParts :: Maybe Integer
    , partSize    :: Maybe Integer
    , maxPartSize :: Maybe Integer
    , minPartSize :: Maybe Integer
    , regular     :: Bool
    , complete    :: Bool
    , inner       :: DDomain
    }
  | DRel
    { size        :: Maybe Integer
    , maxSize     :: Maybe Integer
    , minSize     :: Maybe Integer
    , reflexive   :: Bool
    , symmetric   :: Bool
    , inners      :: [DDomain] -- tuples
    }
  | DTuple
    { inners :: [DDomain]
    }
  | DInt
    { ranges      :: [RRange Expr]
    }
  | DMat
    { innerIdx    :: DDomain
    , inner       :: DDomain
    }
  | DBool
  -- enums
  -- unamed types
    deriving(Show, Generic, Typeable, Eq, Ord, Read)

instance Hashable DDomain
instance Hashable (RRange Expr)
instance Hashable (TType)
instance Hashable (Expr)
instance Hashable (QType)
instance Hashable (BinOp)
instance Hashable (Literal)
instance Hashable (Proc)
instance Hashable (UniOp)


data RRange a =
      RSingle a
    | RFromTo a a
    deriving (Show, Generic, Typeable, Eq, Ord, Read)

data Expr =
    ELit Literal
  | EVar Text
  | EQVar Text
  | EBinOp BinOp
  | EUniOp UniOp
  | EProc Proc  -- e.g alldiff
  | EDom DDomain
  | ETyped TType Expr
  | EEmptyGuard
  | EQuan QType BinOp Expr Expr
  deriving (Show, Generic, Typeable, Eq, Ord, Read)

data QType = ForAll
           | Exists
           | Sum
           deriving (Show, Generic, Typeable, Eq, Ord, Read)


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


           deriving (Show, Generic, Typeable, Eq, Ord, Read)

data UniOp = UBar Expr
           | UNeg Expr
           deriving (Show, Generic, Typeable, Eq, Ord, Read)

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

        --  dontcare?

        --   in Essence?
        --   | Pflatten Expr
        --
        --   | Psum     Expr
        --   | Pproduct Expr
        --
        --   | Pand Expr
        --   | Por  Expr
        --
        --   | Patleast Expr Expr Expr
        --   | Patmost  Expr Expr Expr
        --   | Pgcc     Expr Expr Expr
        --
        --   | Palldifferent_except Expr Expr
        --   | table [Expr]
          deriving (Show, Generic, Typeable, Eq, Ord, Read)

data Literal
    = EB Bool
    | EI Integer
    | ETuple      [Literal]
    | EMatrix     [Literal] DDomain
    | ESet        [Literal]
    | EMSet       [Literal]
    | EFunction   [(Literal, Literal)] -- list of mappings
    | ERelation   [Literal]            -- list of tuples
    | EPartition  [[Literal]]          -- list of parts
    | EExpr Expr
    deriving (Show, Generic, Typeable, Eq, Ord, Read)

data TType =
      TInt
    | TBool
    | TMatix  TType
    | TSet    TType
    | TMSet   TType
    | TFunc   TType TType
    | TTuple  [TType]
    | TRel    [TType]
    | TPar    TType
    | TUnamed Text   -- each unamed type is unique
    | TEnum   Text   -- as are enums
    | TAny
  deriving (Show, Generic, Typeable, Eq, Ord, Read)

instance FromJSON TType
instance ToJSON TType

data OObjective = Maximising Expr
                | Minimising Expr
    deriving(Show, Generic, Typeable, Read, Eq)

class ToEssence ast conjure where
  toEssence :: ast -> conjure

class FromEssence conjure ast where
  fromEssence :: conjure -> Either conjure ast
