{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module AST.Data where


import GHC.Generics
import Data.Typeable

import Data.Text (Text)
import Data.Hashable ( Hashable(..) )

import Data.Aeson(FromJSON(..),ToJSON(..))
import Data.Data


data Domain
  = DSet
    { size        :: Maybe Integer
    , minSize     :: Maybe Integer
    , maxSize     :: Maybe Integer
    , inner       :: Domain
    }
  | DMSet
    { size        :: Maybe Integer
    , minSize     :: Maybe Integer
    , maxSize     :: Maybe Integer
    , minOccur    :: Maybe Integer
    , maxOccur    :: Maybe Integer
    , inner       :: Domain
    }
  | DFunc
    { size        :: Maybe Integer
    , maxSize     :: Maybe Integer
    , minSize     :: Maybe Integer
    , surjective  :: Bool
    , injective   :: Bool
    , total       :: Bool
    , innerFrom   :: Domain
    , innerTo     :: Domain
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
    , inner       :: Domain
    }
  | DRel
    { size        :: Maybe Integer
    , maxSize     :: Maybe Integer
    , minSize     :: Maybe Integer
    , reflexive   :: Bool
    , symmetric   :: Bool
    , inners      :: [Domain] -- tuples
    }
  | DTuple
    { inners :: [Domain]
    }
  | DInt
    { ranges      :: [Range Expr]
    }
  | DMat
    { innerIdx    :: Domain
    , inner       :: Domain
    }
  | DBool
  -- enums
  -- unamed types
    deriving(Show, Generic, Typeable, Eq, Ord, Read)

instance Hashable Domain
instance Hashable (Range Expr)
instance Hashable (Type)
instance Hashable (Expr)
instance Hashable (QType)
instance Hashable (BinOp)
instance Hashable (Literal)
instance Hashable (Proc)
instance Hashable (UniOp)


data Range a =
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
  | EDom Domain
  | ETyped Type Expr
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
    | EMatrix     [Literal] Domain
    | ESet        [Literal]
    | EMSet       [Literal]
    | EFunction   [(Literal, Literal)] -- list of mappings
    | ERelation   [Literal]            -- list of tuples
    | EPartition  [[Literal]]          -- list of parts
    | EExpr Expr
    deriving (Show, Generic, Typeable, Eq, Ord, Read)

data Type =
      TInt
    | TBool
    | TMatix  Type
    | TSet    Type
    | TMSet   Type
    | TFunc   Type Type
    | TTuple  [Type]
    | TRel    [Type]
    | TPar    Type
    | TUnamed Text   -- each unamed type is unique
    | TEnum   Text   -- as are enums
    | TAny
  deriving (Show, Generic, Typeable, Eq, Ord, Read)

instance FromJSON Type
instance ToJSON Type

data Objective = Maximising Expr
               | Minimising Expr
    deriving(Show, Generic, Typeable, Read, Eq)
