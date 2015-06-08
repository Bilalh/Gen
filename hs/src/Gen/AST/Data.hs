{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable, FlexibleInstances, KindSignatures,
             MultiParamTypeClasses #-}
module Gen.AST.Data where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Conjure.Language.Pretty
import Conjure.Language.Type
import Conjure.Prelude
import Text.Groom                     (groom)


class (Pretty ast, Pretty conjure, Show ast) => Translate ast conjure where
  -- Should never cause an error but...
  toConjure   :: MonadFail m => ast -> m conjure
  -- Should never cause an error but...
  fromConjure :: MonadFail m => conjure -> m ast

  toConjureNote :: Doc -> ast -> conjure
  toConjureNote msg a = case toConjure a  of
                  Right x -> x
                  Left  d -> error . renderNormal . vcat $ ["toConjureNote", msg, d ]

  fromConjureNote :: Doc -> conjure -> ast
  fromConjureNote msg a =  case fromConjure a of
                    Right x -> x
                    Left  d -> error . renderNormal . vcat $ ["fromConjureNote", msg, d ]

type Domainn x = Domain () x
type Literal = AbstractLiteral Expr


-- | Outputs quantifiers e.g. exists instead of converting them to list comparisons
class Pretty a => PrettyWithQuan a where
    prettyWithQuan :: Pretty a => a -> Doc

type ListComp =  (Expr, [EGen], [Expr])
data Expr =
    EVar Var
  | ECon Constant
  | ELit Literal
  | EOp (Op Expr)
  | ETyped Type Expr
  | EDom (Domainn Expr)

  | EMetaVar String -- For TH
  | EEmptyGuard
  | EQuan QType Var  -- should var be a  litetal?
    Expr -- over/ in expr
    Expr -- guard
    Expr -- inner

  | EComp
    Expr    -- inner
    [EGen]  -- Generators
    [Expr]  -- Conditions

  deriving (Eq, Ord, Show, Data, Typeable, Generic)


instance Serialize (Expr)
instance Hashable  (Expr)
instance ToJSON    (Expr) where toJSON    = genericToJSON jsonOptions
instance FromJSON  (Expr) where parseJSON = genericParseJSON jsonOptions


data EGen =
    GenDom AbstractPattern (Domainn Expr)
  | GenIn  AbstractPattern Expr
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize (EGen)
instance Hashable  (EGen)
instance ToJSON    (EGen) where toJSON    = genericToJSON jsonOptions
instance FromJSON  (EGen) where parseJSON = genericParseJSON jsonOptions


data Var = Var Text Type
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty Var where
    pretty = pretty . groom

instance Serialize (Var)
instance Hashable  (Var)
instance ToJSON    (Var) where toJSON    = genericToJSON jsonOptions
instance FromJSON  (Var) where parseJSON = genericParseJSON jsonOptions


data QType = ForAll
           | Exists
           | Sum
           deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize QType
instance Hashable  QType
instance ToJSON    QType where toJSON = genericToJSON jsonOptions
instance FromJSON  QType where parseJSON = genericParseJSON jsonOptions



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
