-- Idea for Better Genration

{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable, FlexibleInstances, KindSignatures,
             MultiParamTypeClasses #-}
module Gen.Essence.Info where

import Conjure.Language.Expression.Op.Internal.Generated (Op (..))
import Conjure.Language.Expression.Op.Mod
import Conjure.Language.Type
import Conjure.Language.Definition
import Conjure.Prelude
-- import Data.Map                                          (Map)
import Gen.AST.Imports


newtype TyId = TyId Int
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Ty = Ty     TyId Type
        | TyRef  TyId
        | TyMult TyId [Type]
        -- | TyDep  TyId (Map Type Type)  -- Needed?
  deriving (Eq, Ord, Show, Data, Typeable, Generic)


class OpInfo a where
    artity      :: a -> Int
    minDepth    :: a -> Int
    argsTypes   :: a -> Type -> [Ty]

class OpInfo a => OpApply e a where
  applyArgs   :: a -> [e] -> (Op e)


instance OpInfo (OpMod e -> Op e) where
    artity _            =  2
    minDepth _          =  1
    argsTypes _ TypeInt =  [ Ty (TyId 1) TypeInt, Ty (TyId 2) TypeInt ]

instance OpApply e (OpMod e -> Op e) where
    applyArgs f [e1,e2] =  f (OpMod e1 e2)

instance Show (OpMod e -> Op e) where
    show _ = "(OpMod e -> Op e)"


allowdOpsForType :: forall (m :: * -> *) x . Monad m
                 => Type
                 -> m [OpMod x -> Op x]
allowdOpsForType TypeInt = return [ MkOpMod  ]

opForType :: Monad m => Type -> Int -> m (Maybe Expr)
opForType ty depth = do
  opsAllowed ::  [OpMod Expr -> Op Expr] <- allowdOpsForType ty
  let picked = opsAllowed `at` 0
  case depth >= minDepth picked of
    False -> return Nothing
    True  -> do
      es <- mapM exprOfType (argsTypes picked ty)
      let op :: Op Expr = applyArgs picked es
      return . Just $ (EOp op)


exprOfType :: Monad m => Ty -> m Expr
exprOfType (Ty _ TypeInt) = return $ ECon (ConstantInt 1)
