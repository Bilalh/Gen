{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable, FlexibleInstances, KindSignatures,
             MultiParamTypeClasses, QuasiQuotes #-}
module Gen.Essence.Info where

-- Idea for Generic Genration of ops
-- OpInfo instance for artity, depth and arg types for each op
-- function from type to op

import Conjure.Language.Expression.Op.Internal.Generated (Op (..))
import Conjure.Language.Expression.Op.Mod
import Conjure.Language.Expression.Op.Geq
import Conjure.Language.Expression.Op.Or
import Conjure.Language.Type
import Conjure.Language.Definition
import Conjure.Prelude
import Data.Map                                          (Map)
import qualified Data.Map as M
import Gen.AST.Imports
import Gen.AST.TH


data Ty = Ty     TyId Type
        | TyRef  TyId
        | TyMult TyId [Type]
        -- | TyDep  TyId (Map Type Type)  -- Needed?
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype TyId = TyId Int
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

class OpInfo a where
    artity      :: a -> Int
    minDepth    :: a -> Int
    argsTypes   :: a -> Type -> [Ty]

instance OpInfo (OpMod e -> Op e) where
    artity _            =  2
    minDepth _          =  1
    argsTypes _ TypeInt =  [ Ty (TyId 1) TypeInt, Ty (TyId 2) TypeInt ]

instance OpInfo (OpGeq e -> Op e) where
    artity _             =  2
    minDepth _           =  1
    argsTypes _ TypeBool =  [ Ty (TyId 1) TypeInt, Ty (TyId 2) TypeInt ]

instance OpInfo (OpOr e -> Op e) where
    artity _             =  2
    minDepth _           =  1
    argsTypes _ TypeBool =  [ Ty (TyId 1) (TypeMatrix TypeInt TypeBool)]

class (Monad m, Applicative m) => MonadGen m where
    --   0    Never chosen
    --   100  Normal chance
    --  >100  Incressed chance
    getOpWeighting :: OpName -> m Int
    setOpWeighting :: OpName -> Int ->  m ()

instance MonadGen Identity where
    getOpWeighting _ =  return 100
    setOpWeighting _ =  error "setOpWeighting_ in Identity"


data OpName
    = NOpGeq
    | NOpMod
    | NOpOr
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance OpInfo OpName where
  artity NOpMod = artity MkOpMod
  artity NOpOr  = artity MkOpOr
  artity NOpGeq = artity NOpGeq

  minDepth NOpMod = minDepth MkOpMod
  minDepth NOpOr  = minDepth MkOpOr
  minDepth NOpGeq = minDepth MkOpGeq

  argsTypes NOpMod = argsTypes MkOpMod
  argsTypes NOpOr  = argsTypes MkOpOr
  argsTypes NOpGeq = argsTypes MkOpGeq

applyArgs_ :: OpName -> [e] -> (Op e)
applyArgs_ NOpGeq [e1,e2]  = MkOpGeq (OpGeq e1 e2)
applyArgs_ NOpOr [e1]      = MkOpOr (OpOr e1)
applyArgs_ NOpMod [e1, e2] = MkOpMod (OpMod e1 e2)


opForType :: MonadGen m => Type -> Int -> m (Maybe Expr)
opForType ty depth =
  filter (\p -> depth >= minDepth p ) <$> allowdOpsForType ty >>= \case
   [] -> return Nothing
   allowed -> do
    ws      <- mapM getOpWeighting allowed
    picked  <- frequency3 32 (zip ws allowed)

    es <- mapM exprOfType (argsTypes picked ty)
    let op  = applyArgs_ picked es
    return . Just $ (EOp op)


-- Stubs
allowdOpsForType ::  Monad m => Type -> m [OpName]
allowdOpsForType TypeBool = return [ NOpGeq, NOpOr ]
allowdOpsForType TypeInt  = return [ NOpMod  ]


exprOfType :: Monad m => Ty -> m Expr
exprOfType (Ty _ TypeInt)  = return $ ECon (ConstantInt 1)
exprOfType (Ty _ TypeBool) = return $ ECon (ConstantBool True)
exprOfType (Ty _ (TypeMatrix TypeInt TypeBool)) = return [essencee| [true,false] |]

-- frequency3 is frequency2 that which is given the random choice
frequency3 :: (Monad m) => Int ->  [(Int, a)] -> m a
frequency3 rnd [] = error "frequency3 used with empty list"
frequency3 rnd xs0 = return $ rnd `pick` xs0
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "frequency3.pick used with empty list"
