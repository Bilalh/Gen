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


newtype TyId = TyId Int
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Ty = Ty     TyId Type
        | TyRef  TyId
        | TyMult TyId [Type]
        -- | TyDep  TyId (Map Type Type)  -- Needed?
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

class Show a => OpInfo a where
    artity      :: a -> Int
    minDepth    :: a -> Int
    argsTypes   :: a -> Type -> [Ty]

class ApplyOp a e where
  applyArgs ::  (a e -> Op e) -> [e] -> (Op e)


instance OpInfo (OpMod e -> Op e) where
    artity _            =  2
    minDepth _          =  1
    argsTypes _ TypeInt =  [ Ty (TyId 1) TypeInt, Ty (TyId 2) TypeInt ]

instance Show (OpMod e -> Op e) where
    show _ = "(OpMod e -> Op e)"

instance ApplyOp OpMod e where
    applyArgs f [e1,e2] =  f (OpMod e1 e2)

instance OpInfo (OpGeq e -> Op e) where
    artity _             =  2
    minDepth _           =  1
    argsTypes _ TypeBool =  [ Ty (TyId 1) TypeInt, Ty (TyId 2) TypeInt ]

instance Show (OpGeq e -> Op e) where
    show _ = "(OpGeq e -> Op e)"

instance ApplyOp OpGeq e where
    applyArgs f [e1,e2] =  f (OpGeq e1 e2)


instance OpInfo (OpOr e -> Op e) where
    artity _             =  2
    minDepth _           =  1
    argsTypes _ TypeBool =  [ Ty (TyId 1) (TypeMatrix TypeInt TypeBool)]

instance Show (OpOr e -> Op e) where
    show _ = "(OpOr e -> Op e)"

instance ApplyOp OpOr e where
    applyArgs f [e1] =  f (OpOr e1)



class (Monad m, Applicative m) => MonadGen m where
    --   0    Never chosen
    --   100  Normal chance
    --  >100  Incressed chance
    getOpWeighting :: (a e -> Op e) -> m Int

instance MonadGen Identity where
    getOpWeighting _ =  return 100


-- allowdOpsForType :: MonadGen m
--                  => Type
--                  -> m [a x -> Op x]
-- allowdOpsForType TypeInt = return [ MkOpMod  ]
-- allowdOpsForType TypeBool = return [ MkOpOr, MkOpGt]
allowdOpsForType TypeBool = return [ MkOpOr]

opForType :: MonadGen m => Type -> Int -> m (Maybe Expr)
opForType ty depth =
  filter (\p -> depth >= minDepth p ) <$> allowdOpsForType ty >>= \case
   [] -> return Nothing
   allowed -> do
    ws      <- mapM getOpWeighting allowed
    picked  <- frequency3 32 (zip ws allowed)

    es <- mapM exprOfType (argsTypes picked ty)
    let op  = applyArgs picked es
    return . Just $ (EOp op)


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
  pick _ _  = error "QuickCheck2.pick used with empty list"


-- Using names

data OpName
    = NOpGeq
    | NOpMod
    | NOpOr
  deriving (Eq, Ord, Show, Data, Typeable, Generic)


data OpInfo_ = OpInfo_
    { artity_      :: Int
    , minDepth_    :: Int
    , argsTypes_   :: Map Type [Ty]
    }   deriving (Eq, Show, Ord, Data, Typeable, Generic)

op_info :: OpName -> OpInfo_
op_info NOpGeq = OpInfo_ { artity_   = 2
                         , minDepth_ = 1
                         , argsTypes_ = M.fromList [
                               (TypeBool,  [ Ty (TyId 1) TypeInt, Ty (TyId 2) TypeInt ])
                               ]
                         }

op_info NOpOr = OpInfo_ { artity_   = 2
                         , minDepth_ = 1
                         , argsTypes_ = M.fromList [
                               (TypeBool, [ Ty (TyId 1) (TypeMatrix TypeInt TypeBool)])
                               ]
                         }

applyArgs_ :: OpName -> [e] -> (Op e)
applyArgs_ NOpGeq [e1,e2] = MkOpGeq (OpGeq e1 e2)
applyArgs_ NOpOr [e1]  = MkOpOr (OpOr e1)

argsLookup_ :: OpName -> Type -> [Ty]
argsLookup_ n t = fromJustNote "argsTypesL_" .  (t `M.lookup`) . argsTypes_ . op_info $ n

allowdOpsForType_ TypeBool = return [ NOpGeq, NOpOr ]

opForType_ :: MonadGen_ m => Type -> Int -> m (Maybe Expr)
opForType_ ty depth =
  filter (\p -> depth >= (minDepth_ . op_info) p ) <$> allowdOpsForType_ ty >>= \case
   [] -> return Nothing
   allowed -> do
    ws      <- mapM getOpWeighting_ allowed
    picked  <- frequency3 132 (zip ws allowed)

    es <- mapM exprOfType ( argsLookup_  picked  ty)
    let op  = applyArgs_ picked es
    return . Just $ (EOp op)


class (Monad m, Applicative m) => MonadGen_ m where
    getOpWeighting_ :: OpName -> m Int

instance MonadGen_ Identity where
    getOpWeighting_ _ =  return 100


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

opForType2 :: MonadGen_ m => Type -> Int -> m (Maybe Expr)
opForType2 ty depth =
  filter (\p -> depth >= minDepth p ) <$> allowdOpsForType_ ty >>= \case
   [] -> return Nothing
   allowed -> do
    ws      <- mapM getOpWeighting_ allowed
    picked  <- frequency3 132 (zip ws allowed)

    es <- mapM exprOfType (argsTypes picked ty)
    let op  = applyArgs_ picked es
    return . Just $ (EOp op)
