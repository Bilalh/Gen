{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable, FlexibleInstances, KindSignatures,
             MultiParamTypeClasses #-}
module Gen.Essence.Info where

-- Idea for Generic Genration of ops
-- OpInfo instance for artity, depth and arg types for each op
-- function from type to op

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

class ApplyOp a e where
  applyArgs ::  (a e -> Op e) -> [e] -> (Op e)

class (Monad m, Applicative m) => MonadGen m where
    --   0    Never chosen
    --   100  Normal chance
    --  >100  Incressed chance
    getOpWeighting :: (a e -> Op e) -> m Int


instance MonadGen Identity where
    getOpWeighting _ =  return 100

instance OpInfo (OpMod e -> Op e) where
    artity _            =  2
    minDepth _          =  1
    argsTypes _ TypeInt =  [ Ty (TyId 1) TypeInt, Ty (TyId 2) TypeInt ]

instance Show (OpMod e -> Op e) where
    show _ = "(OpMod e -> Op e)"

instance ApplyOp (OpMod) e where
    applyArgs f [e1,e2] =  f (OpMod e1 e2)


allowdOpsForType :: MonadGen m
                 => Type
                 -> m [OpMod x -> Op x]
allowdOpsForType TypeInt = return [ MkOpMod  ]

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
exprOfType (Ty _ TypeInt) = return $ ECon (ConstantInt 1)


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
