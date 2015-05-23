{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Constant(Generate(..)) where

import Conjure.Language.Definition
import Gen.Essence.Id
import Gen.Essence.Literal         ()
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type            ()
import Gen.Helpers.SizeOf
import Gen.Imports

import qualified Gen.Essence.Data.Types as Types

instance Generate Constant where
  give con = do
   (res, _ :: Type) <- give con
   return res

  possiblePure _ (Just ty) d = fromIntegral d >= depthOf ty
  possiblePure _ _ _         = True

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []

instance Generate (Constant, Type) where
  give GNone = do
    ty <- give GNone
    give (GType ty)

  give (GType TypeInt)  = pure ConstantInt  <*> choose3 (0,5)        >>= z TypeInt
  give (GType TypeBool) = pure ConstantBool <*> choose3 (True,False) >>= z TypeBool
  give (GType ty) | Types.isLiteral ty = lit ty

  give t = giveUnmatched "Generate Constant" t

  possiblePure _ (Just ty) d = fromIntegral d >= depthOf ty
  possiblePure _ _ _         = True

  requires _ (Just ty) = [RAll $ keyList ty]
  requires _ _         = []

z :: Monad m => t -> a -> m (a, t)
z r = return . (, r)

lit :: Type -> GenSt (Constant, Type)
lit ty = do
  (val, lty) :: (AbstractLiteral Constant, Type) <- give (GType ty)
  return (ConstantAbstract val, lty)
