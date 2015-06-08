{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Objective(Generate(..)) where

import Gen.Imports
import Gen.Essence.St
import Gen.Essence.Rnd
import Gen.Helpers.TypeOf

import qualified Data.Foldable as F
import qualified Data.Map as M

instance Generate (Maybe (OObjective, Expr)) where
  give GNone = possible (Proxy :: Proxy (OObjective, Expr)) GNone >>= \case
    False -> return Nothing
    True  -> do
      val <- weightingForKey K_PickObjective
      let (k_obj,k_none) = if val <= 0 then
                               (0, 100)
                           else
                               (val, max 0 (100 - val)  )

      frequency3 [ (k_none, pure Nothing)
                 , (k_obj, Just <$> give GNone)
                 ]

  give t = giveUnmatched "Generate (Maybe (OObjective, Expr))" t

  possible _ con = possible (Proxy :: Proxy (OObjective, Expr)) con

instance Generate (OObjective, Expr) where
  give GNone = do
    ds <- gets doms_
    let pairs = [  (name, runIdentity $ ttypeOf d )
                |  (name,Findd d)  <- M.toList ds
                ]
        tys = [ (name,ty) | (name,ty) <- pairs,  allow ty]

    (name,ty) <- elements3 tys
    kind <- elements3 [Maximisingg, Minimisingg]
    return $ (kind, (EVar $ Var name ty ))


  give t = giveUnmatched "Generate (OObjective, Expr)" t

  possible _ GNone = do
    let
    ds <- gets doms_
    F.foldrM f False ds

    where
    f _  True  = return True
    f gf False = do
       allow <$> ttypeOf gf


  possible _ con = possibleUnmatched "possible ((OObjective, Expr)" con

allow :: Type -> Bool
allow TypeInt = True
allow _       = False
