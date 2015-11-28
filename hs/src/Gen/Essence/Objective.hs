{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Objective(Generate(..)) where

import Gen.Imports
import Gen.Essence.St
import Gen.Essence.Rnd
import Gen.Helpers.TypeOf
import Data.Generics.Uniplate.Data

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable              as R

instance (Generate a, WrapVar a, TTypeOf a)
      => Generate (Maybe (OObjective, a)) where
  give GNone = possible (Proxy :: Proxy (OObjective, a)) GNone >>= \case
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

  possible _ con = possible (Proxy :: Proxy (OObjective, a)) con

instance (Generate a, WrapVar a, TTypeOf a)
      => Generate (OObjective, a) where
  give GNone = do
    logDepthCon $line GNone
    ds <- gets doms_
    (ky,kind) <- elements3 $line [(K_Maximisingg, Maximisingg), (K_Minimisingg, Minimisingg)]
    e :: a <- withKey ky $ give (GType TypeInt)

    -- Checks if a decision variable is referenced
    let names = S.fromList $  namesUsed e
    if (not $ S.null names) && names `S.isSubsetOf` M.keysSet ds then do
      logInfo2 $line [ "Using e"  <+> (pretty e)
                     , "names" <+> (pretty $ groom $ namesUsed e)  ]
      return $ (kind, e)
    else do
      -- Try to replace a sub-expression with a decision variable reference
      let pairs =  [  (name, runIdentity $ ttypeOf (domOfGF $ snd d) )
                   |  (name, d)  <- M.toList ds ]
          tys = [ (name,ty) | (name,ty) <- pairs,  allow ty]

      (name,ty) <- elements3 $line tys
      let var = wrapVar $ Var name ty

      let (e_new,replaced) = replaceExpr e var
      logInfo2 $line ["e"     <+> (pretty e)
                     ,"e_new" <+> (pretty e_new)
                     ]

      -- Check if were able to replace an sub-expression
      -- If not we give up and just return a decision varible of type Int
      if replaced then
        return $ (kind, e_new)
      else
        return $ (kind, var)


  give t = giveUnmatched "Generate (OObjective, Expr)" t

  possible _ GNone = do
    let
    ds <- gets doms_
    F.foldrM f False ds

    where
    f _  True  = return True
    f (_,gf) False = do
       allow <$> ttypeOf gf


  possible _ con = possibleUnmatched "possible ((OObjective, Expr)" con

-- | Replace the first sub-expression of A
-- if that sub-expression has type Int with v
-- returns (A changed, if replaced )
replaceExpr :: (TTypeOf a, Data a) => a -> a -> (a,Bool)
replaceExpr op new = flip runState False $ f1 <$> R.mapM fff ch1
   where
     (ch1, f1) = biplate op
     fff (c :: a) = do
       (done::Bool) <- get
       if not done then do
           if (runIdentity $ ttypeOf c) == TypeInt then do
             put True
             return new
           else
             return c
       else
         return c


allow :: Type -> Bool
allow TypeInt = True
allow _       = False
