{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ParallelListComp#-}
module Gen.Essence.New where

import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Conjure.Prelude
import Data.Map                       (Map)
import Gen.AST.Imports
import Gen.AST.TH
import Gen.Essence.St
import Gen.Helpers.Placeholders       (notDone)
import System.Random                  (Random)
import Test.QuickCheck                hiding (give)


instance Generate Expr where
-- --TODO how would I get this to generate Op Constant?
  give g = do
      defs <- gets depth >>= \case
        0 -> return [ ("ECon", ECon <$> give g) ]
        _ -> return [ ("ECon", ECon <$> give g)
                    , ("EOp",  EOp  <$> give g)
                    ]

      parts <- withWeights defs
      frequency3 parts


instance Generate Constant where
  give GNone = do
      ty <- give GNone
      give (GType ty)

  give (GType TInt)      = pure ConstantInt      <*> choose3 (0,5)
  give (GType TBool)     = pure ConstantBool     <*> choose3 (True,False)
  give (GType ty@TSet{}) = pure ConstantAbstract <*> give (GType ty)


instance Generate a => Generate (AbstractLiteral a) where
  give GNone = do
      ty <- give GNone
      give (GType ty)

  give (GType (TSet ty)) = do
      es <- vectorOf3 2 (give (GType ty))
      return $ AbsLitSet es


-- Need to know the possible return types for each op
instance Generate a => Generate (Op a) where
  give a = do
      ops <- withWeights (allOps a)
      withDepthDec $ frequency3 ops


instance Generate a => Generate (OpGeq a) where
    give GNone = give (GType TBool)

    give (GType TBool) = do
      -- ty <- pure GType <*> give GNone
      ty <- pure (GType TInt)
      pure OpGeq <*> (give ty) <*> (give ty)


instance (Generate a, WrapConstant a) => Generate (Domain () a) where
  give GNone = do
      -- ty <- give GNone
      give (GType TInt)

  give (GType TBool)           = return DomainBool
  give (GType TInt)            = pure DomainInt <*> vectorOf3 2 (give GNone)
  -- give (GType (TMatix ty))     = _x
  -- give (GType (TSet ty))       = _x
  -- give (GType (TMSet ty))      = _x
  -- give (GType (TFunc ty1 ty2)) = _x
  -- give (GType (TTuple ty))     = _x
  -- give (GType (TRel ty))       = _x
  -- give (GType (TPar ty))       = _x
  -- give (GType (TUnamed ty))    = _x
  -- give (GType (TEnum ty))      = _x
  -- give (GType TAny)            = _x


instance (Generate a, WrapConstant a) => Generate (Range a) where
    give GNone = do
        parts <- withWeights [("RangeSingle", single),("RangeBounded", bounded) ]
        frequency3 parts

      where
        single  = do
          a <- choose3 (0,5 :: Integer)
          return $ RangeSingle (wrapConstant . ConstantInt $ a)
        bounded = do
          a <- choose3 (0,5 :: Integer)
          b <- choose3 (a,5)
          return $ RangeBounded (wrapConstant . ConstantInt $ a)
                                (wrapConstant . ConstantInt $ b)


instance Generate TType where
  give GNone = do
      defs <- gets depth >>= \d ->
         if | d < 0     -> error "invaild Depth"
            | d == 0    -> return [ ("TInt",  pure TInt)
                                  , ("TBool", pure TBool) ]
            | otherwise -> return [
                             ("TSet",   liftM TSet   (withDepthDec (give GNone) ))
                           -- , ("TMatix", liftM TMatix (withDepthDec (give GNone) ))
                           -- , ("TMSet",  liftM TMSet  (withDepthDec (give GNone) ))
                           -- , ("TPar",   liftM TPar   (withDepthDec (give GNone) ))
                           ]

      parts <- withWeights defs
      frequency3 parts



runGenerate :: Generate a => St -> IO a
runGenerate st = generate $ evalStateT (give GNone) st

withWeights :: MonadState St m => [(Key,a) ] -> m [(Int, a)]
withWeights vs= do
  weights <- mapM (\(x,_) -> weightingForKey x  ) vs
  return [ (w,p) | (_,p) <- vs
                 | w <- weights  ]

withDepthDec :: GenSt a -> GenSt a
withDepthDec f = do
    oldDepth <- gets depth
    modify $ \st -> st{ depth = oldDepth - 1 }
    res <- f
    modify $ \st -> st{ depth = oldDepth }
    return res

-- Will be auto genrated
allOps :: forall a . Generate a
       => GenerateConstraint
       -> [(Key, GenSt (Op a)) ]
allOps con =
    [
      (getId (error "getId" :: OpGeq a ), MkOpGeq <$> give con)
    ]



-- To Move

frequency3 :: [(Int, GenSt a)] -> GenSt a
frequency3 [] = error "frequency3 used with empty list"
frequency3 xs0 =  choose3 (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "frequency3.pick used with empty list"

-- choose3 :: Random a => (a,a) -> GenSt a
choose3 rng = lift $ choose rng

vectorOf3 :: Int -> GenSt a -> GenSt [a]
vectorOf3 k gen = sequence [ gen | _ <- [1..k] ]
