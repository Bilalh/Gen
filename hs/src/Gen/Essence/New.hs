{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ParallelListComp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.New where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Conjure.Prelude
import Gen.AST.Imports
import Gen.AST.TH
import Gen.Essence.St
import Gen.Helpers.Placeholders       (notDone)
import System.Random                  (Random)
import Test.QuickCheck                (generate, choose)


instance Generate Expr where
  give g@(GType ty) | ty == TBool || ty == TInt = do
      defs <- gets depth >>= \case
        0 -> return [ ("ECon",  ECon <$> give g) ]
        _ -> return [ ("ECon",  ECon <$> give g)
                    , ("EOp",   EOp  <$> give g)
                    ]

      parts <- getWeights defs
      frequency3 parts

  give g = do
      defs <- gets depth >>= \case
        0 -> return [ ("ECon",  ECon <$> give g) ]
        _ -> return [ ("ECon",  ECon <$> give g)
                    , ("EOp",   EOp  <$> give g)
                    , ("ELit",  wrapLiteral <$> give g)
                    ]

      parts <- getWeights defs
      frequency3 parts

-- Put a Typed around empty lits e.g a empty set
wrapLiteral ::  AbstractLiteral Expr -> Expr
wrapLiteral a = ELit a

instance Generate Constant where
  give GNone = do
      ty <- give GNone
      give (GType ty)

  give (GType TInt)      = pure ConstantInt      <*> choose3 (0,5)
  give (GType TBool)     = pure ConstantBool     <*> choose3 (True,False)
  give (GType ty@TSet{}) = pure ConstantAbstract <*> give (GType ty)

  give t = giveUnmatched "Generate Constant" t


instance Generate a => Generate (AbstractLiteral a) where
  give GNone = do
      ty <- give GOnlyLiteralTypes
      give (GType ty)

  give (GType (TSet ty)) = do
      es <- vectorOf3 2 (give (GType ty))
      return $ AbsLitSet es

  give t = giveUnmatched "Generate (AbstractLiteral a)" t


-- Need to know the possible return types for each op
instance Generate a => Generate (Op a) where
  give a = do
      ops <- getWeights (allOps a)
      withDepthDec $ frequency3 ops


instance Generate a => Generate (OpGeq a) where
  give GNone = give (GType TBool)

  give (GType TBool) = do
    -- ty <- pure GType <*> give GNone
    ty <- pure (GType TInt)
    pure OpGeq <*> (give ty) <*> (give ty)

  give t = giveUnmatched "Generate (OpGeq a)" t


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


  give t = giveUnmatched "Generate (Domain () a)" t


instance (Generate a, WrapConstant a) => Generate (Range a) where
  give GNone = do
      parts <- getWeights [("RangeSingle", single),("RangeBounded", bounded) ]
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

  give t = giveUnmatched "Generate (Range a)" t

instance Generate TType where
  give GNone = do
    defs <- gets depth >>= \d ->
       if | d < 0     -> error $ "Generate TType invaild Depth: " ++ show d
          | d == 0    -> return [ ("TBool", pure TBool)
                                -- , ("TInt",  pure TInt)
                                ]
          | otherwise -> return [
                           ("TBool", pure TBool)
                         -- , ("TInt",  pure TInt)
                         , ("TSet",   liftM TSet   (withDepthDec (give GNone) ))
                         -- , ("TMatix", liftM TMatix (withDepthDec (give GNone) ))
                         -- , ("TMSet",  liftM TMSet  (withDepthDec (give GNone) ))
                         -- , ("TPar",   liftM TPar   (withDepthDec (give GNone) ))
                         ]

    parts <- getWeights defs
    frequency3 parts

  give GOnlyLiteralTypes = do
    defs <- gets depth >>= \d ->
       if | d <= 0     -> error $ "Generate TType(literal) invaild Depth: " ++ show d
          | otherwise -> return [
                           ("TSet",   liftM TSet   (withDepthDec (give GNone) ))
                         -- , ("TMatix", liftM TMatix (withDepthDec (give GNone) ))
                         -- , ("TMSet",  liftM TMSet  (withDepthDec (give GNone) ))
                         -- , ("TPar",   liftM TPar   (withDepthDec (give GNone) ))
                         ]

    parts <- getWeights defs
    frequency3 parts

  give t = giveUnmatched "Generate (TType)" t



runGenerate :: Generate a => St -> IO a
runGenerate st = generate $ evalStateT (give GNone) st

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

choose3 :: Random a => (a,a) -> GenSt a
choose3 rng = lift $ choose rng

vectorOf3 :: Int -> GenSt a -> GenSt [a]
vectorOf3 k gen = sequence [ gen | _ <- [1..k] ]
