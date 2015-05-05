{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ParallelListComp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.New where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Conjure.Prelude
import Gen.AST.Imports
import Gen.Essence.St
import Gen.Helpers.Placeholders       (notDone)
import System.Random                  (Random)
import Test.QuickCheck                (generate, choose)
import Gen.Helpers.SizeOf

import qualified Data.Map as M


-- TODO Need a check if there is an op that matches the requested type
-- which can be generated in the available depth
instance Generate Expr where
  give g  = do
    let defs =
          [ (possible (error "" :: Constant), ("ECon",  ECon <$> give g))
          , (possible (error "" :: Op Expr),  ("EOp",   EOp  <$> give g))
          , (possible (error "" :: AbstractLiteral Expr), ("ELit",  wrapLiteral <$> give g))
          ]

    parts <- getPossibilities g defs
    frequency3 parts

  possiblePure _ _ _ = True

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

  possiblePure _ ty d = (depthOf ty) <= (fromIntegral d)


instance Generate a => Generate (AbstractLiteral a) where
  give GNone = do
      ty <- give GOnlyLiteralTypes
      give (GType ty)

  give (GType (TSet ty)) = do
      es <- vectorOf3 2 (withDepthDec $ give (GType ty))
      return $ AbsLitSet es

  give t = giveUnmatched "Generate (AbstractLiteral a)" t

  possiblePure _ TBool _  = False
  possiblePure _ TInt  _  = False
  possiblePure _ ty d = (depthOf ty) <= (fromIntegral d)


-- Need to know the possible return types for each op
instance Generate a => Generate (Op a) where
  give a = do
      ops <- getPossibilities a (allOps a)
      withDepthDec $ frequency3 ops

  possible _ ty = do
    d <- gets depth
    case depthOf ty + 1 <= fromIntegral d of
      False -> return False
      True  -> do
        bs <- mapM (check) (allOps GNone)
        return $ and bs
    where
    check :: MonadState St m
          => ((TType -> m Bool), (Key, GenSt (Op a)))
          -> m Bool
    check (f,_) = f ty



instance Generate a => Generate (OpGeq a) where
  give GNone = give (GType TBool)

  give (GType TBool) = do
    ty <- GType <$> give GNone
    pure OpGeq <*> give ty <*> give ty

  give t = giveUnmatched "Generate (OpGeq a)" t

  -- possiblePure includes the ops type
  possiblePure _ ty _ | ty /= TBool = False
  possiblePure _ ty d = depthOf ty + 1 <= (fromIntegral d)

instance Generate a => Generate (OpEq a) where
  give GNone = give (GType TBool)

  give (GType TBool) = do
    ty <- GType <$> give GNone
    pure OpEq <*> give ty <*> give ty

  give t = giveUnmatched "Generate (OpEq a)" t

  -- possiblePure includes the ops type
  possiblePure _ ty _ | ty /= TBool = False
  possiblePure _ ty d = depthOf ty + 1 <= (fromIntegral d)



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

  possiblePure _ _ _ = True


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

  possiblePure _ _ _ = True


instance Generate TType where
  give GNone = do
    defs <- gets depth >>= \d ->
       if | d < 0     -> error $ "Generate TType invaild Depth: " ++ show d
          | d == 0    -> return [ ("TBool", pure TBool)
                                , ("TInt",  pure TInt)
                                ]
          | otherwise -> return [
                           ("TBool", pure TBool)
                         , ("TInt",  pure TInt)
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

  possiblePure _ _ _ = True


instance Generate Spec where
  give GNone = do
    depth <- gets depth
    let domsCount = (1, min ((depth+1)*2) 7)
    let exprCount = (0, min ((depth+1)*2) 7)
    i_d <- choose3 domsCount
    i_e <- choose3 exprCount

    doms <- mapM (\_ -> give GNone) [1..i_d]
    let withNames =  zipWith (\d i -> (name i , Findd d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames

    exprs <- mapM (\_ -> give (GType TBool) ) [0..i_e]

    return $ Spec mappings exprs Nothing

    where name i =  stringToText $  "var" ++  (show  i)

  give t = giveUnmatched "Generate (Spec)" t


  possiblePure _ _ _ = True


runGenerate :: Generate a => St -> IO a
runGenerate st = generate $ evalStateT (give GNone) st



-- Will be auto genrated
allOps :: forall m a
        . (Generate a, MonadState St m)
       => GenerateConstraint
       -> [((TType -> m Bool ), (Key, GenSt (Op a))) ]
allOps con =
    [
      ( possible (error "getId" :: OpEq a ),  (getId (error "getId" :: OpEq a),   MkOpEq  <$> give con))
    , ( possible (error "getId" :: OpGeq a ), (getId (error "getId" :: OpGeq a),  MkOpGeq <$> give con))
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
