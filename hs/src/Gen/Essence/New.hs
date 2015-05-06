{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ParallelListComp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.New where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Gen.Essence.St
import Gen.Helpers.SizeOf
import Gen.Helpers.StandardImports
import Gen.Helpers.TypeOf
import System.Random                  (Random)
import Test.QuickCheck                (choose)

import qualified Data.Foldable as F
import qualified Data.Map      as M


instance Generate Expr where
  give g  = do
    let defs =
          [ (possible (Proxy :: Proxy Constant), ("ECon",  ECon <$> give g))
          , (possible (Proxy :: Proxy  Var),      ("EVar",  EVar  <$> give g))
          , (possible (Proxy :: Proxy (Op Expr)),  ("EOp",   EOp  <$> give g))
          , (possible (Proxy :: Proxy (AbstractLiteral Expr)), ("ELit",  wrapLiteral <$> give g))
          ]

    parts <- getPossibilities g defs
    frequency3 parts

    where
    -- Put a Typed around empty lits e.g a empty set
    wrapLiteral ::  AbstractLiteral Expr -> Expr
    wrapLiteral a = ELit a

  possiblePure _ _ _ = True


instance Generate Var where
  give (GType ty) = do
    ds <- gets doms_
    let ks = M.toList . M.filter (== ty) . M.map (typeOfDom . domOfGF) $ ds
    let choices =  map (return . uncurry Var) ks
    oneof3 choices

  give t = giveUnmatched "Generate Var" t

  possible _ ty = do
    ds <- gets doms_
    F.foldrM f False ds

    where
    f _  True  = return True
    f gf False = do
       b <- ttypeOf gf
       return $ b == ty


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
        return $ or bs
    where
    check :: MonadState St m
          => ((TType -> m Bool), (Key, GenSt (Op a)))
          -> m Bool
    check (f,_) = f ty


instance Generate a => Generate (OpGeq a) where
  give GNone = give (GType TBool)

  give (GType TBool) = do
    ws <- getWeights [ ("TInt", pure TInt)
                     , ("TBool", pure TBool)]
    ty <- GType <$> frequency3 ws
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

  -- possible counts the ops in calculation
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
    modify $ \st -> st{doms_=mappings}

    exprs <- mapM (\_ -> give (GType TBool) ) [0..i_e]
    return $ Spec mappings exprs Nothing

    where name i =  stringToText $  "var" ++  (show  i)

  give t = giveUnmatched "Generate (Spec)" t


  possiblePure _ _ _ = True


-- Will be auto genrated
allOps :: forall m a
        . (Generate a, MonadState St m, Applicative m)
       => GenerateConstraint
       -> [((TType -> m Bool ), (Key, GenSt (Op a))) ]
allOps con =
    [
      ( possible (Proxy :: Proxy (OpEq a) ),  (getId (Proxy :: Proxy (OpEq a)),   MkOpEq  <$> give con))
    , ( possible (Proxy :: Proxy (OpGeq a) ), (getId (Proxy :: Proxy (OpGeq a)),  MkOpGeq <$> give con))
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

oneof3 :: [GenSt a] -> GenSt a
oneof3 [] = error "oneof3 used with empty list"
oneof3 gs = choose3 (0,length gs - 1) >>=   (gs `at`)


vectorOf3 :: Int -> GenSt a -> GenSt [a]
vectorOf3 k gen = sequence [ gen | _ <- [1..k] ]

elements3 :: [a] -> GenSt a
elements3 as  = lift $ elements as
