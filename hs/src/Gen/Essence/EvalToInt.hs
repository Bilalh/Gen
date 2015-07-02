{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.EvalToInt where

import Conjure.Language.Definition
import Conjure.Language.Instantiate
import Gen.AST.TH
import Gen.Essence.St
import Gen.Essence.Rnd
import Gen.Essence.Type             ()
import Gen.Imports

class EvalToInt a where
  evalToInt     :: a -> GenSt Integer
  ensureGTE0    :: a -> GenSt a
  adjustInRange :: a -> Integer -> Integer -> GenSt (a, Integer)


instance EvalToInt Expr where
  evalToInt x = do
    cx :: Expression <- toConjure x
    evalToInt cx

  ensureGTE0 x = do
    i <- evalToInt x
    if  | i == (-99) -> do
          j <- choose3 (0,5)
          return . ECon . ConstantInt $ max 0 j

        | i >= 0     -> return x
        | otherwise  -> do
          let c = ECon $ ConstantInt (0 - i)
          return [essencee| &x + &c |]

  adjustInRange a low high = do
    i <- evalToInt a
    if  i == (-99) then do
        -- recordFailEval a
        v <- choose3 (low,high)
        return (ECon . ConstantInt $ v, v)
    else
      if i > high then do
        moda <- choose3 (high - 1, high - 1  + ( high - low ))
        let e = ECon . ConstantInt $ moda
        return ([essencee| &a - &e |],  i - moda )
      else if i < low  then do
        moda <- choose3 (low - i,  low - i  +  ( high - low ) )
        let e = ECon . ConstantInt $ moda
        return ([essencee| &a + &e |], i + moda)
      else
        return (a, i)


instance EvalToInt Expression where
  evalToInt x = case instantiateExpression [] x of
    Right (ConstantInt v) -> return v

    -- FIXME terrible hacks
    Right (ConstantUndefined _ TypeInt)     -> return (-99)
    Right (ConstantUndefined _ TypeAny)     -> return (-99)
    Left msg | "N/A:" `isPrefixOf` (dropWhile (==' ') $ show msg) -> return (-99)

    Right v -> docError ["Not an int in EvalToInt Expression"
                        , "exprI :"  <+> pretty x
                        , "result:"  <+> pretty v
                        , "resultGroomed:"  <+> (pretty . groom) v
                        , "exprGroomed:"    <+> (pretty . groom) x
                        ]
    -- FIXME errors in instantiateExpression happen too often
    _  -> return (-99)
    -- Left msg -> docError ["instantiateExpression bug in EvalToInt Expression"
    --                      , "expr:" <+> pretty x
    --                      , "msg : " <+> pretty msg
    --                      , "exprGroomed:" <+> pretty (groom x)
    --                      ]

  ensureGTE0 x = do
    i <- evalToInt x
    if  | i == (-99) -> do
          j <- choose3 (0,5)
          return . Constant . ConstantInt $ max 0 j

        | i >= 0     -> return x
        | otherwise  -> do
          let c = Constant $ ConstantInt (0 - i)
          return $ x + c

  adjustInRange a low high = do
    i <- evalToInt a
    if  i == (-99) then do
        -- recordFailEval a
        v <- choose3 (low,high)
        return (Constant . ConstantInt $ v, v)
    else
      if i > high then do
        moda <- choose3 (high - 1, high - 1  + ( high - low ))
        return $ (a - (Constant . ConstantInt) moda,  i - moda )
      else if i < low  then do
        moda <- choose3 (low - i,  low - i  +  ( high - low ) )
        return $ (a + (Constant . ConstantInt) moda,  i + moda )
      else
        return (a, i)


instance EvalToInt Constant where
  evalToInt (ConstantInt x) = return  x
  evalToInt x = fail $ "Not a int" <+> pretty x

  ensureGTE0 (ConstantInt x) | x < 0 = return $ ConstantInt 0
  ensureGTE0 c@ConstantInt{} = return c
  ensureGTE0 x = fail $ "Not a int" <+> pretty x

  adjustInRange c@(ConstantInt a) low high = do
    if a > high || a < low then do
      v <- choose3 (low,high)
      return (ConstantInt $ v, v)
    else
      return (c, a)

  adjustInRange x low high = fail $ "Not a int" <+> pretty x
                                 <+> nn "low"  low
                                 <+> nn "high" high
