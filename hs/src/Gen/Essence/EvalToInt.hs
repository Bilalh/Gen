{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.EvalToInt where

import Conjure.Language.Definition
import Gen.Essence.Type        ()
import Gen.Imports
import Conjure.Language.Instantiate
import Conjure.Language.TH
import Gen.AST.TH

class EvalToInt a where
  evalToInt  :: MonadFail m => a -> m Integer
  ensureGTE0 :: MonadFail m => a -> m a


instance EvalToInt Expr where
  evalToInt x = do
    cx :: Expression <- toConjure x
    evalToInt cx

  ensureGTE0 x = do
    i <- evalToInt x
    if i >= 0 then
        return x
    else do
        let c = ECon $ ConstantInt (0 - i)
        return [essencee| &x + &c |]


instance EvalToInt Expression where
  evalToInt x = case instantiateExpression [] x of
    Right (ConstantInt v) -> return v
    Right v -> docError ["Not an int in EvalToInt Expression"
                        , "exprInt:"  <+> pretty x
                        , "result :"  <+> pretty v
                        ]

    Left msg -> docError ["instantiateExpression bug in EvalToInt Expression"
                         , "expr:" <+> pretty x
                         , "msg: " <+> pretty msg
                         ]
  ensureGTE0 x = do
    i <- evalToInt x
    if i >= 0 then
        return x
    else do
        let c = Constant $ ConstantInt (0 - i)
        return [essence| &x + &c |]


instance EvalToInt Constant where
  evalToInt (ConstantInt x) = return  x
  evalToInt x = fail $ "Not a int" <+> pretty x

  ensureGTE0 (ConstantInt x) | x < 0 = return $ ConstantInt 0
  ensureGTE0 c@ConstantInt{} = return c
  ensureGTE0 x = fail $ "Not a int" <+> pretty x
