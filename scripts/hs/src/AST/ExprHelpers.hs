{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module AST.ExprHelpers where

import AST.Data

import Language.E

import Control.Monad.State


type ExprM a = State (Text,Int) a

evalExpr :: ExprM a -> a
evalExpr op = evalState op ("ast__",0)

($+), ($-), ($=), ($/=) :: Monad m => Expr -> Expr -> m Expr
($+) x y = return . EBinOp $ BPlus x y
($-) x y = return . EBinOp . BPlus x . EUniOp $ UNeg y
($=) x y = return . EBinOp $ BEQ x y
($/=) x y = return . EBinOp $ BNEQ x y

--forAll :: Expr -> (Expr -> ExprM Expr) -> ExprM Expr
--forAll dom b = do
--  (pre,d) <- get
--  let e = EVar . T.append pre . T.pack $ show d
--  put (pre,d+1)
--  b e >>= return . EQuan ForAll e dom EEmptyGuard

--_test :: ExprM Expr
--_test = forAll (EVar "s") b where
--  b x = x $+ EVar "y"
