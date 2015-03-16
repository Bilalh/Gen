{-# LANGUAGE QuasiQuotes #-}
module Gen.Arbitrary.Op(
    Bop
  , Uop
  , bop
  , opOf
  , bopOf
  , equivExpr
  , arithmeticExprOf
  , relationExpr
  , boolOpFor
  ) where

import Conjure.Language.Expression.Op
import Gen.Arbitrary.Expr
import Gen.AST.TH
import Gen.AST.Ops
import Gen.Prelude


type Bop = (Expr -> Expr -> Expr)
type Uop = (Expr -> Expr)

bop :: Bop ->  GG Expr
bop op = do
    depth_ <- gets depth_
    addLog "bop" ["depth_" <+> pretty depth_ ]

    if
        | depth_ < 1 -> ggError "bop depth_ < 1" [ ]
        | otherwise -> do
            exprType <-  withDepthDec atype
            addLog "bop" ["bop ty" <+> pretty exprType ]
            e1 <- withDepthDec (exprOf exprType)
            e2 <- withDepthDec (exprOf exprType)

            return $ op e1 e2


bopOf :: Bop -> TType -> GG Expr
bopOf op exprType = do

    depth_ <- gets depth_
    addLog "bopOf" []
    addLog "bopOf" ["depth_" <+> pretty depth_, "ty" <+> pretty exprType ]

    if
        | depth_ < 1 -> ggError "bopOf depth_ < 1" [pretty exprType]
        | otherwise -> do
            e1 <- withDepthDec (exprOf exprType)
            e2 <- withDepthDec (exprOf exprType)

            return $ op e1 e2


opOf :: Uop -> TType ->  GG Expr
opOf op exprType =  do
    depth_ <- gets depth_
    addLog "opOf" ["depth_" <+> pretty depth_, "ty" <+> pretty exprType ]

    if
        | depth_ < 1 -> ggError "opOf depth_ < 1" [ pretty . groom $ exprType ]
        | otherwise -> do
            e1 <- withDepthDec (exprOf exprType)
            return $ op e1


equivExpr :: GG Expr
equivExpr = oneof2 $ map bop [ opEq, opNeq ]

arithmeticTypes :: GG TType
arithmeticTypes  = return TInt

arithmeticExpr :: GG Expr
arithmeticExpr = do
  kind <- arithmeticTypes
  arithmeticExprOf kind

arithmeticExprOf :: TType ->  GG Expr
arithmeticExprOf kind = do
  oneof2 $ map (flip (bopOf) kind ) [opPlus, opMult, opDiv, opMod]


relationExpr :: GG Expr
relationExpr =  do
  oneof2 $ map (`bopOf` TBool ) [opOr, opAnd, opImply, opIff]

comparisonExpr :: GG Expr
comparisonExpr =  do
  oneof2 $ map (`bopOf` TBool ) [opLt, opLeq, opGt, opGeq]


boolOpFor :: TType -> GG (Expr -> Expr -> Expr)
boolOpFor TBool = do
  elements2 [ opEq, opNeq, opOr, opAnd, opImply, opIff ]

boolOpFor TInt = do
  elements2 [ opEq, opNeq, opLt, opLeq, opGt, opGeq ]

boolOpFor (TSet _) =  do
  elements2 [ opEq, opNeq, opSubset, opSubsetEq, opSupset, opSupsetEq ]

boolOpFor (TMSet _) =  do
  elements2 [ opEq, opNeq, opLt, opSubset, opSubsetEq, opSupset, opSupsetEq ]

boolOpFor (TMatix _) = do
  elements2 [ opEq, opNeq ]

boolOpFor (TTuple _) = do
  elements2 [ opEq, opNeq ]

boolOpFor (TRel _) = do
  elements2 [ opEq, opNeq ]

boolOpFor (TFunc _ _) = do
  elements2 [ opEq, opNeq ]

boolOpFor (TPar _) = do
  elements2 [ opEq, opNeq ]

boolOpFor t = ggError "boolOpFor" [pretty t]
