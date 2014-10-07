{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Arbitrary where

import AST.Imports
import TestGen.Arbitrary.Helpers
import TestGen.Arbitrary.Data
import TestGen.Arbitrary.Type

import Language.E hiding(trace)
-- import Debug.Trace(trace)

import Test.QuickCheck
-- import Control.Monad(liftM2)

import qualified Data.Text as T
import qualified Data.Map as M
import Text.Groom(groom)


instance Arbitrary SpecE where
    arbitrary = sized spec

spec :: Depth -> Gen SpecE
spec depth = do
    doms <- listOfB 1 10 (dom depth)
    let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
    let mappings  = M.fromList withNames

    let state = SS{depth_=depth, doms_=mappings, nextNum_ = length doms + 1,newVars_=[]}

    exprs <- listOfB 0 15 ( expr state)

    return $ SpecE mappings exprs

    where name i =  T.pack $  "var" ++  (show  i)


---- Domains

dom :: Depth -> Gen (Domain)
dom 0 = oneof [ intDom 0 , return DBool ]
dom n = oneof
    [
      intDom n
    , return DBool
    , setDom n
    ]

intDom :: Depth -> Gen Domain
intDom d = return DInt `ap` (listOfB 1 4 (range d))

setDom :: Depth -> Gen Domain
setDom depth = do
    inner <- dom (depth - 1)
    return $ dset{inner}


range :: Depth -> Gen (Range Expr)
range _ = oneof
    [
      arbitrarySingle
    , arbitraryFromTo
    ]

    where
    arbitrarySingle :: Gen (Range Expr)
    arbitrarySingle = do
        a <- choose ((-10),10 :: Integer)
        return $ RSingle (ELit . EI $ a)

    arbitraryFromTo :: Gen (Range Expr)
    arbitraryFromTo = do
        do
            a <- choose ((-10),10 :: Integer)
            b <- choose (a,10)
            return $ RFromTo (ELit . EI $ a) (ELit . EI $  b)


---- lits

boolLit :: SpecState -> Gen Expr
boolLit _ = do
    b <- arbitrary
    return  (ELit (EB b))

intLit :: SpecState -> Gen Expr
intLit _ = do
    i <- choose ((-10),10 :: Integer)
    return (ELit (EI i) )

--FIXME depth?
setLit :: SpecState -> Gen Expr
setLit s@SS{..} = do
    innerType <- atype s{depth_ = depth_ -1 }
    setLitOf s{depth_=depth_-1} innerType

setLitOf :: SpecState -> Type ->  Gen Expr
setLitOf s@SS{..} innerType = do
    exprs <- listOfB 0 15 ( exprOf s{depth_=depth_ - 1} innerType)
    return $ ELit $ ESet $ map EExpr $ exprs

--- Exps

expr :: SpecState -> Gen Expr
expr s@SS{..} | depth_ < 3 = boolExpr s
expr s = oneof $
    [ boolExpr s, quanExpr s]

boolExpr :: SpecState -> Gen Expr
boolExpr s@SS{..} = oneof $ case depth_ of
     0 ->  [ boolLit s ]
     1 ->  [ boolLit s, bop s BEQ ]
     2 ->  [ boolLit s, bop s BEQ ]
     _ ->  [ boolLit s, bop s BEQ, quanExpr s ]

quanExpr :: SpecState -> Gen Expr
quanExpr s@SS{..} =
    case overs of
        Nothing  -> boolExpr s  -- Nothing to quan over
        Just gen -> do
            over@(EVar overName) <- gen
            let overType = lookUpType s overName

            let (s', inName) = nextQuanVarName s
            let inType =  quanType_in overType
            let s'' = introduceVariable s' (inName, inType)

            -- Esure with high prob that inName is actually used

            bs <- boolExpr s''{depth_=depth_ - 1}
            let a = EQuan ForAll (BIn (EQVar inName) over) EEmptyGuard
                        bs
            return $ a

    where
        overs =  varOf s (TSet TAny)

lookUpType :: SpecState -> Text -> Type
lookUpType  s@SS{..} name =
    -- FIXME look in new vars
    case fmap (typeOfDom . domOfFG) $  name `M.lookup` doms_ of
        Nothing -> error . show $ vcat ["lookUpType", pretty s, pretty name]
        Just v  -> v

nextQuanVarName :: SpecState -> (SpecState, Text)
nextQuanVarName s@SS{..} =
    let varName = T.pack $ "q_" ++ show nextNum_
        s' = s{nextNum_=nextNum_ + 1}
    in  (s', varName)

introduceVariable :: SpecState -> (Text,Type) -> SpecState
introduceVariable s@SS{..} var =
    s{newVars_= newVar : newVars_ }
    where
    newVar = var

----OPS

type Bop = (Expr -> Expr -> BinOp)

bop :: SS -> Bop ->  Gen Expr
bop s@SS{..} op =  do
    -- TODO we what domain without attributes, for type checking
    exprType <- atype s{depth_=depth_ - 1}

    e1 <- exprOf s{depth_=depth_ - 1} exprType
    e2 <- exprOf s{depth_=depth_ - 1} exprType

    return $ EBinOp $  op e1 e2


bopOf :: SS -> Bop -> Type -> Gen Expr
bopOf s@SS{..} op exprType =  do
    e1 <- exprOf s{depth_=depth_ - 1} exprType
    e2 <- exprOf s{depth_=depth_ - 1} exprType

    return $ EBinOp $ op e1 e2

opOf :: SS -> (Expr -> UniOp) -> Type ->  Gen Expr
opOf s@SS{..} op exprType = do
    e1 <- exprOf s{depth_=depth_ - 1} exprType
    return $ EUniOp $ op e1

bar :: SS -> Gen Expr
bar s@SS{..} = do
    exprType <- undefined :: Gen Type
    opOf s UBar exprType

-- Return a expr of the specifed depth and type
exprOf :: SS -> Type -> Gen Expr
exprOf s@SS{depth_=0,..} d@TBool = oneof $ ofType ++
    [
      boolLit s
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@TBool = oneof $ ofType ++
    [
      boolLit s
    , bop s BEQ
    ]
    where ofType = maybeToList $ varOf s d


exprOf s@SS{depth_=0,..} d@TInt = oneof $ ofType ++
    [
      intLit s
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{depth_=1,..} d@TInt = oneof $ ofType ++
    [
      intLit s
    , bopOf s BPlus d
    ]
    where ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@TInt = oneof $ ofType ++
    [
      intLit s
    , bopOf s BPlus d
    ]
    where
    ofType = maybeToList $ varOf s d

exprOf s@SS{..} d@(TSet inner) | depth_ >=1 = oneof $ ofType ++
    [
       setLitOf s inner
    ]
    where ofType = maybeToList $ varOf s d

exprOf ss  exprDom = error . show . vcat $
    ["exprOfType not Matched", "exprDom:" <+> pretty exprDom, pretty . groom $ ss]


varOf :: SS -> Type -> Maybe (Gen Expr)
varOf SS{..} exprType = toGenExpr $ newVars ++  (map fst . M.toList  .
    M.filter (typesUnify exprType . typeOfDom . domOfFG ))  doms_

    where
    newVars :: [Text]
    newVars = map fst $ filter (typesUnify exprType . snd ) $ newVars_


toGenExpr :: [Text] -> Maybe (Gen Expr)
toGenExpr doms =  case (map (EVar) $ doms) of
    [] -> Nothing
    xs -> Just $ elements xs
