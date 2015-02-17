{-# LANGUAGE QuasiQuotes,  ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module TestGen.Arbitrary.Literal where

import TestGen.Prelude

import TestGen.Arbitrary.Domain

import TestGen.Arbitrary.Expr(exprOf,deAny)

import qualified Data.IntSet as I
import Data.IntSet(IntSet)

import Conjure.Language.Domain



boolLit :: GG Expr
boolLit = do
    b <- lift arbitrary
    return  (ELit (EB b))

intLit :: GG Expr
intLit  = do
    i <- choose2 ((-10),10 :: Int)
    return (ELit (EI i) )


setLit :: GG Expr
setLit = do
    innerType <- withDepthDec atype
    withDepthDec (setLitOf innerType)

setLitOf :: TType ->  GG Expr
setLitOf innerType = do
    depth_ <- gets depth_
    t2 <- deAny $ TSet innerType
    listOfBounds (0,  min 15 (2 * depth_) ) (withDepthDec $ exprOf innerType) >>= \case
                     [] -> return $ ETyped t2 (ELit $ ESet [])
                     xs -> return . ELit . ESet . map EExpr $ xs
    --


msetLit :: GG Expr
msetLit = do
    innerType <-  withDepthDec atype
    withDepthDec (msetLitOf innerType)

msetLitOf :: TType ->  GG Expr
msetLitOf innerType = do
    depth_ <- gets depth_
    t2 <- deAny $ TMSet innerType
    listOfBounds (0,  min 15 (2 * depth_) ) (withDepthDec $ exprOf innerType) >>= \case
                     [] -> return $ ETyped t2 (ELit $ EMSet [])
                     xs -> return . ELit . EMSet . map EExpr $ xs


matrixLitOf :: TType -> GG Expr
matrixLitOf innerType = do
    idx <-  withDepthDec intDom
    let numElems = I.size $ I.fromList $ concat $ ints idx
    exprs <- vectorOf2 numElems ( withDepthDec $ exprOf innerType)
    return $ ELit $ EMatrix (map EExpr $ exprs) idx

    where
      ints (DomainInt rs)         = map rsInts rs
      ints x = docError ["not matched ints", pretty x]
      rsInts (RangeSingle x)      = [getInt x]
      rsInts (RangeBounded a b)   = [(getInt a) .. (getInt b)]
      rsInts x = docError ["not matched rsInts", pretty x]
      getInt (ELit (EI x)) = x
      getInt x = docError ["not matched getInt", pretty x]

-- FIXME from mappings should be distinct?
funcLitOf :: TType -> TType -> GG Expr
funcLitOf fromType toType = do
    depth_ <- gets depth_
    numElems <- choose2 (1, min 15 (2 * depth_) )
    froms <- vectorOf2 numElems  ( withDepthDec $ exprOf fromType)
    tos   <- vectorOf2 numElems  ( withDepthDec $ exprOf toType)

    t2 <- deAny $ TFunc fromType toType
    case zipWith (\a b -> (EExpr $ a, EExpr $ b) ) froms tos of
      [] -> return $ ETyped t2 $ (ELit $ EFunction [])
      xs -> return $ ELit $ EFunction xs


tupleLitOf :: [TType] -> GG Expr
tupleLitOf tys = do
    depth_ <- gets depth_
    if
        | depth_ < 1 -> ggError "tupleLitOf depth_ <1" [pretty $ groom tys]
        | otherwise -> do
            parts <- mapM mkParts tys
            return $ ELit $ ETuple parts

    where
        mkParts ty  = do
            e <- withDepthDec $ exprOf ty
            return $ EExpr  e

relLitOf :: [TType] -> GG Expr
relLitOf types = do
    depth_ <- gets depth_
    if
        | depth_ < 2 -> ggError "relLitOf depth_ <2" [pretty $ groom types]
        | otherwise -> do
            parts <- vectorOf2 3 $ mkParts types
            t2 <- deAny $ TRel types
            case parts of
              [] -> return $ ETyped t2  (ELit $ ERelation [])
              xs -> return $ ELit $ ERelation xs

    where
    mkParts tys = do
        (ELit lit) <- withDepthDec $ tupleLitOf tys
        return lit



parLitOf :: TType -> GG Expr
parLitOf innerType = do
    depth_ <- gets depth_

    if
        | depth_ < 1 -> ggError "parLitOf depth <1" [pretty $ groom innerType]
        | otherwise -> do

            let maxElems :: Int  = fromInteger $ sizeOfLimited 10 (TPar innerType)

            numElems <- choose2 (1,  minimum [maxElems, 15,  2 *  depth_  ] )
            numParts <- choose2 (1, numElems)
            t2 <- deAny $ TPar innerType
            mkParts numElems numParts (I.empty) >>= \case
                    [] -> return $ ETyped t2 (ELit $ EPartition [])
                    xs -> return $ ELit $ EPartition xs

    where

        mkParts
            :: Int  -- Number of elements to put in all parts
            -> Int  -- Number of parts
            -> IntSet
            -> GG [[Literal]]
        mkParts e 1 done = (\f -> [fst f]) <$> mkPart e done
        mkParts e p done = do
            numElems <- choose2 (1, (e - (p - 1) ))

            (part, done') <- mkPart numElems done
            parts <- (mkParts (e - numElems) (p-1) done')
            return $ part : parts

        mkPart
            :: Int         -- Number of elements
            -> IntSet
            -> GG ( [Literal], IntSet)
        mkPart e done = do
            (es,done') <- expr e done
            return $ (map EExpr es, done')

            where
            expr 0 seen = return ([], seen)
            expr g seen = do
                e1 <- withDepthDec $ exprOf innerType
                if
                    -- | (hash e1) `I.member` seen -> expr g seen
                    | otherwise          -> do
                        (es,seen') <- expr (g - 1) ((hash e1) `I.insert` seen)
                        return $ (e1 : es, seen' )
