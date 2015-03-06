 {-# LANGUAGE QuasiQuotes #-}
module Gen.Arbitrary.Literal where
import Conjure.Language.Constant
import Conjure.Language.AbstractLiteral
import Conjure.Language.Domain
import Data.IntSet               (IntSet)
import Gen.Arbitrary.Domain
import Gen.Arbitrary.Expr        (deAny, exprOf)
import Gen.Prelude

import qualified Data.IntSet               as I
import qualified Data.Set                  as S

boolLit :: GG Expr
boolLit = do
    b <- lift arbitrary
    return  (ECon (ConstantBool b))

intLit :: GG Expr
intLit  = do
    i <- choose2 ((-10),10 :: Integer)
    return (ECon (ConstantInt i) )


setLit :: GG Expr
setLit = do
    innerType <- withDepthDec atype
    withDepthDec (setLitOf innerType)

setLitOf :: TType ->  GG Expr
setLitOf innerType = do
    depth_ <- gets depth_
    t2 <- deAny $ TSet innerType
    listOfBounds (0,  min 15 (2 * depth_) ) (withDepthDec $ exprOf innerType) >>= \case
                     [] -> return $ ETyped t2 (ELit $ AbsLitSet [])
                     xs -> return . ELit . AbsLitSet $ xs


msetLit :: GG Expr
msetLit = do
    innerType <-  withDepthDec atype
    withDepthDec (msetLitOf innerType)

msetLitOf :: TType ->  GG Expr
msetLitOf innerType = do
    depth_ <- gets depth_
    t2 <- deAny $ TMSet innerType
    listOfBounds (0,  min 15 (2 * depth_) ) (withDepthDec $ exprOf innerType) >>= \case
                     [] -> return $ ETyped t2 (ELit $ AbsLitMSet [])
                     xs -> return . ELit . AbsLitMSet  $ xs


matrixLitOf :: TType -> GG Expr
matrixLitOf innerType = do
    idx <-  withDepthDec intDom
    let numElems = S.size $ S.fromList $ concat $ ints idx
    exprs <- vectorOf2 numElems ( withDepthDec $ exprOf innerType)
    return $ ELit $ AbsLitMatrix idx exprs

    where
      ints (DomainInt rs)         = map rsInts rs
      ints x = docError ["not matched ints", pretty x]
      rsInts (RangeSingle x)      = [getInt x]
      rsInts (RangeBounded a b)   = [(getInt a) .. (getInt b)]
      rsInts x = docError ["not matched rsInts", pretty x]
      getInt (ECon (ConstantInt x)) = x
      getInt x = docError ["not matched getInt", pretty x]

-- FIXME from mappings should be distinct?
funcLitOf :: TType -> TType -> GG Expr
funcLitOf fromType toType = do
    depth_ <- gets depth_
    numElems <- choose2 (1, min 15 (2 * depth_) )
    froms <- vectorOf2 numElems  ( withDepthDec $ exprOf fromType)
    tos   <- vectorOf2 numElems  ( withDepthDec $ exprOf toType)

    t2 <- deAny $ TFunc fromType toType
    case zipWith (\a b -> (a, b) ) froms tos of
      [] -> return $ ETyped t2 $ (ELit $ AbsLitFunction [])
      xs -> return $ ELit $ AbsLitFunction xs


tupleLitOf :: [TType] -> GG Expr
tupleLitOf tys = do
    depth_ <- gets depth_
    if
        | depth_ < 1 -> ggError "tupleLitOf depth_ <1" [pretty $ groom tys]
        | otherwise -> do
            parts <- mapM mkParts tys
            return $ ELit $ AbsLitTuple parts

    where
        mkParts ty  = do
            e <- withDepthDec $ exprOf ty
            return $ e

relLitOf :: [TType] -> GG Expr
relLitOf types = do
    depth_ <- gets depth_
    if
        | depth_ < 2 -> ggError "relLitOf depth_ <2" [pretty $ groom types]
        | otherwise -> do
            parts <- vectorOf2 3 $ mkParts types
            t2 <- deAny $ TRel types
            case parts of
              [] -> return $ ETyped t2  (ELit $ AbsLitRelation [])
              xs -> return $ ELit $ AbsLitRelation xs

    where
    -- FIXME CHECK depth
    mkParts tys = mapM exprOf tys



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
                    [] -> return $ ETyped t2 (ELit $ AbsLitPartition [])
                    xs -> return $ ELit $ AbsLitPartition xs

    where

        mkParts
            :: Int  -- Number of elements to put in all parts
            -> Int  -- Number of parts
            -> IntSet
            -> GG [[Expr]]
        mkParts e 1 done = (\f -> [fst f]) <$> mkPart e done
        mkParts e p done = do
            numElems <- choose2 (1, (e - (p - 1) ))

            (part, done') <- mkPart numElems done
            parts <- (mkParts (e - numElems) (p-1) done')
            return $ part : parts

        mkPart
            :: Int         -- Number of elements
            -> IntSet
            -> GG ( [Expr], IntSet)
        mkPart e done = do
            (es,done') <- expr e done
            return $ (es, done')

            where
            expr 0 seen = return ([], seen)
            expr g seen = do
                e1 <- withDepthDec $ exprOf innerType
                if
                    -- | (hash e1) `I.member` seen -> expr g seen
                    | otherwise          -> do
                        (es,seen') <- expr (g - 1) ((hash e1) `I.insert` seen)
                        return $ (e1 : es, seen' )
