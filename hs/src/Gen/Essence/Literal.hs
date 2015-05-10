{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Literal where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type            ()
import Gen.Helpers.SizeOf
import Gen.Imports
import qualified Data.Set as S


instance (Generate a, WrapConstant a) => Generate (AbstractLiteral a) where
  give GNone = do
    ty <- give GOnlyLiteralTypes
    give (GType ty)

  give (GType (TypeSet ty)) = do
    es <- vectorOf3 2 (withDepthDec $ give (GType ty))
    return $ AbsLitSet es

  give (GType (TypeMatrix TypeInt ty)) = do
    es <- vectorOf3 2 (withDepthDec $ give (GType ty))

    numElems <- return 2
    numRanges <- choose3 (1 :: Integer, numElems)

    ranges <- mkRanges numElems numElems numRanges S.empty
    -- let idx = DomainInt (sortBy  rangeComp ranges)
    let idx = DomainInt ranges

    return $ AbsLitMatrix idx es

  give t = giveUnmatched "Generate (AbstractLiteral a)" t

  possiblePure _ TypeBool _  = False
  possiblePure _ TypeInt  _  = False
  possiblePure _ ty d = (depthOf ty) <= (fromIntegral d)

  possibleNoType _ _ = True


rangeComp :: Range Expr -> Range Expr -> Ordering
rangeComp (RangeSingle (ECon (ConstantInt a) ))    (RangeSingle (ECon (ConstantInt b) ))     = compare a b
rangeComp (RangeSingle (ECon (ConstantInt a) ))    (RangeBounded (ECon (ConstantInt b) ) _)  = compare a b
rangeComp (RangeBounded (ECon (ConstantInt a) ) _ ) (RangeBounded (ECon (ConstantInt b) ) _) = compare a b
rangeComp (RangeBounded (ECon (ConstantInt a) ) _)  (RangeSingle (ECon (ConstantInt b) ))    = compare a b
rangeComp a b  = docError [
    "rangeComp not matched",
    pretty $ show a, pretty $ show b,
    pretty a, pretty b
    ]


mkRanges :: forall a . WrapConstant a
         => Integer ->  Integer -> Integer -> Set Integer -> GenSt ( [Range a] )
mkRanges _ 0 0 _ = return []

mkRanges ub ns 1 used = do
    (l,u) <- chooseUnusedSized ub ns used
    return  [ RangeBounded (wrapConstant . ConstantInt $ l) (wrapConstant . ConstantInt $ u) ]

mkRanges ub ns rs used | ns == rs = do
    i <- chooseUnused ub used
    rest <- mkRanges ub (ns - 1) (rs - 1) (S.union (S.singleton i )  used)
    return $ RangeSingle (wrapConstant . ConstantInt $ i) : rest

mkRanges _ 1 rs _ | rs /= 1 = do
     error . show $ ("mkRanges invaild" :: String, 1 :: Integer, rs)

mkRanges ub ns rs used | ns >=2= do
    single :: Bool <- elements3 [True,False]
    if single then do
        i <- chooseUnused ub used
        rest <- mkRanges ub (ns - 1) (rs - 1) (S.union (S.singleton i )  used)
        return $ RangeSingle (wrapConstant . ConstantInt $ i) : rest
    else do
        num <- choose3 (2, ns - rs + 1 )
        (l,u) <- chooseUnusedSized ub num used

        let used' = S.fromList [l..u]  `S.union` used
        rest <- mkRanges ub (ns - (u - l + 1) ) (rs - 1) used'

        return $ RangeBounded (wrapConstant . ConstantInt $ l) (wrapConstant . ConstantInt $ u) : rest

mkRanges _ ns rs _  = docError ["mkRanges unmatched", pretty ns, pretty rs]

chooseUnusedSized ::  Integer -> Integer -> Set Integer -> GenSt (Integer, Integer)
chooseUnusedSized ub size used | S.null used  =  do
    lower <- choose3 (-ub*3,ub*3 - size)
    return (lower, lower + size - 1)

chooseUnusedSized ub size used =  do
    lower <- chooseUnused' (-ub*3,ub*3 - size) used
    let upper = lower + (size - 1)
    if  S.fromList [lower..upper] `S.isSubsetOf` used then
        chooseUnusedSized ub size used
    else
        return (lower,upper)


chooseUnused :: Integer -> Set Integer -> GenSt Integer
chooseUnused ub = chooseUnused' (-ub*3, ub*3)

chooseUnused' :: (Integer,Integer) -> Set Integer -> GenSt Integer
chooseUnused' (l,u) used | S.null used  = choose3 (l,u)
chooseUnused' (l,u) used = do
    i <- choose3 (l,u)
    if i `S.member` used then
        chooseUnused' (l,u) used
    else
        return i
