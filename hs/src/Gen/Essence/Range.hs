{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Range(Generate(..), mkRanges, rangeComp, chooseInt,intLowerBounded) where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Imports

import qualified Data.Set as S


instance (Generate a, WrapConstant a) => Generate (Range a) where
  give GNone = do
      parts <- getWeights [("RangeSingle", single),("RangeBounded", bounded) ]
      frequency3 parts

    where
      single  = do
        a <- chooseInt
        return $ RangeSingle (wrapConstant . ConstantInt $ a)
      bounded = do
        a <- chooseInt
        b <- intLowerBounded a
        return $ RangeBounded (wrapConstant . ConstantInt $ a)
                              (wrapConstant . ConstantInt $ b)

  give t = giveUnmatched "Generate (Range a)" t

  possiblePure _ _ _ = True
  possibleNoType _ _ = True


chooseInt :: GenSt Integer
chooseInt = do
  defs <- return [ (fromString  $ "Int_" ++ show i, return i )  | i <- [0..5] ]
  choices <- getWeights defs
  frequency3 choices

intLowerBounded :: Integer -> GenSt Integer
intLowerBounded  a = do
  defs <- return [ (fromString  $ "Int_" ++ show i, return i )  | i <- [a..5] ]
  choices <- getWeights defs
  frequency3 choices


mkRanges :: forall a . WrapConstant a
         => Integer ->  Integer -> Integer -> Set Integer
         -> GenSt [Range a]
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

    return $ RangeBounded (wrapConstant . ConstantInt $ l)
                          (wrapConstant . ConstantInt $ u) : rest

mkRanges _ ns rs _  = docError ["mkRanges unmatched", pretty ns, pretty rs]

chooseUnusedSized ::  Integer -> Integer -> Set Integer -> GenSt (Integer, Integer)
chooseUnusedSized ub size used | S.null used  =  do
  -- lower <- choose3 (-ub*3,ub*3 - size)
  lower <- choose3 (0,ub*3 - size)
  return (lower, lower + size - 1)

chooseUnusedSized ub size used =  do
  -- lower <- chooseUnused' (-ub*3,ub*3 - size) used
  lower <- chooseUnused' (0,ub*3 - size) used
  let upper = lower + (size - 1)
  if  S.fromList [lower..upper] `S.isSubsetOf` used then
      chooseUnusedSized ub size used
  else
      return (lower,upper)


chooseUnused :: Integer -> Set Integer -> GenSt Integer
chooseUnused ub = chooseUnused' (0, ub*3)

chooseUnused' :: (Integer,Integer) -> Set Integer -> GenSt Integer
chooseUnused' (l,u) used | S.null used  = choose3 (l,u)
chooseUnused' (l,u) used = do
  i <- choose3 (l,u)
  if i `S.member` used then
      chooseUnused' (l,u) used
  else
      return i


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
