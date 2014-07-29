{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module TestGen.Sample(sample,weightedRange) where

import TestGen.Data(MonadGen,rangeRandomG)

import Data.Foldable(toList)
import Data.Maybe(fromJust)

import qualified Data.Sequence as Seq
import qualified Data.List as L

sample :: MonadGen m => [a] -> Int -> m [a]
sample ys num | length ys == num = return ys
sample ys num =
   go 0 (l - 1) (Seq.fromList ys)
   where
   l =  length ys
   go !n !i xs | n > num =  return $!  (toList . Seq.drop (l - num)) xs
               | otherwise = do
                   j <- rangeRandomG (0, i)
                   let toI = xs `Seq.index` j
                       toJ = xs `Seq.index` i
                       next = (Seq.update i toI . Seq.update j toJ) xs
                   go (n + 1)  (i - 1) next


-- Returns a value in the range (0,n) with each number 1/2 the chance of being
-- picked as the last
weightedRange :: MonadGen m => Int -> m Int
weightedRange n | n < 0 = error  "weightedRange less then zero"
weightedRange 0 = return 0
weightedRange n = do
    let powers = scanl1 (*) $ replicate (n+1) 2
        bounds = reverse $ 1 : powers
    r <- rangeRandomG (1, (head bounds)-1)
    let val = head $ dropWhile (>r) bounds
        index = L.elemIndex val bounds

    return $ (fromJust index) - 1


