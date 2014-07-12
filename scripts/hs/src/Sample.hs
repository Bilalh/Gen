{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Sample(sample) where

import Data
import Data.Foldable(toList)

import qualified Data.Sequence as Seq

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
