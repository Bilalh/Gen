{-# LANGUAGE Rank2Types #-}
module Gen.Generalise.Runner where

import Gen.Generalise.Data
import Gen.Imports
import Gen.IO.RunResult
import Gen.Reduce.Runner
import Gen.Instance.Point

runSpec2 :: Spec -> Maybe Point ->  EEE (Maybe ErrData)
runSpec2 sp mayP = do
  fst <$> runSpec sp mayP
