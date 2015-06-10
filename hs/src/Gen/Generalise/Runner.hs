module Gen.Generalise.Runner where

import Gen.Generalise.Data
import Gen.Imports
import Gen.IO.RunResult
import Gen.Reduce.Runner

runSpec2 :: Spec -> EE (Maybe ErrData)
runSpec2 sp = do
  fst <$> runSpec sp
