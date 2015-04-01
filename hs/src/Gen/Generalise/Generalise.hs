module Gen.Generalise.Generalise where

import Gen.Generalise.Data
import Gen.Prelude


generaliseMain :: GState -> IO GState
generaliseMain startState = do
  $notDone
