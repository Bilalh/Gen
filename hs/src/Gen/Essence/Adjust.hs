module Gen.Essence.Adjust where

import Data.List          (foldl')
import Data.Map           (Map)
import Gen.Essence.Carry
import Gen.Essence.Id
import Gen.Essence.Reduce
import Gen.Essence.St
import Gen.Imports

import qualified Data.Foldable as F
import qualified Data.Map      as M


adjust :: (MonadState Carry m, MonadIO m) => ReduceResult -> m ()
adjust ReduceResult{..} = do
  let tree :: Tree Key = keyTree finalSpec
  let keys = F.toList tree
  (KeyMap ws) <- gets cWeighting
  let newWeights = foldl' doWeighting ws keys

  modify $ \st -> st{cWeighting=KeyMap newWeights}
  return ()

doWeighting :: Map Key Int -> Key -> Map Key Int
doWeighting m k =
    case k `M.member` m of
      True  ->  M.adjust decreaseWeighting k m
      False ->  M.insert k 95 m

-- | decrease 1the weighting by 5%, min 1,  n.b. scale is [0,100+]
decreaseWeighting :: Int -> Int
decreaseWeighting a = max 1 (truncate $ fromIntegral a * (0.95 :: Double))
