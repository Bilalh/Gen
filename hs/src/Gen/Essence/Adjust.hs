module Gen.Essence.Adjust where

import Data.Map           (Map)
import Gen.Essence.Id
import Gen.Essence.Reduce
import Gen.Essence.St
import Gen.Imports
import Data.List(foldl')

import qualified Data.IntSet as I
import qualified Data.Foldable as F
import qualified Data.Map as M

data Carry = Carry
    { cWeighting :: KeyMap
    , cHashes    :: I.IntSet
    } deriving (Show)


adjust :: (MonadState Carry m, MonadIO m) => ReduceResult -> m ()
adjust ReduceResult{..} = do
  let tree :: Tree Key = keyTree finalSpec
  let keys = F.toList tree
  ws <- gets cWeighting
  let newWeights = foldl' doWeighting ws keys

  modify $ \st -> st{cWeighting=newWeights}
  return ()

doWeighting :: Map Key Int -> Key -> Map Key Int
doWeighting m k =
    case k `M.member` m of
      True  ->  M.adjust decreaseWeighting k m
      False ->  M.insert k 95 m

-- | decreaseWeighting by 5%, min 1
-- | Note scale is [0,100+]
decreaseWeighting :: Int -> Int
decreaseWeighting a = max 0 (truncate $ fromIntegral a * (0.95 :: Double))
