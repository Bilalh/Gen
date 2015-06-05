module Gen.Essence.Adjust where

import Data.Map           (Map)
import Gen.Essence.Id
import Gen.Essence.Reduce
import Gen.Essence.St
import Gen.Imports
import Data.List(foldl')
import Gen.IO.RunResult

import qualified Data.IntSet as I
import qualified Data.Foldable as F
import qualified Data.Map as M

data Carry = Carry
    { cWeighting         :: KeyMap
    , cHashes            :: I.IntSet
    , cWeightingHashPrev :: Int
    , cDB                :: ResultsDB
    , cSpecDir           :: FilePath
    , cDBDir             :: FilePath
    } deriving (Show)

instance Monad m => MonadDB (StateT Carry m) where
    getsDb             = gets cDB
    putsDb db          = modify $ \st -> st{cDB=db}
    getDbDirectory     = gets cDBDir >>= return . Just
    getOutputDirectory = gets cSpecDir
    sortByKindStatus   = return True


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
