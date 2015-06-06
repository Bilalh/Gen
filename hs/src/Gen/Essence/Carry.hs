module Gen.Essence.Carry where

import Gen.Essence.St
import Gen.Imports
import Gen.IO.RunResult

import qualified Data.IntSet as I

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
