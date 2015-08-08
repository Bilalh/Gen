module Gen.Essence.Carry where

import Gen.Essence.St
import Gen.Imports
import Gen.IO.RunResult


data Carry = Carry
    { cWeighting         :: KeyMap
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
    useSkipped         = return True
