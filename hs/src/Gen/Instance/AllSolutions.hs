module Gen.Instance.AllSolutions where

import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.SamplingError
import System.IO                  (readFile)
import Data.List(dropWhileEnd)
import Text.Printf

data Solutions     = Solutions Int [SolutionCount]
  deriving (Eq, Show)
data SolutionCount = SolCount Int String
  deriving (Eq, Show)

readSolutionCounts :: MonadIO m => FilePath -> m Solutions
readSolutionCounts fp = do
  inn <- liftIO $ readFile fp
  let parts = [ words . (dropWhile isSpace) . (dropWhileEnd  isSpace) $ li
              | li <- lines inn ]
  let (sols,total) = (flip runState) (0 :: Int) $ forM parts $ \ma ->do
        let (ival, name) = m2t ma
        let val :: Int = fromMaybe (error $ "int reading:" ++ $line) $ readMay  ival
        cur <- get
        put (cur + val)
        return $ SolCount cur name
  let (SolCount _ lname) = last sols

  return $ Solutions total (sols ++ [(SolCount total lname)])

  where m2t [a,b] =(a,b)
        m2t x  = error $ "Not an tuple readSolutionCounts: " ++ show x

randomPointFromAllSolutions
    :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
    => m (Either SamplingErr Point)
randomPointFromAllSolutions = do
  (Solutions total sols) <- readSolutionCounts "/Users/bilalh/Desktop/all_solutions/all_sols/solutions.counts"

  -- The nth solution (indexed from 1)
  solNum <- liftIO $ randomRIO (1,total)
  -- let solNum = 656590 + (1000000)
  -- let solNum = 15005497

  let (SolCount lix solBase) = last $ filter
                                  (\(SolCount ix _) -> solNum > ix) sols
  let lineIdx = solNum - lix

  -- since there are 1000000 lines in each file
  let fileIdx = (lineIdx -1) `div` 1000000
  let fileLineIdx = lineIdx `mod` 1000000

  let solName :: String = printf "%s.%010d" solBase fileIdx

  logDebug2 $line [ nn "solNum" solNum
                  , nn "solBase" (show solBase)
                  , nn "solNameIdx" lix
                  , nn "lineIdx" lineIdx
                  , nn "fileIdx" fileIdx
                  , nn "fileLineIdx" fileLineIdx
                  , nn "solName" solName
                  ]
  return $ Right $ Point []
