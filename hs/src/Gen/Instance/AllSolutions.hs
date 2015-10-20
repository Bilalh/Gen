module Gen.Instance.AllSolutions where

import Gen.Imports
import Gen.Instance.SamplingError
import System.IO                  (readFile)
import Data.List(dropWhileEnd)

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

-- randomPointFromAllSolutions :: (Sampling a, MonadState (Method a) m, MonadIO m, MonadLog m)
--                             => m (Either SamplingErr Point)
-- randomPointFromAllSolutions = do
--     chosen <- liftIO $ randomRIO (a,b)
