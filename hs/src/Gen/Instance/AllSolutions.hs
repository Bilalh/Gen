module Gen.Instance.AllSolutions where

import Conjure.Language.Domain
import Data.List               (dropWhileEnd)
import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Point
import Gen.Instance.RaceRunner (createParamEssence1, runPadded, script_lookup,
                                script_lookup1)
import System.Directory        (copyFile)
import System.FilePath         (takeBaseName)
import Text.Printf

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
    => m Point
randomPointFromAllSolutions = do
  (Method MCommon{mOutputDir,mPreGenerate=Just (preBase,(Solutions total sols))} _) <- get

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
  let paramBase = takeBaseName solBase

  logDebugVerbose2 $line [ nn "solNum" solNum
                         , nn "solBase" (show solBase)
                         , nn "solNameIdx" lix
                         , nn "lineIdx" lineIdx
                         , nn "fileIdx" fileIdx
                         , nn "fileLineIdx" fileLineIdx
                         , nn "solName" solName
                         ]

  let out = mOutputDir </> "all_sols" </> show solNum
  liftIO $ createDirectoryIfMissing True out

  let args = map stringToText [
               preBase
             , solName
             , show fileLineIdx
             , paramBase
             , show (60 :: Int)
             ]

  let env = map (second stringToText) [
              ("GENERATED_OUTPUT_DIR", out)
            , ("TIMEOUT5_FILE", out </> "timeout_file")
            ]

  -- Convert the solution back to essence
  cmd <- script_lookup "instances/all_solutions/get_solution.sh"
  void $ liftIO $ runPadded " € " env cmd args

  finds  <- readPoint (out </> paramBase <.> ".solution")
  givens <- readPoint (preBase </> "_params" </>  paramBase <.> ".param")

  return $ finds `mappend` givens

createAllSolutionScript :: (MonadIO m, MonadLog m)
                        => Provider -> VarInfo -> FilePath -> FilePath
                        -> m ()
createAllSolutionScript provider info essence out = do
  createParamEssence1 essence info out

  let param_dir =  out </> "_params"
  liftIO $ createDirectoryIfMissing True param_dir
  points <- provideAllValues provider

  let nf = nameFunc provider

  liftIO $ forM_ points $ \p -> do
    let fp = param_dir </> nf p <.> ".param"
    writePoint p fp
    base <- script_lookup1 "instances/all_solutions_run/"
    forM_ ["create_all_solutions.sh", "split_results.sh", "README.md"] $ \name -> do
      copyFile (base </> name) (out </> name)


  where
  nameFunc :: Provider -> (Point -> String)
  nameFunc (Provider xs) | all (simple . snd) xs = simpleName
  nameFunc _ = pointHash

  simple DomainInt{}  = True
  simple DomainBool{} = True
  simple _            = False

  simpleName (Point xs) =  intercalate "-" $ map (show . pretty . snd) xs
