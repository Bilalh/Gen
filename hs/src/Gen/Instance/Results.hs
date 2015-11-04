{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.Results where

import Conjure.Language.Definition
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField ()
import Database.SQLite.Simple.FromRow   ()
import Gen.Helpers.Str
import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.RaceRunner          (conjureCompact, runSolve, script_lookup1)
import Gen.IO.Toolchain                 (runCommand')
import System.Exit                      (ExitCode (ExitSuccess))

import qualified Data.Text as T

smallestQuery, numSetsQuery, numModelsQuery, selectorQuery :: Query

smallestQuery =[str|
  Select DISTINCT eprimes From  ParamsData
  Where eprimesLeft = (Select min(eprimesLeft) From ParamsData);
  |]

numSetsQuery = "Select distinct count(*) From ParamQuality"
numModelsQuery = "Select count(eprime) From Eprimes;"
selectorQuery = [str|
    Select group_concat(D.eprimeId, ", ") as eprimesIds

        From ParamQuality P
        Join TimingsRecorded D on P.paramHash = D.paramHash

        Where D.isDominated = 0
        Group by P.paramHash
        Having P.quality < 1
        Order by P.quality;
  |]

showResults :: (MonadIO m)
            => FilePath ->m ()
showResults outdir = do
  let db = outdir </> "results.db"
  let summary = outdir </> "summary"


  (numSets,numModels,groups) <- liftIO $ withConnection db $ \conn -> do
    [Only numSets]    <- query_ conn numSetsQuery
    [Only numModels]  <- query_ conn numModelsQuery
    groups :: [Only Text] <- query_ conn selectorQuery

    -- rows :: [Only String] <- liftIO $ query_ conn smallestQuery
    -- let smallest =  [ s | Only s <- rows  ]
    -- liftIO $ print smallest
    return (numSets,numModels,groups)

  let ints :: [[Integer]] = [ mapMaybe (readMay . textToString) $ T.split (== ',') g
                            | Only g <- groups ]
  let sets = [ ConstantAbstract . AbsLitSet $ map (ConstantInt) is  | is <- ints ]

  let param = Point [("numSets", ConstantInt numSets)
                    ,("sets",  (ConstantAbstract $ AbsLitMSet sets ) )
                    ,("numModels", ConstantInt numModels)]


  hittingSet summary param >>= \case
    Just (Point [(_,ConstantAbstract (AbsLitSet []))]) -> do
      liftIO $ writeFile (summary </> "hittingSet" ) "NOTHING"
      liftIO $ writeFile (summary </> "resultSet" ) "NOTHING"
    Just (hset@(Point [(_,hval)])) -> do
      liftIO $ print . pretty $ hset
      liftIO $ writeFile (summary </> "hittingSet" ) (renderSized 10000 $  pretty hval)
      findResultingSet db summary hval
    _ -> do
      docError [nn  "hittingSet failed on" param]

findResultingSet :: MonadIO m => FilePath -> FilePath -> Constant -> m ()
findResultingSet db out hval = do
  script <- liftIO $ script_lookup1 "instances/results/gent_idea.py"
  runCommand' Nothing (script) [db,  (renderSized 10000 $  pretty hval) ]
    (Just (out </> "resultSet")) >>= \case
      ExitSuccess ->  liftIO $ readFile (out </> "resultSet") >>= putStrLn
      x           -> docError [ "findResultingSet failed with"  <+> (pretty . show $ x)
                              , nn "on" hval]



hittingSet :: MonadIO m => FilePath -> Point -> m (Maybe Point)
hittingSet out point = do
  liftIO $ createDirectoryIfMissing True (out </> "_run_solve")

  ess <- liftIO $ script_lookup1 "instances/results/hittingSetMsetOpt.essence"
  let eprime = out </> "_run_solve" </> "hittingSet.eprime"
  conjureCompact ess eprime >>= \case
    False -> docError [nn "failed to refine" (groom ess) ]
    True  -> do
      runSolve out ess eprime point >>= \case
        Left x  -> error . show $ x
        Right (res, _) -> return res

_ex1 :: IO ()
_ex1 = showResults "/Users/bilalh/Desktop/fractest/"

_ex2 :: IO ()
_ex2 = showResults "/Users/bilalh/Desktop/fractest/org"
