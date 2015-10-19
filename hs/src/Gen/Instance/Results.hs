{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.Results where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField ()
import Database.SQLite.Simple.FromRow   ()
import Gen.Helpers.Str
import Gen.Imports
import Gen.Instance.Data
import Gen.Instance.Point
import System.IO.Temp                   (withSystemTempDirectory)
import Conjure.Language.Definition
import Gen.Instance.RaceRunner (runSolve, script_lookup1,conjureCompact)

import qualified Data.Text as T

smallestQuery, numSetsQuery, numModelsQuery, selectorQuery :: Query

smallestQuery =[str|
  Select DISTINCT eprimes From  ParamsData
  Where eprimesLeft = (Select min(eprimesLeft) From ParamsData);
  |]

numSetsQuery = "Select distinct count(eprimes) From ParamsData Where quality < 1;"
numModelsQuery = "Select count(eprime) From Eprimes;"
-- selectorQuery = "Select distinct  eprimesIds  as A from ParamsData where quality < 1;"
selectorQuery = [str|
    Select group_concat(D.eprimeId, ", ") as eprimesIds

        From ParamQuality P
        Join TimingsRecorded D on P.paramHash = D.paramHash

        Where D.isDominated = 0
        Group by P.paramHash
        Order by P.quality;

  |]

showResults :: (MonadIO m)
            => FilePath ->m ()
showResults outdir = do
  let db = outdir </> "results.db"
  let summary = outdir </> "summary"

  conn <- liftIO $ open (db)
  -- rows :: [Only String] <- liftIO $ query_ conn smallestQuery
  -- let smallest =  [ s | Only s <- rows  ]
  -- liftIO $ print smallest

  [Only numSets]    <- liftIO $ query_ conn numSetsQuery
  [Only numModels]  <- liftIO $ query_ conn numModelsQuery

  groups :: [Only Text] <- liftIO $ query_ conn selectorQuery
  let ints :: [[Integer]] = [ mapMaybe (readMay . textToString) $ T.split (== ',') g
                            | Only g <- groups ]
  let sets = [ ConstantAbstract . AbsLitSet $ map (ConstantInt) is  | is <- ints ]

  let param = Point [("numSets", ConstantInt numSets)
                    ,("sets",  (ConstantAbstract $ AbsLitMSet sets ) )
                    ,("numModels", ConstantInt numModels)]

  results <- hittingSet summary param
  liftIO $ print . pretty $ results


hittingSet :: MonadIO m => FilePath -> Point -> m Point
hittingSet out point = do
  liftIO $ createDirectoryIfMissing True out

  ess <- liftIO $ script_lookup1 "instances/hittingSetMsetOpt/hittingSetMsetOpt.essence"
  let eprime = out </> "hittingSet.eprime"
  conjureCompact ess eprime >>= \case
    False -> docError [nn "failed to refine" (groom ess) ]
    True  -> do
      runSolve out ess eprime point >>= \case
        Left x  -> error . show $ x
        Right (Just solPoint, _) -> do
          return solPoint
        Right x -> docError ["No solution in hitting set" <+> pretty x]

_ex1 :: IO ()
_ex1 = showResults "/Users/bilalh/Desktop/fractest/"

_ex2 :: IO ()
_ex2 = showResults "/Users/bilalh/Desktop/fractest/org"
