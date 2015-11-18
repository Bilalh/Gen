{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.Results where

import Conjure.Language.Definition
import Data.Csv                         (FromNamedRecord,ToNamedRecord,DefaultOrdered,decodeByName,encodeDefaultOrderedByName)
import Data.List                        (break)
import Data.Map                         (Map)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField ()
import Database.SQLite.Simple.FromRow   ()
import Gen.Helpers.Str
import Gen.Imports                      hiding (group)
import Gen.Instance.Data
import Gen.Instance.Point
import Gen.Instance.RaceRunner          (conjureCompact, runSolve, script_lookup1)
import Gen.IO.Formats                   (readFromJSON)
import Gen.IO.Toolchain                 (runCommand')
import System.Exit                      (ExitCode (ExitSuccess))

import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified Gen.Instance.SettingsIn  as IN
import qualified Gen.Instance.SettingsOut as OUT
import qualified Gen.Instance.Versions    as S
import qualified Gen.Instance.ModelRow    as MR
import qualified Gen.Instance.ModelInfo   as MI

numSetsQuery, numModelsQuery, selectorQuery, compactQuery, modelsQuery :: Query

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

compactQuery = "Select eprime, eprimeId from Eprimes where isCompact = 1"

modelsQuery = [str|
  Select
   PQ.ordering as ParamId, eprimeId, eprime, nullif(SavileRow, -1) as SavileRow,
   nullif(Minion, -1) as Minion, nullif(TotalTime, -1) as TotalTime,
   nullif(MinionNodes, -1) as MinionNodes, nullif(MinionTimeout, -1) as MinionTimeout,
   nullif(MinionSolutionsFound, -1) as MinionSolutionsFound, MinionSatisfiable,
   IsOptimum, isDominated, solutionValue, minimising,
   PQ.quality as ParamQuality, TD.paramHash as PatamHash
  From TimingsDomination TD
  Join paramQuality PQ
  ON   TD.paramHash = PQ.paramHash
  |]


showResults :: (MonadIO m)
            => FilePath ->m ()
showResults outdir = do
  let db = outdir </> "results.db"
  let summary = outdir </> "summary"
  liftIO $ createDirectoryIfMissing True summary

  (numSets,numModels,groups,comp,mrows) <- liftIO $ withConnection db $ \conn -> do
    [Only numSets]    <- query_ conn numSetsQuery
    [Only numModels]  <- query_ conn numModelsQuery
    groups   :: [Only Text] <- query_ conn selectorQuery
    compacts :: [(Text,Int)]  <- query_ conn compactQuery
    mrows :: [ MR.ModelRow ] <- query_ conn modelsQuery
    return (numSets,numModels,groups,compacts,mrows)

  let ints :: [[Integer]] = [ mapMaybe (readMay . textToString) $ T.split (== ',') g
                            | Only g <- groups ]
  let sets = [ ConstantAbstract . AbsLitSet $ map (ConstantInt) is  | is <- ints ]

  let param = Point [("numSets", ConstantInt numSets)
                    ,("sets",  (ConstantAbstract $ AbsLitMSet sets ) )
                    ,("numModels", ConstantInt numModels)]


  liftIO $ writeFile (summary </> "meta" ) $ show $ vcat [
        "Compact is " <+> hcat (punctuate ", " [ pretty num | (_,num) <- comp  ])
      , ""
      ]

  liftIO $ writeFile (summary </> "meta2" ) $ show $ vcat [
        "Compact is " <+> hcat (punctuate ", " [ pretty name | (name,_) <- comp  ])
      , ""
      ]

  (fracs,hOrder) <- hittingSet summary param >>= \case
    Just (Point [(_,ConstantAbstract (AbsLitSet []))]) -> do
      liftIO $ writeFile (summary </> "hittingSet" ) "NOTHING\n"
      liftIO $ writeFile (summary </> "resultSet" ) "NOTHING\n"
      liftIO $ writeFile (summary </> "resultSet2" ) "NOTHING\n"
      return ([], Nothing)
    Just (hset@(Point [(_,hval)])) -> do
      liftIO $ print . pretty $ hset
      liftIO $ writeFile (summary </> "hittingSet" ) (renderSized 10000 $  pretty hval)
      findResultingSet db summary hval
    _ -> do
      docError [nn  "hittingSet failed on" param]

  meta ::  RunMetadata <- liftIO $ readFromJSON (outdir </> "metadata.json")
  vs <- liftIO $ V.toList <$> decodeCSV (outdir </> "settings.csv")
  vv <- liftIO $ V.toList <$> decodeCSV (outdir </> "version-run.csv")
  let hostType = S.host_type $ vv `at` 0

  let fracs_size = pa $ map ( show . length ) fracs
  let fracs_str  = pa $ map ( pa . map show ) fracs
  let compact_str  = pa $ map ( show . snd ) comp

  let compactWon = case comp of
        [(_, num)] -> case fracs of
                        [[x]] -> if x == num then 1 else 0
                        _     -> 0
        _          ->  0

  let vs_head  = headNote "setting.csv should have one row" vs

  let vs2_head = processSettings meta (length fracs)
              fracs_size fracs_str compact_str compactWon hOrder hostType vs_head
  liftIO $ encodeCSV (summary </> "summary.csv") [vs2_head]

  let minfos = map (processModelsInfo meta vs_head  ) mrows
  liftIO $ encodeCSV (summary </> "models.csv") minfos

  return ()

  where
    pa m =  "[" ++ (intercalate ", " m) ++ "]"

    processSettings meta numFracs fracs_size fracs_str compact_str compactWon hOrder hostType lin  =
      let ho = fromMaybe (rIterationsDone meta) hOrder
          (he, clas) =
            case splitOn "@" (IN.essence_name lin) of
              [h,c] -> (Just h,c)
              _         -> (Nothing, IN.essence_name lin)


      in  inToOut meta lin clas he numFracs fracs_size fracs_str compact_str compactWon ho hostType

    processModelsInfo meta lin mrow  =
      let (he, clas) =
            case splitOn "@" (IN.essence_name lin) of
              [h,c] -> (Just h,c)
              _         -> (Nothing, IN.essence_name lin)


      in minToOut meta lin mrow clas he


findResultingSet :: MonadIO m => FilePath -> FilePath -> Constant -> m ([[Int]], Maybe Int)
findResultingSet db out hval = do
  script <- liftIO $ script_lookup1 "instances/results/gent_idea.py"
  runCommand' Nothing (script) [db,  (renderSized 10000 $  pretty hval) ]
    (Just (out </> "resultSet")) >>= \case
      ExitSuccess -> do
        st <- liftIO $ readFile (out </> "resultSet")
        liftIO $ putStrLn st
        let (fLines,end) = break ( (==) 'h'  . head ) (lines st)
        let hOrder = fromJustNote "findResultingSet" .
                     readMay .  tail . dropWhile (/=' ') . head $ end

        let fIds :: [[Int]] = [ fromJustNote "findResultingSet readIds"
                              . readMay. tail . dropWhile (/= ' ') $ ids
                              | ids <- fLines ]

        mapping :: Map Int  String  <- liftIO $ liftM M.fromList
               <$> withConnection db $ \conn -> do
          query_ conn "Select eprimeId, eprime from Eprimes"
        let errX = fromJustNote "Missing Id"
        let mapped = [ [ errX $ f  `M.lookup` mapping | f <- ids ]  |  ids <- fIds ]
        let res = [ (show $ length m) ++ " "  ++ "[" ++ (intercalate ", " m) ++ "]"
                  | m  <- mapped  ]

        liftIO $ writeFile (out </> "resultSet2") $ unlines (res ++ end)
        return $ (fIds, Just hOrder)

      x -> docError [ "findResultingSet failed with"  <+> (pretty . show $ x)
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


decodeCSV :: FromNamedRecord a => FilePath -> IO (V.Vector a)
decodeCSV fp = do
    csvData <- BL.readFile fp
    case decodeByName csvData of
        Left err -> error  err
        Right (_, v) -> V.forM v $ \(p :: a) -> return p


encodeCSV :: (ToNamedRecord a, DefaultOrdered a) => FilePath -> [a] -> IO ()
encodeCSV fp cs = do
  BL.writeFile fp $ encodeDefaultOrderedByName cs



inToOut :: RunMetadata -> IN.CSV_IN
        -> String -> Maybe String -> Int -> String -> String -> String -> Int -> Int -> String
        -> OUT.CSV_OUT
inToOut RunMetadata{..} IN.CSV_IN{..}
        essenceClass heuristic numFractures fracturesSize fractures compact compactWon
        highestOrderingNeeded hostType
      = OUT.CSV_OUT{..}

minToOut :: RunMetadata -> IN.CSV_IN -> MR.ModelRow
        -> String -> Maybe String
        -> MI.ModelInfo
minToOut RunMetadata{..} IN.CSV_IN{..} MR.ModelRow{..} essenceClass heuristic
      =  MI.ModelInfo{..}

_ex1, _ex2 :: IO ()
_ex1 = showResults "/Users/bilalh/Desktop/Results/sampling_no_large/babbage/results/sdf@prob034-warehouse/nsample/sample-64_rndsols%1%16035"
_ex2 = showResults "/Users/bilalh/Desktop/Results/sampling_no_large/babbage/results/prob030-BACP/nsample/sample-64_rndsols%1%16005/"
