{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.Results.Results where

import Conjure.Language.Definition
import Data.Csv                         (DefaultOrdered, FromNamedRecord,
                                         ToNamedRecord, decodeByName,
                                         encodeDefaultOrderedByName)
import Data.List                        (break)
import Data.Map                         (Map)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField ()
import Database.SQLite.Simple.FromRow   ()
import Gen.Helpers.Str
import Gen.Imports                      hiding (group)
import Gen.Instance.Data
import Gen.Instance.Results.ModeMeta
import Gen.Instance.Point
import Gen.Instance.RaceRunner          (conjureCompact, runSolve, script_lookup1)
import Gen.IO.Formats                   (readFromJSON, readFromJSONMay)
import Gen.IO.Toolchain                 (runCommand')
import System.Directory                 (getHomeDirectory)
import System.Exit                      (ExitCode (ExitSuccess))
import System.IO                        (readFile)
import System.FilePath                  (takeDirectory)

import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet              as I
import qualified Data.Map                 as M
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified Gen.Instance.Results.ModelInfo   as MI
import qualified Gen.Instance.Results.ModelRow    as MR
import qualified Gen.Instance.Results.SettingsIn  as IN
import qualified Gen.Instance.Results.SettingsOut as OUT
import qualified Gen.Instance.Results.Versions    as S
import qualified Crypto.Hash as Crypto

numSetsQuery, numModelsQuery, selectorQuery, compactQuery :: Query
modelsQuery, paramsQuery :: Query

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

paramsQuery = [str|
  Select   paramHash
  From     ParamQuality
  Order by paramHash asc
  |]

showResults :: (MonadIO m)
            => FilePath ->m ()
showResults outdir = do
  let db = outdir </> "results.db"
  let summary = outdir </> "summary"
  liftIO $ createDirectoryIfMissing True summary

  (numSets,numModels,groups,comp,mrows,bps) <- liftIO $ withConnection db $ \conn -> do
    [Only numSets]   <- query_ conn numSetsQuery
    [Only numModels] <- query_ conn numModelsQuery

    groups   :: [Only Text]           <- query_ conn selectorQuery
    compacts :: [(Text,Int)]          <- query_ conn compactQuery
    mrows    :: [ MR.ModelRow ]       <- query_ conn modelsQuery
    bps      :: [ Only String ]       <- query_ conn paramsQuery
    return (numSets,numModels,groups,compacts,mrows,bps)

  let ints :: [[Integer]] = [ mapMaybe (readMay . textToString) $ T.split (== ',') g
                            | Only g <- groups ]
  let sets = [ ConstantAbstract . AbsLitSet $ map (ConstantInt) is  | is <- ints ]

  let param = Point [("numSets", ConstantInt numSets)
                    ,("sets",  (ConstantAbstract $ AbsLitMSet sets ) )
                    ,("numModels", ConstantInt numModels)]


  let paramsUsedS = intercalate "-" [ p | Only p <- bps ]
      paramsUsedHash :: Crypto.Digest Crypto.SHA512 = Crypto.hash $ C.pack $ paramsUsedS
      paramUsedHashString = show paramsUsedHash

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

  let vs2_head = processSettings meta (length fracs) fracs_size fracs_str
                   compact_str compactWon hOrder hostType paramUsedHashString vs_head
  liftIO $ encodeCSV (summary </> "summary.csv") [vs2_head]


  let winnerIds  = M.fromList $ concat $ zipWith
                     (\is ix -> zip is (repeat ix)) fracs [0 :: Int ..]
  let compactIds = I.fromList $ map snd comp

  essenceDir <- liftIO $ takeDirectory <$> getFullPath (IN.essence vs_head)
  let modeMeta =  IN.essence_name vs_head ++ "_" ++ IN.mode vs_head <.> ".json"
  noChanNames <- readFromJSONMay (essenceDir </> modeMeta) >>= \case
                 Just ModeMeta{..} -> return $ Set.map dropExtension  no_chan
                 Nothing           -> return Set.empty



  let minfos = map (processModelsInfo meta vs_head winnerIds compactIds compactWon
                     (length fracs) fracs_size noChanNames ) mrows
  liftIO $ encodeCSV (summary </> "models.csv") minfos

  return ()

  where

    pa m =  "[" ++ (intercalate ", " m) ++ "]"

    processSettings meta numFracs fracs_size fracs_str
                    compact_str compactWon hOrder hostType ph lin =
      let ho = fromMaybe (rIterationsDone meta) hOrder
          (he, clas) =
            case splitOn "@" (IN.essence_name lin) of
              [h,c] -> (h,c)
              _     -> ("static", IN.essence_name lin)

          (kindClass, isGiven) =
              let t = T.pack $ IN.kind lin
              in (T.unpack .  T.replace "~given" "" $ t
                 , fromEnum $  "~given" `T.isInfixOf` t )

      in inToOut meta lin clas he numFracs fracs_size fracs_str
           compact_str compactWon ho hostType kindClass isGiven ph

    processModelsInfo meta lin winnerIds compactIds compactWon
                      numFracs fracs_size noChanNames
                      mrow =
      let (he, clas) =
            case splitOn "@" (IN.essence_name lin) of
              [h,c] -> (h,c)
              _     -> ("static", IN.essence_name lin)

          (fracId,win) = case MR.eprimeId mrow `M.lookup` winnerIds of
                           x@Just{} -> (x,1)
                           _        -> (Nothing, 0)
          (kindClass, isGiven) =
              let t = T.pack $ IN.kind lin
              in (T.unpack .  T.replace "~given" "" $ t
                 , fromEnum $  "~given" `T.isInfixOf` t )

      in minToOut meta lin mrow clas he win inCompact compactWon
           numFracs fracs_size fracId inNoChan  kindClass isGiven

      where
        inCompact = fromEnum $ ( MR.eprimeId mrow) `I.member` compactIds
        inNoChan  = fromEnum $ ( MR.eprime mrow) `Set.member` noChanNames

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
        -> String -> String -> Int -> String -> String -> String
        -> Int -> Int -> String -> String -> Int -> String
        -> OUT.CSV_OUT
inToOut RunMetadata{..} x@IN.CSV_IN{..}
        essenceClass heuristic numFractures fracturesSize fractures compact compactWon
        highestOrderingNeeded hostType kindClass isGiven paramsUsedHash
      = OUT.CSV_OUT{..}
 where (givenRunGroup, paramGroup) = (Nothing, Nothing)

minToOut :: RunMetadata -> IN.CSV_IN -> MR.ModelRow
         -> String -> String -> Int -> Int
         -> Int -> Int -> String -> Maybe Int -> Int -> String -> Int
         -> MI.ModelInfo
minToOut RunMetadata{..} IN.CSV_IN{..} MR.ModelRow{..}
         essenceClass heuristic isWinner isCompact compactWon
         numFractures fracturesSize fracId isNoChan kindClass isGiven
      =  MI.ModelInfo{..}
 where (givenRunGroup, paramGroup) = (Nothing, Nothing)

getFullPath :: FilePath -> IO FilePath
getFullPath s = do
    homeDir <- getHomeDirectory
    return $ case s of
        "~"             -> homeDir
        ('~' : '/' : t) -> homeDir </> t
        _               -> s

_ex1, _ex2 :: IO ()
_ex1 = showResults "/Users/bilalh/Desktop/Results/sampling_no_large/babbage/results/sdf@prob034-warehouse/nsample/sample-64_rndsols%1%16035"
_ex2 = showResults "/Users/bilalh/Desktop/Results/sampling_no_large/babbage/results/prob030-BACP/nsample/sample-64_rndsols%1%16005/"
