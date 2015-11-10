{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, QuasiQuotes #-}
module Gen.Instance.Results where

import Conjure.Language.Definition
import Data.Csv                         hiding (Only)
import Data.List                        (break)
import Data.Map                         (Map)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField ()
import Database.SQLite.Simple.FromRow   ()
import Gen.Helpers.Str
import Gen.Imports                      hiding (group)
import Gen.Instance.Data
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


numSetsQuery, numModelsQuery, selectorQuery, compactQuery :: Query

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


showResults :: (MonadIO m)
            => FilePath ->m ()
showResults outdir = do
  let db = outdir </> "results.db"
  let summary = outdir </> "summary"
  liftIO $ createDirectoryIfMissing True summary

  (numSets,numModels,groups,comp) <- liftIO $ withConnection db $ \conn -> do
    [Only numSets]    <- query_ conn numSetsQuery
    [Only numModels]  <- query_ conn numModelsQuery
    groups   :: [Only Text] <- query_ conn selectorQuery
    compacts :: [(Text,Int)]  <- query_ conn compactQuery
    return (numSets,numModels,groups,compacts)

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

  fracs <- hittingSet summary param >>= \case
    Just (Point [(_,ConstantAbstract (AbsLitSet []))]) -> do
      liftIO $ writeFile (summary </> "hittingSet" ) "NOTHING\n"
      liftIO $ writeFile (summary </> "resultSet" ) "NOTHING\n"
      liftIO $ writeFile (summary </> "resultSet2" ) "NOTHING\n"
      return []
    Just (hset@(Point [(_,hval)])) -> do
      liftIO $ print . pretty $ hset
      liftIO $ writeFile (summary </> "hittingSet" ) (renderSized 10000 $  pretty hval)
      findResultingSet db summary hval
    _ -> do
      docError [nn  "hittingSet failed on" param]

  meta ::  RunMetadata <- liftIO $ readFromJSON (outdir </> "metadata.json")
  vs <- liftIO $ V.toList <$> decodeCSV (outdir </> "settings.csv")

  let fracs_size = pa $ map ( show . length ) fracs
  let fracs_str  = pa $ map ( pa . map show ) fracs
  let compact_str  = pa $ map ( show . snd ) comp

  let compactWon = case comp of
        [(_, num)] -> case fracs of
                        [[x]] -> if x == num then 1 else 0
                        _     -> 0
        _          ->  0



  let vs2 = map (processSettings meta (length fracs)
                                 fracs_size fracs_str compact_str compactWon) vs
  liftIO $ encodeCSV (summary </> "summary.csv") vs2

  return ()

  where
    pa m =  "[" ++ (intercalate ", " m) ++ "]"

    processSettings meta numFracs fracs_size fracs_str compact_str compactWon lin =
      let (he, clas) =
            case splitOn "@" (IN.essence_name lin) of
              [h,c] -> (Just h,c)
              _         -> (Nothing, IN.essence_name lin)


      in  inToOut meta lin clas he numFracs fracs_size fracs_str compact_str compactWon

findResultingSet :: MonadIO m => FilePath -> FilePath -> Constant -> m [[Int]]
findResultingSet db out hval = do
  script <- liftIO $ script_lookup1 "instances/results/gent_idea.py"
  runCommand' Nothing (script) [db,  (renderSized 10000 $  pretty hval) ]
    (Just (out </> "resultSet")) >>= \case
      ExitSuccess -> do
        st <- liftIO $ readFile (out </> "resultSet")
        liftIO $ putStrLn st
        let (fLines,end) = break ( (==) 'h'  . head ) (lines st)
        let fIds :: [[Int]] = [ fromJustNote "findResultingSet readIds"
                              . readMay. tail . dropWhile (/= ' ') $ ids
                              | ids <- fLines ]

        mapping :: Map Int  String  <- liftIO $ liftM M.fromList
               <$> withConnection db $ \conn -> do
          query_ conn "Select eprimeId, eprime from Eprimes"
        let errX = fromJustNote "Missing Id"
        let mapped = [ [ errX $ f  `M.lookup` mapping | f <- ids ]  |  ids <- fIds ]
        let res = [ (show $ length m) ++ " "  ++ "[" ++ (intercalate ", " m) ++ "]"  | m  <- mapped  ]

        liftIO $ writeFile (out </> "resultSet2") $ unlines (res ++ end)
        return $ fIds

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


decodeCSV ::FilePath -> IO (V.Vector IN.CSV_IN)
decodeCSV fp = do
    csvData <- BL.readFile fp
    case decodeByName csvData of
        Left err -> error  err
        Right (_, v) -> V.forM v $ \(p :: IN.CSV_IN) -> return p

encodeCSV :: FilePath -> [OUT.CSV_OUT] -> IO ()
encodeCSV fp cs = do
  BL.writeFile fp $ encodeDefaultOrderedByName cs

inToOut :: RunMetadata -> IN.CSV_IN
        -> String -> Maybe String -> Int -> String -> String -> String -> Int
        -> OUT.CSV_OUT
inToOut RunMetadata{..} IN.CSV_IN{..}
        essenceClass heuristic numFractures fracturesSize fractures compact compactWon
      = OUT.CSV_OUT{..}

_ex1, _ex2 :: IO ()
_ex1 = showResults "/Users/bilalh/Desktop/Results/sampling_no_large/babbage/results/sdf@prob034-warehouse/nsample/sample-64_rndsols%1%16035"
_ex2 = showResults "/Users/bilalh/Desktop/Results/sampling_no_large/babbage/results/prob030-BACP/nsample/sample-64_rndsols%1%16005/"
