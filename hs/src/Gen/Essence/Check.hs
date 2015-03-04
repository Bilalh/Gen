module Gen.Essence.Check where

import Conjure.Language.Definition
import Gen.Prelude
import Gen.IO.Formats
-- import Gen.Arbitrary.Arbitrary
import Gen.Helpers.Runner

-- import Gen.Classify.Meta(mkMeta)

-- import Conjure.Language.Domain
-- import Conjure.UI.IO(readModelFromFile)

import qualified Data.Map as M

getResult :: Cores ->  Int -> FilePath -> Bool -> Spec -> IO RefineR
getResult cores time outBase newConjure specE = do
    sp  <- toConjure specE
    _   <- typeChecks sp
    ts <- timestamp >>= return . show
    num <- (randomRIO (10,99) :: IO Int)  >>= return . show
    -- let num = show ts_int_
    let uname  =  (ts ++ "_" ++ num )
    let outdir =  (out </> uname)
    createDirectoryIfMissing True outdir
    -- writeFile (outdir </> "spec.logs" ) (renderSized 120 wlogs_)
    writeFile (outdir </> "spec.specE" ) (show specE)

    -- let meta = mkMeta specE
    -- writeFile (outdir </> "spec.meta" ) (show meta)
    -- writeJSON  (outdir </> "spec.meta.json" ) (meta)

    result <- runRefine' 44 cores sp (out </> uname ) time newConjure
    return $ result

    where
    out     = outBase </> "_passing"
    errdir  = outBase </> "_errors"


main startTime =  do
  f startTime

  where
  f time | time < 0 = return ()
  f time = do
    res <- getResult 1 time "outt" True aSpec
    putStrLn . groom $ res
    f (time - 5)



aSpec :: Spec
aSpec = $notDone

typeChecks :: MonadFail m => Model -> m ()
typeChecks m = return ()
