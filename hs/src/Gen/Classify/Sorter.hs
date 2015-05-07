{-# LANGUAGE DeriveDataTypeable #-}
module Gen.Classify.Sorter where

import Data.Data
import Data.Maybe         (fromJust)
import Gen.Classify.Meta
import Gen.IO.Formats
import Gen.Imports
import System.FilePath    (takeDirectory, takeExtensions, takeFileName)
import System.Posix.Files (createSymbolicLink)

import qualified Data.Text as T


data SArgs = SArgs{
        fp_ :: String
      , types_ :: [FuncType]
    }

data FuncType =
      TDepth
    | TcDepth
    | TdDepth
    | TdTypes
    | TdTypesComplex
    | TFeatures
    | Tlength
    deriving (Data, Typeable)

fall :: [FuncType]
fall = map fromConstr $ dataTypeConstrs . dataTypeOf $ (error "FuncType" :: FuncType)

sorterMain :: IO ()
sorterMain = getArgs >>= sorterMain'

sorterMain' :: [String] -> IO ()
sorterMain' = \case
   []     ->  putStrLn "sorterLink <dir+>"
   [x]    ->  sorter SArgs{fp_=x,types_=fall}
   (x:xs) ->  sorter SArgs{fp_=x,types_=fall} >> sorterMain' xs


sorter :: SArgs -> IO ()
sorter SArgs{fp_,types_} = do
  metaJson <- ffind fp_
  metaA :: [(FilePath, Maybe SpecMeta)] <- fmap (zip metaJson) $ mapM readFromJSON metaJson
  let meta :: [(FilePath, SpecMeta)]  = map ( \(a,b) -> (a, fromJust b) )
                 $ flip filter metaA $ \(_,b) -> isJust b

  void $ zipWithM relink meta [0..]

  where
  relink (fp, meta) i = do
    let dirName = case (takeFileName . takeDirectory) fp of
                    "final" ->  (takeFileName . takeDirectory .takeDirectory) fp
                    s       ->  s
    let dir = dirName ++ "#" ++ zeroPad 4 i

    forM types_ $ \type_ -> do
      let newDir = map ("link" </>) (getFunc type_ $ meta)
      forM newDir $ \d -> do
        createDirectoryIfMissing True d
        createSymbolicLink (takeDirectory fp) (d </> dir)

getFunc :: FuncType -> (SpecMeta -> [FilePath])
getFunc TDepth = \SpecMeta{..} ->
                  ["depths" </>
                   zeroPad 2 (fromInteger dom_depth_) </>
                   zeroPad 2 (fromInteger constraint_depth_)
                  ]

getFunc TcDepth = \SpecMeta{..} ->
                  ["constraints_depth" </>
                   zeroPad 2 (fromInteger constraint_depth_)
                  ]

getFunc TdDepth = \SpecMeta{..} ->
                  ["dom_depth" </>
                   zeroPad 2 (fromInteger dom_depth_)
                  ]

getFunc TdTypes = \SpecMeta{..} ->
                  map ( ("dom_type" </>) . prettyShowType ) dom_types_

getFunc TFeatures = \SpecMeta{..} ->
                  map ( ("features" </>) .  show ) features_

getFunc Tlength = \SpecMeta{..} ->
                  [ "lengths" </> "constraints"  </> zeroPad 2  constraint_count_
                  , "lengths" </> "dom" </> zeroPad 2  dom_count_
                  ]

getFunc TdTypesComplex = \SpecMeta{..} ->
                  [ "dom_type_most_complex" </> prettyShowType dom_most_complex_ ]

prettyShowType ::Type -> String
prettyShowType ty =
    let t   = T.pack . show . pretty $ ty
        res = T.unpack
            . T.take 150
            . T.unwords
            . T.words
            . T.replace "\n" ""
            $ t

    in res



ffind :: FilePath -> IO [FilePath]
ffind path = do
  names <- getRecursiveContents path
  filterM p names

  where
    p fp = do
      return $ (takeExtensions $ fp)  == ".meta.json"

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".." ]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

zeroPad :: Int -> Int ->  String
zeroPad p n = replicate (p - length sn) '0'  ++ sn
 where sn = show n
