{-# LANGUAGE DeriveDataTypeable #-}
module Gen.Classify.Sorter where

import Data.Data
import Gen.Classify.Meta
import Gen.Imports
import Gen.IO.Formats
import Gen.IO.ToolchainData
import System.FilePath      (takeDirectory, takeFileName, takeBaseName)
import System.Posix.Files   (createSymbolicLink)
import System.Directory(canonicalizePath)

import qualified Data.Text as T


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

sorterMain :: Bool -> [String] -> IO ()
sorterMain onlyr = \case
   []     ->  putStrLn "sorterLink <dir+>"
   [x]    ->  sorter onlyr x fall
   (x:xs) ->  sorter onlyr x fall >> sorterMain onlyr xs
   <=< mapM canonicalizePath


sorter :: Bool -> FilePath -> [FuncType] -> IO ()
sorter onlyr fp_ types_ = do
  specsPaths <- ffind fp_
  metaA :: [(FilePath, Maybe SpecMeta)] <- fmap (zip specsPaths) $
    mapM (\x -> readFromJSONMay (replaceExtensions x "meta.json" ) ) specsPaths
  let meta :: [(FilePath, Maybe SpecMeta)]  = map ( \(a,b) -> (a, b) ) $ metaA

  withStats <- forM meta $ \(fp,m) -> do
    v :: Maybe DirError <- readFromJSONMay (takeDirectory fp </> "dir_error.json" )
    return (fp,m,v)

  zipWithM_ relink withStats [0..]

  where
  relink (fp, meta, stats) i = do
    let (re,dirName) = case (takeFileName . takeDirectory) fp of
                    "final" ->  (True, (takeFileName . takeDirectory . takeDirectory) fp)
                    s       ->  let t = (takeFileName . takeDirectory . takeDirectory) fp
                                in ("_r-.model" `isInfixOf` t && ".choices" `isSuffixOf` t  ,s)
    when ( (not onlyr) || (onlyr && re) ) $ do
      let dir = dirName ++ "#" ++ zeroPad 4 i
      let linker = doLink dir

      case meta of
        Nothing -> return ()
        Just m  -> forM_ types_ $ \type_ -> do
          let newDir = map ("link" </>) (getFunc type_ $ m)
          forM newDir $ \d -> do
            createDirectoryIfMissing True d
            createSymbolicLink (takeDirectory fp) (d </> dir)

      linker $ "link" </> "All"

      when (re && not onlyr) $ linker $ "link" </> "Reduced"

      case stats of
        Nothing -> return ()
        Just DirError{..} -> do
          linker $ "link" </> "Kind" </> show dirKind
          linker $ "link" </> "Status" </> show dirStatus

    where
    doLink dir base = do
      createDirectoryIfMissing True base
      createSymbolicLink (takeDirectory fp) (base </> dir)

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
      return $ (takeFileName $ fp)  == "spec.essence"

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir | takeBaseName topdir == "org" = return []
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
