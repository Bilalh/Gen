{-# LANGUAGE TupleSections, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
{-# OPTIONS_GHC -fno-warn-unused-matches  #-}
module Gen.IO.Dups(removeSolveDups) where

import Gen.Imports
import Gen.IO.RunResult (MonadDB)
import Data.Digest.Pure.MD5

import qualified Data.ByteString.Char8 as C


data DirData = DirData {
      spec    :: MD5Digest
    , eprimes :: [(MD5Digest,MD5Digest)] -- eprime, error
    } deriving (Show,Eq,Generic)

-- | remove dups when multipe errors reduce to same cause
removeSolveDups :: (MonadDB m, MonadIO m, Functor m) => [Directory] -> m ()
removeSolveDups dirs = do
  hashes <- mapM (\d -> (d ,) <$> makeData d) dirs >>= return . filter ( isJust . snd)
  $notDone


makeData :: (MonadIO m) => Directory -> m (Maybe DirData)
makeData dir = do
  $notDone


hashFileStrict :: FilePath -> IO MD5Digest
hashFileStrict fp = do
 content <- C.readFile fp
 let slns = C.concat $ [ x | x <- C.lines content
                        , not $ "###" `C.isPrefixOf` x
                        , not $ "+"   `C.isPrefixOf` x
                        ]
 return $ hash' slns
