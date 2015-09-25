{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.Point where

import Conjure.Language.Constant
import Conjure.Language.Definition
import Gen.Imports hiding (hash)
import Crypto.Hash
import Conjure.UI.IO(readModelFromFile,writeModel,EssenceFileMode(PlainEssence))

import qualified Data.ByteString.Char8 as B

newtype Point  = Point [(Name,Constant)]
 deriving (Eq, Show, Data, Typeable, Generic)

type ParamFP   = FilePath
type ParamName = String
type ParamHash = String

pointHash :: Point -> ParamHash
pointHash px =
    let name = pointName px
        dgt  = hash (B.pack name) :: Digest SHA512
    in show $ dgt

pointName :: Point -> ParamName
pointName (Point xs) = renderSized 100000 $ vcat [ pretty n <+> pretty c  | (n,c) <- xs ]

readPoint :: MonadIO m => ParamFP -> m Point
readPoint fp = do
  Model{mStatements=sts} <- liftIO $ readModelFromFile fp
  let exprs = concatMap (\st ->
            [ (label,lit) | Declaration (Letting (label) (lit))
                          <- universe st]) sts

  let cons = (flip map) exprs $ \(n,expr) ->
          case e2c expr of
            Just x -> (n,x)
            Nothing -> error "Not a constant"

  return $ Point cons


writePoint :: MonadIO m => Point  -> FilePath -> m ()
writePoint (Point ps) fp = do
  let sts = [ Declaration (Letting (label) (Constant con))
            |  (label,con) <- ps ]
  let m :: Model = def{mStatements=sts}
  liftIO $ writeModel PlainEssence (Just fp) m
