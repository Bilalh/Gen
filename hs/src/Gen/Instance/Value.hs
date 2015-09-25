{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.Value where

import Conjure.Language.Constant
import Conjure.Language.Definition
import Gen.Imports hiding (hash)
import Crypto.Hash
import Conjure.UI.IO(readModelFromFile)

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

readParam :: MonadIO m => ParamFP -> m Point
readParam fp = do
  Model{mStatements=sts} <- liftIO $ readModelFromFile fp
  let exprs = concatMap (\st ->
            [ (label,lit) | Declaration (Letting (label) (lit))
                          <- universe st]) sts

  let cons = (flip map) exprs $ \(n,expr) ->
          case e2c expr of
            Just x -> (n,x)
            Nothing -> error "Not a constant"

  return $ Point cons
