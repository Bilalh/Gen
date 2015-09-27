{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.Point where

import Conjure.Language.Constant
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.UI.IO               (EssenceFileMode (PlainEssence),
                                    readModelFromFile, writeModel)
import Crypto.Hash
import Gen.Imports                 hiding (hash)
import Gen.Instance.Data
import System.FilePath             (takeDirectory)

import qualified Data.ByteString.Char8 as B


-- | Param Values
newtype Point  = Point [(Name,Constant)]
 deriving (Eq, Show, Data, Typeable, Generic)

instance Monoid Point where
  mempty                        = Point []
  mappend (Point xs) (Point ys) = Point $ xs ++ ys

type ParamFP   = FilePath
type ParamName = String
type ParamHash = String


pointName :: Point -> ParamName
pointName (Point xs) = renderSized 100000 $ vcat [ pretty n <+> pretty c  | (n,c) <- xs ]

pointHash :: Point -> ParamHash
pointHash px =
    let name = pointName px
        dgt  = hash (B.pack name) :: Digest SHA512
    in show $ dgt


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
  liftIO $ createDirectoryIfMissing True (takeDirectory fp)
  liftIO $ writeModel PlainEssence (Just fp) m


-- Give a value for each domain using the previous values for future domains references
provideValues :: Monad m => Provider -> m Point
provideValues (Provider xs) = do
  vs <- flip execStateT [] $
    forM xs $ \(n, d) -> do
        rnd <- domainRandomValue d
        modify $ \old -> (n,rnd) : old
  return $ Point vs

-- For simple true like ints and bools
domainRandomValue :: Monad m => Domain () Constant -> m Constant
domainRandomValue (DomainInt rs) = return (ConstantInt 3)
