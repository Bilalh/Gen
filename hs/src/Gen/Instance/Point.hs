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
provideValues :: MonadIO m => Provider -> m Point
provideValues (Provider xs) = do
  vs <- flip execStateT [] $
    forM xs $ \(n, d) -> do
        rnd <- domainRandomValue d
        modify $ \old -> (n,rnd) : old
  return $ Point vs

-- For simple true like ints and bools
domainRandomValue :: MonadIO m => Domain () Constant -> m Constant
domainRandomValue (DomainInt [])              = error "Not int values"
domainRandomValue (DomainInt [RangeSingle a]) = return a
domainRandomValue (DomainInt [RangeBounded (ConstantInt a) (ConstantInt b) ]) = do
  chosen <- liftIO $ randomRIO (a,b)
  return $ ConstantInt chosen

domainRandomValue (DomainInt _) = error "Only single range of int allowed"
domainRandomValue _             = error "Only Int domains supported"

class Distance a where
    distance :: a -> a -> Integer

-- instance Distance Constant where
--   distance (ConstantBool c)        (ConstantBool d)        = _x
--   distance (ConstantInt c)         (ConstantInt d)         = (c ^ d)
--   distance (ConstantEnum c1 c2 c3) (ConstantEnum d1 d2 d3) = _x
--   distance (ConstantField c1 c2)   (ConstantField d1 d2)   = _x
--   distance (ConstantAbstract c)    (ConstantAbstract d)    = _x
--   distance c d = docError ["Unsupported" ,nn "c" c ,nn "d" d ]

-- instance Distance (AbstractLiteral Constant) where
--   distance (AbsLitTuple c)           (AbsLitTuple d)          = _x
--   distance (AbsLitRecord c)          (AbsLitRecord d)         = _x
--   distance (AbsLitVariant c1 c2 c3)  (AbsLitVariant d1 d2 d3) = _x
--   distance (AbsLitMatrix c1 c2)      (AbsLitMatrix d1 d2)     = _x
--   distance (AbsLitSet c)             (AbsLitSet d)            = _x
--   distance (AbsLitMSet c)            (AbsLitMSet d)           = _x
--   distance (AbsLitFunction c)        (AbsLitFunction d)       = _x
--   distance (AbsLitSequence c)        (AbsLitSequence d)       = _x
--   distance (AbsLitRelation c)        (AbsLitRelation d)       = _x
--   distance (AbsLitPartition c)       (AbsLitPartition d)      = _x
