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
import Data.List                  (iterate, takeWhile)

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S

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
    distanceSq :: a -> a -> Integer
    distanceSq a b = distance a b ^ (2 :: Integer)

instance Distance Point where
  distance (Point center) (Point point) =
    let summed = sum [ (distanceSq c p)   | ((_,p),(_,c)) <- zip center point ]
    in squareRoot summed

instance Distance Constant where
  distance (ConstantAbstract c)    (ConstantAbstract d)    =
      distance c d
  distance (ConstantBool c)        (ConstantBool d)        =
      fromIntegral $ (fromEnum c) - (fromEnum d)
  distance (ConstantInt c)         (ConstantInt d)         =
      (c - d)
--   distance (ConstantEnum c1 c2 c3) (ConstantEnum d1 d2 d3) = _x
--   distance (ConstantField c1 c2)   (ConstantField d1 d2)   = _x

  distance c d = docError [ "Unsupported Distance Constant"
                          , pretty $line
                          , nn "c" c ,nn "d" d ]

instance Distance (AbstractLiteral Constant) where
  distance (AbsLitTuple c)           (AbsLitTuple d)          =
       squareRoot $ sum [ distanceSq a b  |  (Just a, Just b) <- zipPad c d  ]
  -- distance (AbsLitRecord c)          (AbsLitRecord d)         = _x
  -- distance (AbsLitVariant c1 c2 c3)  (AbsLitVariant d1 d2 d3) = _x
  -- distance (AbsLitMatrix c1 c2)      (AbsLitMatrix d1 d2)     = _x
  distance (AbsLitSet c)             (AbsLitSet d)            =
      let sc  = S.fromList c
          sd  = S.fromList d
          res = S.size (S.difference sc sd) + S.size (S.difference sd sc)
      in squareRoot $  fromIntegral res
  -- distance (AbsLitMSet c)            (AbsLitMSet d)           = _x
  distance (AbsLitFunction c)        (AbsLitFunction d)       =
       squareRoot $ sum [ distanceSq v1 v2 | (Just (_,v1), Just (_,v2))
                            <- zipPad (sort c) (sort d)]
  -- distance (AbsLitSequence c)        (AbsLitSequence d)       = _x
  distance (AbsLitRelation c)        (AbsLitRelation d)       =
      distance (AbsLitSet $ map (ConstantAbstract .AbsLitTuple ) c)
               (AbsLitSet $ map (ConstantAbstract .AbsLitTuple ) d)
  -- distance (AbsLitPartition c)       (AbsLitPartition d)      = _x

  distance c d = docError [ "Unsupported Distance AbstractLiteral Constant"
                          , pretty $line
                          , nn "c" c ,nn "d" d ]


zipPad :: [a] -> [a] -> [(Maybe a, Maybe a)]
zipPad [] [] = []
zipPad xs ys = (next xs, next ys) : zipPad (rest xs) (rest ys)

next :: [a] -> Maybe a
next []    = Nothing
next (x:_) = Just x

rest ::  [a] -> [a]
rest [] = []
rest xs = tail xs

-- sqrt does not work on Integers
-- https://wiki.haskell.org/Generic_number_type#squareRoot
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters

   where
     (^!) :: Num a => a -> Int -> a
     (^!) x m = x^m
