{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Gen.Instance.Point where

import Conjure.Language.Constant
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty     (prettyList)
import Conjure.Process.Enumerate
import Conjure.UI                  (OutputFormat( Plain ))
import Conjure.UI.IO               (readModelFromFile, writeModel)
import Crypto.Hash
import Data.List                   (iterate)
import Gen.Imports                 hiding (hash)
import System.FilePath             (takeDirectory)

import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Set              as S
import qualified Data.Map              as M

type ParamFP   = FilePath
type PointName = String
type PointHash = String

-- | Param Values
newtype Point  = Point [(Name,Constant)]
 deriving (Eq, Show, Data, Typeable, Generic)

instance Hashable Point

instance A.FromJSON Point
instance A.ToJSON Point

instance Pretty Point where
    pretty (Point []) = "⟪⟫"
    pretty (Point xs) =  "⟪" <+> (vcat . map pretty $ xs) <+> "⟫"

instance Monoid Point where
  mempty                        = Point []
  mappend (Point xs) (Point ys) = Point $ xs ++ ys


-- | Provides values for givens
newtype Provider = Provider [(Name, Domain () Constant)]
    deriving (Eq, Show, Data, Typeable, Generic)

instance A.FromJSON Provider
instance A.ToJSON Provider


pointName :: Point -> PointName
pointName (Point xs) = renderSized 100000 $ vcat [ pretty n <+> pretty c  | (n,c) <- xs ]

pointHash :: Point -> PointHash
pointHash px =
    let name = pointName px
        dgt  = hash (B.pack name) :: Digest SHA512
    in show $ dgt


removeNames :: [Text] -> Point -> Point
removeNames rm (Point xs)= Point $ [ x | x@(Name n,_) <- xs, n `notElem` rm ]

onlyNames :: Set Text -> Point -> Point
onlyNames keep (Point xs) = Point $ [ x | x@(Name n,_) <- xs, n `S.member` keep ]

onlyNames2 :: Set Name -> Point -> Point
onlyNames2 keep (Point xs) = Point $ [ x | x@(n,_) <- xs, n `S.member` keep ]


readPoint :: MonadIO m => ParamFP -> m Point
readPoint fp = do
  liftIO $ readModelFromFile fp >>= return . modelToPoint

readPointMay :: MonadIO m  => ParamFP -> m (Maybe Point)
readPointMay fp = do
  liftIO $ doesFileExist fp >>= \case
    False -> return Nothing
    True  -> Just <$> readPoint fp

writePoint :: MonadIO m => Point  -> FilePath -> m ()
writePoint (Point ps) fp = do
  let sts = [ Declaration (Letting (label) (Constant con))
            |  (label,con) <- ps ]
  let m :: Model = def{mStatements=sts}
  liftIO $ createDirectoryIfMissing True (takeDirectory fp)
  liftIO $ writeModel 120 Plain (Just fp) m


pointToModel :: Point -> Model
pointToModel (Point ps) =
  let sts = [ Declaration (Letting (label) (Constant con))
            |  (label,con) <- ps ]
      m :: Model = def{mStatements=sts}
  in  m

modelToPoint :: Model -> Point
modelToPoint Model{mStatements=sts} = do
  let exprs = concatMap (\st ->
            [ (label,lit) | Declaration (Letting (label) (lit))
                          <- universe st]) sts

  let process (n,(Domain dom)) =
        case mapM e2c dom of
          Just x -> (n,DomainInConstant x)
          Nothing -> lineError $line ["Not a constant (Domain)"
                                     , nn "n" n
                                     , nn "dom" dom
                                     , nn "dom" (groom dom)
                                     , nn "exprs" (prettyArr exprs)
                                     ]
      process (n,expr) =
        case e2c expr of
          Just x -> (n,x)
          Nothing -> lineError $line ["Not a constant"
                                     , nn "n" n
                                     , nn "expr" expr
                                     , nn "expr" (groom expr)
                                     , nn "exprs" (prettyArr exprs)
                                     ]

  Point $  map process exprs


-- Give a value for each domain using the previous values for future domains references
provideValues :: MonadIO m => Provider -> m Point
provideValues (Provider xs) = do
  vs <- flip execStateT [] $
    forM xs $ \(n, d) -> do
        rnd <- domainRandomValue d
        modify $ \old -> (n,rnd) : old
  return $ Point vs

-- | Uses enumerateDomain to provides all the values of the givens
provideAllValues :: (MonadIO m, MonadLog m) => Provider -> m [Point]
provideAllValues (Provider xs) = do
  cs <- forM xs $ \(n,d) -> do
          ds <- liftIO $ enumerateDomain d
          return (n,ds)
  let (names,byName) = unzip cs
  -- logDebug2 $line (map (prettyArr) byName)
  let expended = sequence byName

  let res = [  Point $ zip names vals   | vals <-expended]
  -- logDebug2 $line [ nn "res" (length res)
  --                 , nn "expended" (length expended)
  --                 , nn "byName" (length byName)]
  return res

-- | For simple true like ints and bools
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
  distance (AbsLitSet c)             (AbsLitSet d)            =
      let sc  = S.fromList c
          sd  = S.fromList d
          res = S.size (S.difference sc sd) + S.size (S.difference sd sc)
      in squareRoot $  fromIntegral res
  -- distance (AbsLitMSet c)            (AbsLitMSet d)           = _x
  distance (AbsLitFunction c)        (AbsLitFunction d)       =
       squareRoot $ sum [ distanceSq v1 v2 | (Just (_,v1), Just (_,v2))
                            <- zipPad (sort c) (sort d)]
  distance (AbsLitMatrix _ c2)       (AbsLitMatrix _ d2)      =
       squareRoot $ sum [ distanceSq v1 v2 | (v1,  v2) <- zip c2 d2]


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
  where
  next :: [a] -> Maybe a
  next []    = Nothing
  next (x:_) = Just x

  rest ::  [a] -> [a]
  rest [] = []
  rest zs = tail zs

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

giveParam :: FilePath -> IO (Maybe Point)
giveParam spec_directory = do
  mayParam <- doesFileExist (spec_directory </> "given.param") >>= \case
    True  -> Just <$> readPoint  (spec_directory </> "given.param")
    False -> return Nothing

  essenceM <- readModelFromFile (spec_directory </> "spec.essence")
  let givens = [ nm | Declaration (FindOrGiven Given nm _) <- mStatements essenceM ]
  when (not (null givens) &&  mayParam == Nothing ) $
      docError
          [ "The problem specification is parameterised, but no *.param files are given."
          , "Parameters:" <+> prettyList id "," givens
          ]

  return mayParam

checkForParamIfNeeded :: Spec -> Maybe Point -> IO ()
checkForParamIfNeeded (Spec (doms) _ _)  mayParam = do
  let givens = M.filter isGiven doms
  when (not (M.null givens) &&  mayParam == Nothing ) $
      docError
          [ "The problem specification is parameterised, but no *.param files are given."
          , "Parameters:" <+> prettyList id "," (map (second snd) $ M.toList givens)
          ]

  where
  isGiven :: (Int,GF) -> Bool
  isGiven (_, Givenn{}) = True
  isGiven _             = False
