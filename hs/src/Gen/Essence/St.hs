{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             ParallelListComp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.St
  ( module X
  , GenSt
  , Generate(..)
  , GenerateConstraint(..)
  , St(..)
  , WrapConstant(..)
  , fieldKeys
  , generateFreq
  , generateTypeFreq
  , getId
  , getPossibilities
  , getWeighting
  , getWeights
  , giveOnly
  , giveUnmatched
  , nextVarName
  , possibleUnmatched
  , runGenerate
  , runGenerate2
  , runGenerateWithLogs
  , weightingForKey
  , withDepthDec
  , withVars
  , withWeights
  , LVar(..)
  , logInfo2
  , ToJSON(..)
  , dgive
  , sanity
  , withDepth
  , giveOnlyFunc
  , RKind(..)
  ) where

import Conjure.Language.Definition (Expression (..))
import Data.Data                   hiding (Proxy)
import Data.Map                    (Map)
import Gen.Essence.Data.Key  as X
import Gen.Helpers.Log
import Gen.Helpers.TypeOf
import Gen.Imports
import Test.QuickCheck             (Gen, generate)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.PrettyPrint as Pr


instance ToJSON (Map Key Int) where
  toJSON m = toJSON $ M.mapKeysMonotonic (tail . tail . show) m


-- | Generate a random value of a specified type
class (Data a, Pretty a) => Generate a where
  {-# MINIMAL give, possible | give, possiblePure, requires  #-}

  -- | Return a random value of type a subject to the constraints
  give  :: GenerateConstraint -> GenSt a

  -- | Returns True if this op can be used with the specified return type
  possible :: (Applicative m, MonadState St m) => Proxy a -> GenerateConstraint -> m Bool
  possible a (GType ty) = do
    d <- gets depth
    return $ possiblePure a (Just ty) d

  possible a _ = do
    d <- gets depth
    case possiblePure a Nothing d of
      False -> return False
      True  -> do
        let needed = requires a Nothing
        bs <- forM needed $ \(r) -> do
              let (f,ks) = rKindOp r
              is <- mapM weightingForKey (ks)
              return  $ f (>0) is
        return $ and bs

  -- | Convenience for a pure implementation, Never call this method ouside the instance
  -- | return True if the return type can be generated within the specified depth
  -- | If no type given assume you can choose it, return True if any of the types
  -- | would be allowed
  possiblePure :: Proxy a -> Maybe Type -> Depth -> Bool
  possiblePure = error "no default possiblePure"

  -- | Convenience for a pure implementation, Never call this method ouside the instance
  -- | They types that are needed
  -- | e.g. Union
  -- |   requires _ (Just ty) = [RAll $ keyList ty]
  -- |   requires _ _         = [RAny Types.unionLike]
  requires :: Proxy a ->  Maybe Type -> [ RKind ]
  requires = error "no default requires"

data RKind = RAny [Key] | RAll [Key]
           deriving (Show,Eq)

rKindOp :: RKind -> ( ((Int -> Bool) -> [Int] -> Bool), [Key] )
rKindOp (RAny xs) = (any, xs)
rKindOp (RAll xs) = (all, xs)

sanity :: (MonadState St m) => String -> m ()
sanity msg =  do
  d <- gets depth
  st <- get
  when (d <0 ) $ docError
           ["Generate Expr Depth <0"
           , pretty msg
           , pretty st
           ]

-- | withDepthDec . give for Convenience
dgive :: forall a . Generate a  => GenerateConstraint -> GenSt a
dgive = withDepthDec . give

getId :: Data a => Proxy a -> Key
getId = fromString . tyconUQname . dataTypeName . proxyDataTypeOf

getWeighting :: (MonadState St m, Data a) => Proxy a -> m Int
getWeighting a = do
    let key = getId a
    gets weighting >>= \kv ->
        case key `M.lookup` kv  of
          Nothing  -> return 100
          (Just x) -> return x



nextVarName :: MonadState St m => Text -> m Text
nextVarName prefix = do
  i <- gets varCounter
  modify $ \st -> st{varCounter=i+1}
  return $  mconcat [prefix, "_", (stringToText . show $ i)]



-- type GenSt a = StateT St Gen a
type GenSt a = StateT St (WriterT [(LogLevel, Doc)] Gen) a

data GenerateConstraint = GNone
                        | GType Type          -- The resulting type
                        | GOnlyLiteralTypes   -- for literals
                        | GOnlyTopLevel [Key] -- Weights to use at the toplevel
 deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty GenerateConstraint where
  pretty (GType x) = "GType" <+> pretty x
  pretty t = pretty . show $ t


newtype LVar = LVar Var
    deriving (Data, Typeable, Show)

instance Pretty LVar where
    pretty (LVar v) = "LVar" <+> pretty v

data St = St{
      weighting  :: Map Key Int
    , depth      :: Int
    , varCounter :: Int
    , newVars_   :: [Var] -- Domains from e.g. forall
    , doms_      :: Domains
    }
 deriving (Eq,Show)



instance Pretty St where
    pretty (St{..}) =
        "St" <+> Pr.braces (
            Pr.sep [
                    nn "depth"       $ depth
                  , nn "newVars_"    $ prettyTypeArr newVars_
                  , nn "doms"        $ doms_
                  , nn "varCounter"  $ varCounter
                  , nn "weighting" (pretty . groom $  weighting)
                  ] )


instance Default St where
  def = St{
          weighting  = M.fromList [(K_BinRelAttrStop, 800) ]
        , depth      = $(neverNote "No depth specified")
        , newVars_   = def
        , doms_      = def
        , varCounter = 1
        }


weightingForKey :: MonadState St m => Key -> m Int
weightingForKey key = do
  gets weighting >>= \kv ->
      case key `M.lookup` kv  of
        Nothing  -> return 100
        (Just x) -> return x

withWeights :: [(Key, Int)] ->  GenSt a -> GenSt a
withWeights vs f = do
  old <- gets weighting
  let new = (M.fromList vs) `M.union` old
  modify $ \st -> st{ weighting = new }
  res <- f
  modify $ \st -> st{ weighting = old }
  return res

getWeights :: MonadState St m => [(Key,a)] -> m [(Int, a)]
getWeights vs= do
  weights <- mapM (\(x,_) -> weightingForKey x  ) vs
  return [ (w,p) | (_,p) <- vs
                 | w <- weights  ]

-- | getWeights but takes function. If the function return False the
-- | weighting for that item is set to 0 regresses of current weighting
getPossibilities :: GenerateConstraint
                 -> [(GenerateConstraint -> GenSt Bool, (Key, v))]
                 -> GenSt [(Int, v)]
getPossibilities con vs = do
  mapM doPossibilities vs

  where
  doPossibilities ::(GenerateConstraint -> GenSt Bool, (Key, v))
                  -> GenSt (Int, v)
  doPossibilities (f,(k,v)) =
   f con >>= \case
     False -> return (0,v)
     True  -> do
       w <- weightingForKey k
       return (w,v)


withDepthDec :: GenSt a -> GenSt a
withDepthDec f = do
  oldDepth <- gets depth
  modify $ \st -> st{ depth = oldDepth - 1 }
  res <- f
  modify $ \st -> st{ depth = oldDepth }
  return res


withDepth :: Int -> GenSt a -> GenSt a
withDepth d f = do
  oldDepth <- gets depth
  modify $ \st -> st{ depth = d }
  res <- f
  modify $ \st -> st{ depth = oldDepth }
  return res



withVars :: [Var] ->  GenSt a -> GenSt a
withVars nvars f = do
  old <- gets newVars_
  modify $ \st -> st{ newVars_ = nvars ++ old }
  res <- f
  modify $ \st -> st{ newVars_ = old }
  return res


-- | Error message for give
giveUnmatched :: forall c a. Pretty a => Doc -> a -> GenSt c
giveUnmatched msg t = do
  st  <- get
  error . show . vcat $ ["Unmatched give" <+> msg
                        , "Show  " <+> (pretty . show ) t
                        , "Pretty" <+> pretty t
                        , "St"     <+> pretty st]


-- | Error message for give
possibleUnmatched :: forall m c a. (Pretty a, MonadState St m) => Doc -> a -> m c
possibleUnmatched msg t = do
  st  <- get
  error . show . vcat $ ["Unmatched possible" <+> msg
                        , "Show  " <+> (pretty . show ) t
                        , "Pretty" <+> pretty t
                        , "St"     <+> pretty st]


runGenerate :: Generate a => St -> IO a
runGenerate = runGenerate2 LogNone GNone

runGenerate2 :: Generate a => LogLevel -> GenerateConstraint -> St -> IO a
runGenerate2 allowed  con st  = do
  let s = (flip evalStateT ) st (give con)
  let w = runWriterT s
  (res,logs) <- generate w

  let ls =  [ msg | (lvl, msg) <- logs , lvl <= allowed ]
  case ls of
    [] -> return ()
    xs -> putStrLn $ renderSized 120 $  vcat xs
  return res

runGenerateWithLogs :: Generate a => GenerateConstraint -> St -> Gen (a,[(LogLevel,Doc)])
runGenerateWithLogs con st = do
  let s = (flip evalStateT ) st (give con)
  runWriterT s


-- | DataType of a proxy
proxyDataTypeOf :: forall a . Data a => Proxy a -> DataType
proxyDataTypeOf _ = dataTypeOf (error "proxyDataTypeOf" :: a)

-- | Keys of all fields in a datatype
fieldKeys :: Data a => Proxy a -> [Key]
fieldKeys a = do
  let names = dataTypeConstrs . proxyDataTypeOf $ a
  map (fromString . show) names

-- | Reduce the weighting of all key (of the specific type) NOT mentioned to 0
giveOnly :: forall a . (Data a, Generate a)
         => GenerateConstraint ->  [Key] -> GenSt a
giveOnly g keys = do
  let allowed = S.fromList keys
  let ws = [ (k,0) | k <- fieldKeys (Proxy :: Proxy a), k `S.notMember` allowed ]
  withWeights ws (give g)

-- | Reduce the weighting of all key (of the specific type) NOT mentioned to 0
giveOnlyFunc :: forall a b . (Data a, Generate a, Data b, Generate b)
         =>  Proxy b ->  [Key] -> GenSt a -> GenSt a
giveOnlyFunc proxy keys f = do
  let allowed = S.fromList keys
  let ws = [ (k,0) | k <- fieldKeys proxy, k `S.notMember` allowed ]
  withWeights ws f

-- | Generate n values of type a and print the frequency of them
generateFreq :: forall a . (Generate a, Pretty a, Ord a)
             => Proxy a -> GenerateConstraint -> Int -> St -> IO ()
generateFreq _ con n st = do
  let freq s = sort . map (\x->(length x,head x)) . group . sort $ s
  ts :: [a] <-  mapM  (\_ -> runGenerate2 LogNone con  st)  [1..n]
  print .  vcat .  map pretty $ freq ts


-- | Generate n values of type a and print the frequency of them
generateTypeFreq :: forall a . (Generate a, Pretty a, Ord a, TTypeOf a)
                  => Proxy a -> GenerateConstraint -> Int -> St -> IO  ()
generateTypeFreq _ con n st = do
  let freq s = sort . map (\x->(length x,head x)) . group . sort $ s
  ts :: [a] <-  mapM  (\_ ->  runGenerate2 LogNone  con st)  [1..n]
  tys <- mapM ttypeOf ts
  print .  vcat .  map pretty $ freq tys


-- | To allow a Range Constant or a Range Expr
class WrapConstant a where
  wrapConstant :: Constant -> a

instance WrapConstant Expr where
  wrapConstant = ECon

instance WrapConstant Constant where
  wrapConstant = id

instance WrapConstant Expression where
  wrapConstant = Constant

instance MonadLog (WriterT [(LogLevel, Doc)] Gen)  where
    log lvl msg = tell [(lvl, msg)]


logInfo2 :: MonadLog m => String -> [Doc] -> m ()
logInfo2 ln docs = log LogInfo . hang (pretty ln) 4 $ vcat docs
