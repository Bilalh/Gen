{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, GeneralizedNewtypeDeriving #-}
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
  , getPossibilities
  , getWeights
  , giveOnly
  , giveUnmatched
  , nextVarName
  , possibleUnmatched
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
  , KeyMap(..)
  , logDebug2
  , logDepthCon
  , ldc
  , logDebugVerbose2
  , runGenerateNullLogs
  , logHigher2
  , runGenerate
  , sanityn
  ) where

import Conjure.Language.Constant
import Conjure.Language.Definition (Expression (..))
import Data.Data                   hiding (Proxy)
import Data.Map                    (Map)
import Gen.Essence.Data.Key        as X
import Gen.Helpers.Log
import Gen.Helpers.TypeOf
import Gen.Essence.Log()
import Gen.Imports
import Test.QuickCheck             (Gen, generate)

import qualified Data.Map         as M
import qualified Data.Set         as S
import qualified Text.PrettyPrint as Pr

-- | Generate a random value of a specified type
class (Data a, Pretty a) => Generate a where
  {-# MINIMAL give, possible | give, possiblePure, requires  #-}

  -- | Return a random value of type a subject to the constraints
  give  :: GenerateConstraint -> GenSt a

  -- | Returns True if this op can be used with the specified return type
  possible :: (Applicative m, MonadState St m, MonadLog m )
           => Proxy a -> GenerateConstraint -> m Bool
  possible a con = do
    d <- gets depth
    case possiblePure a (con2Ty con) d of
      False -> return False
      True  -> do
        let needed = requires a (con2Ty con)
        bs <- forM needed $ \(r) -> do
              let (f,ks) = rKindOp r
              is <- mapM weightingForKey (ks)
              return  $ f (>0) is
        return $ and bs

     where con2Ty (GType x) = Just x
           con2Ty _         = Nothing

  -- | Convenience for a pure implementation, Never call this method ouside the instance
  -- | return True if the return type can be generated within the specified depth
  -- | If no type given assume you can choose it, return True if any of the types
  -- | would be allowed
  possiblePure :: Proxy a -> Maybe Type -> Depth -> Bool
  possiblePure = error "no default possiblePure"

  -- | Convenience for a pure implementation, Never call this method ouside the instance
  -- | They types that are needed, Run after possiblePure returns True
  -- | e.g. Union
  -- |   requires _ (Just ty) = [RAll $ keyList ty]
  -- |   requires _ _         = [RAny Types.unionLike]
  requires :: Proxy a ->  Maybe Type -> [ RKind ]
  requires = error "no default requires"



-- type GenSt a = StateT St Gen a
type GenSt a = StateT St (WriterT [(LogLevel, Doc)] Gen) a

data St = St{
      weighting  :: KeyMap
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
          weighting  = def
        , depth      = $(neverNote "No depth specified")
        , newVars_   = def
        , doms_      = def
        , varCounter = 1
        }


data GenerateConstraint = GNone
                        | GType Type          -- The resulting type
                        | GOnlyTopLevel [Key] -- Weights to use at the toplevel
                        | GBinRel             -- For Relation Atrr
                        | GNonEmpty           -- For Mset domain
 deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty GenerateConstraint where
  pretty (GType x) = "GType" <+> pretty x
  pretty t = pretty . show $ t


newtype LVar = LVar Var
    deriving (Data, Typeable, Show)

instance Pretty LVar where
    pretty (LVar v) = "LVar" <+> pretty v


newtype  KeyMap = KeyMap (Map Key Int)
 deriving (Show, Data, Typeable, Eq, Generic)

instance Hashable KeyMap where
  hashWithSalt i (KeyMap ws) = hashWithSalt i  . M.toList $ ws

instance Default KeyMap where
    def = KeyMap $ M.fromList [(K_BinRelAttrStop, 800) ]

instance Pretty KeyMap where
    pretty = pretty . groom

instance ToJSON KeyMap where
  toJSON (KeyMap m) = toJSON $ M.mapKeysMonotonic (tail . tail . show) m

instance FromJSON KeyMap where
  parseJSON v = KeyMap . M.mapKeys fromString <$> parseJSON v


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
           ["Generate Depth <0"
           , pretty msg
           , pretty st
           ]

sanityn :: (MonadState St m) => Int -> String -> m ()
sanityn n msg =  do
  d <- gets depth
  st <- get
  when (d <n ) $ docError
           ["Generate Depth <" <+> pretty n
           , pretty msg
           , pretty st
           ]


-- | withDepthDec . give for Convenience
dgive :: forall a . Generate a  => GenerateConstraint -> GenSt a
dgive = withDepthDec . give


nextVarName :: MonadState St m => Text -> m Text
nextVarName prefix = do
  i <- gets varCounter
  modify $ \st -> st{varCounter=i+1}
  return $  mconcat [prefix, "_", (stringToText . show $ i)]



weightingForKey :: MonadState St m => Key -> m Int
weightingForKey key = do
  gets weighting >>= \(KeyMap kv) ->
      case key `M.lookup` kv  of
        Nothing  -> return 100
        (Just x) -> return x

withWeights :: [(Key, Int)] ->  GenSt a -> GenSt a
withWeights vs f = do
  (KeyMap old) <- gets weighting
  let new = (M.fromList vs) `M.union` old
  modify $ \st -> st{ weighting = KeyMap new }
  res <- f
  modify $ \st -> st{ weighting = KeyMap old }
  return res

getWeights :: MonadState St m => [(Key,a)] -> m [(Int, a)]
getWeights vs= do
  weights <- mapM (\(x,_) -> weightingForKey x  ) vs
  return [ (w,p) | ((_,p),w) <- zip vs weights ]

-- | getWeights but takes function. If the function return False the
-- | weighting for that item is set to 0 regresses of current weighting
getPossibilities :: arg
                 -> [(arg -> GenSt Bool, (Key, v))]
                 -> GenSt [(Int, v)]
getPossibilities con vs = do
  mapM doPossibilities vs

  where
  doPossibilities (f,(k,v)) =
   f con >>= \case
     False -> return (0,v)
     True  -> do
       w <- weightingForKey k
       return (w,v)


withDepthDec :: forall m a. MonadState St m
             => m a -> m a
withDepthDec f = do
  oldDepth <- gets depth
  modify $ \st -> st{ depth = oldDepth - 1 }
  res <- f
  modify $ \st -> st{ depth = oldDepth }
  return res


withDepth :: forall m a. MonadState St m
          => Int -> m a -> m a
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



runGenerate2 :: Generate a => LogLevel -> GenerateConstraint -> St -> IO a
runGenerate2 allowed  con st  = do
  (res, logs) <- runGenerate allowed  con st
  when (not $ Pr.isEmpty logs) $ putStrLn $ renderSized 120 $ logs
  return res


runGenerate :: Generate a => LogLevel -> GenerateConstraint -> St -> IO (a, Doc)
runGenerate allowed  con st  = do
  let s = (flip evalStateT ) st ( logInfo2 $line ["√Starting"]
                                  >> logDepthCon $line con
                                  >> give con
                                  >>= \g -> do {logInfo2 $line ["πEnding"]; return g})
  let w = runWriterT s
  (res,logs) <- generate w

  let ls =  [ msg | (lvl, msg) <- logs , lvl <= allowed ]
  return (res, vcat ls)

runGenerateWithLogs :: Generate a => GenerateConstraint -> St -> Gen (a,[(LogLevel,Doc)])
runGenerateWithLogs con st = do
  let s = (flip evalStateT ) st (give con)
  runWriterT s

runGenerateNullLogs :: Generate a => GenerateConstraint -> St -> Gen a
runGenerateNullLogs con st = do
  let s = (flip evalStateT ) st (give con)
  fst <$> runWriterT s

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
-- | Really should be reanmed
class WrapConstant a where
  wrapConstant :: Constant -> a
  wrapDomain   :: Domain () a -> a
  allowEmpty   :: Proxy a -> Bool

instance WrapConstant Expr where
  wrapConstant = ECon
  wrapDomain  =  EDom
  allowEmpty _ = True

instance WrapConstant Constant where
  wrapConstant = id
  wrapDomain   = DomainInConstant
  allowEmpty _ = False


instance WrapConstant Expression where
  wrapConstant = Constant
  wrapDomain   = Domain
  allowEmpty _ = True


logHigher2 :: MonadLog m => String -> [Doc] -> m ()
logHigher2 ln docs = log LogFollow . hang (pretty ln) 4 $ Pr.vcat docs


logInfo2 :: MonadLog m => String -> [Doc] -> m ()
logInfo2 ln docs = log LogInfo . hang (pretty ln) 4 $ Pr.vcat docs


logDebug2 :: MonadLog m => String -> [Doc] -> m ()
logDebug2 ln docs = log LogDebug . hang (pretty ln) 4 $ Pr.vcat docs

logDebugVerbose2 :: MonadLog m => String -> [Doc] -> m ()
logDebugVerbose2 ln docs = log LogDebugVerbose . hang (pretty ln) 4 $ Pr.vcat docs


logDepthCon :: forall m b. (MonadState St m, MonadLog m, Pretty b) =>
               String -> b -> m ()
logDepthCon l con= gets depth >>= \d -> logInfo2 l [nn "depth" d, nn "con" con]

ldc :: forall m b b1.
       (MonadState St m, MonadLog m, Pretty b1) =>
       String -> b1 -> m b -> m b
ldc l con f= do
  logDepthCon l con
  f
