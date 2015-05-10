{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             ParallelListComp #-}
module Gen.Essence.St
  ( module X
  , GenSt
  , Generate(..)
  , GenerateConstraint(..)
  , giveOnly
  , St(..)
  , WrapConstant(..)
  , fieldKeys
  , generateFreq
  , generateTypeFreq
  , getPossibilities
  , getWeights
  , giveUnmatched
  , runGenerate
  , runGenerateWithLogs
  , weightingForKey
  , withDepthDec
  , withWeights
  , possibleUnmatched
  ) where

import Conjure.Language.Definition (Expression (..))
import Data.Data                   hiding (Proxy)
import Data.Map                    (Map)
import Gen.Essence.Key             as X
import Gen.Helpers.Log
import Gen.Helpers.TypeOf
import Gen.Imports
import Test.QuickCheck             (Gen, generate)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.PrettyPrint as Pr


class Data a => Generate a where
  {-# MINIMAL give, possible | give, possiblePure, possibleNoType  #-}

  -- | Return a random value of type a subject to the constraints
  give  :: GenerateConstraint -> GenSt a

  -- | Returns True if this op can be used with the specified return type
  possible :: (Applicative m, MonadState St m) => Proxy a -> GenerateConstraint -> m Bool
  possible a (GType ty) = do
    d <- gets depth
    return $ possiblePure a ty d

  possible a _ = do
    d <- gets depth
    return $ possibleNoType a d

  -- | Convenience for a pure implementation, Never call this method
  possiblePure :: Proxy a -> Type -> Depth -> Bool
  possiblePure = error "no default possiblePure"

  possibleNoType :: Proxy a -> Depth -> Bool
  possibleNoType _ _ = error "no default possibleNoType"

  getId ::  Proxy a -> Key
  getId = fromString . tyconUQname . dataTypeName . proxyDataTypeOf

  getWeighting :: MonadState St m => Proxy a -> m Int
  getWeighting a = do
      let key = getId a
      gets weighting >>= \kv ->
          case key `M.lookup` kv  of
            Nothing  -> return 100
            (Just x) -> return x


type GenSt a = StateT St Gen a

data GenerateConstraint = GNone
                        | GType Type          -- The resulting type
                        | GOnlyLiteralTypes   -- for literals
                        | GGTE Integer        -- for min/max size
                        | GOnlyTopLevel [Key] -- Weights to use at the toplevel
 deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty GenerateConstraint where
  pretty (GType x) = "GType" <+> pretty x
  pretty t = pretty . show $ t


-- To allow a Range Constant or a Range Expr
class WrapConstant a where
  wrapConstant :: Constant -> a

instance WrapConstant Expr where
  wrapConstant = ECon

instance WrapConstant Constant where
  wrapConstant = id

instance WrapConstant Expression where
  wrapConstant = Constant


data St = St{
      weighting  :: Map Key Int
    , depth      :: Int
    , beConstant :: Bool  -- when true only generate constrant expressions
    , newVars_   :: [Var] -- Domains from e.g. forall
    , doms_      :: Domains
    }
 deriving (Eq,Show, Data, Typeable, Generic)

instance Pretty St where
    pretty (St{..}) =
        "St" <+> Pr.braces (
            Pr.sep [
                    nn "depth"       $ pretty depth
                  , nn "beConstant " $ pretty beConstant
                  , nn "newVars_"    $ prettyTypeArr newVars_
                  , nn "doms"        $ pretty doms_
                  , nn "weighting" (pretty . groom $  weighting)
                  ] )


instance Default St where
  def = St{
          weighting  = def
        , depth      = $(neverNote "No depth specified")
        , beConstant = False
        , newVars_   = def
        , doms_      = def
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


runGenerate :: Generate a => GenerateConstraint -> St -> IO a
runGenerate con st = generate $ evalStateT (give con) st

-- No Logs at the moment
runGenerateWithLogs :: Generate a => St -> Gen (a,LogsTree)
runGenerateWithLogs st = do
  res <- evalStateT (give GNone) st
  return (res,LSEmpty)

-- | DataType of a proxy
proxyDataTypeOf :: forall a . Data a => Proxy a -> DataType
proxyDataTypeOf _ = dataTypeOf (error "proxyDataTypeOf" :: a)

-- | Keys of all fields in a datatype
fieldKeys :: Data a => Proxy a -> [Key]
fieldKeys a = do
  let names = dataTypeConstrs . proxyDataTypeOf $ a
  map (fromString . show) names

giveOnly :: forall a . (Data a, Generate a)
         => GenerateConstraint ->  [Key] -> GenSt a
giveOnly g keys = do
  let allowed = S.fromList keys
  let ws = [ (k,0) | k <- fieldKeys (Proxy :: Proxy a), k `S.notMember` allowed ]
  withWeights ws (give g)


-- | Generate n values of type a and print the frequency of them
generateFreq :: forall a . (Generate a, Pretty a, Ord a)
             => Proxy a -> GenerateConstraint -> Int -> St -> IO ()
generateFreq _ con n st = do
  let freq s = sort . map (\x->(length x,head x)) . group . sort $ s
  ts :: [a] <-  mapM  (\_ -> runGenerate con st)  [1..n]
  print .  vcat .  map pretty $ freq ts


-- | Generate n values of type a and print the frequency of them
generateTypeFreq :: forall a . (Generate a, Pretty a, Ord a, TTypeOf a)
                  => Proxy a -> GenerateConstraint -> Int -> St -> IO  ()
generateTypeFreq _ con n st = do
  let freq s = sort . map (\x->(length x,head x)) . group . sort $ s
  ts :: [a] <-  mapM  (\_ ->  runGenerate  con st)  [1..n]
  tys <- mapM ttypeOf ts
  print .  vcat .  map pretty $ freq tys
