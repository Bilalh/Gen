{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             ParallelListComp #-}
module Gen.Essence.St
  ( module X
  , Generate(..)
  , St(..)
  , GenSt
  , GenerateConstraint(..)
  , WrapConstant(..)
  , weightingForKey
  , withWeights
  , getWeights
  , getPossibilities
  , withDepthDec
  , giveUnmatched
  , runGenerate
  , asProxyTypeOf
  , typeKeys
  ) where

import Conjure.Language.Definition (Constant, Expression (..))
import Data.Data                   hiding (Proxy)
import Data.Map                    (Map)
import Gen.Helpers.StandardImports
import Gen.Essence.Key as X

import qualified Data.Map as M


class Data a => Generate a where
  {-# MINIMAL give, possible | give, possiblePure  #-}

  -- | Return a random value of type a subject to the constraints
  give  :: GenerateConstraint -> GenSt a

  -- | Returns True if this op can be used with the specified return type
  possible :: (Applicative m, MonadState St m) => Proxy a -> Type -> m Bool
  possible a ty = do
      d <- gets depth
      return $ possiblePure a ty d

  -- | Convenience for a pure implementation, Never call this method
  possiblePure :: Proxy  a -> Type -> Depth -> Bool
  possiblePure = error "no default possiblePure"


  getId ::  Proxy a -> Key
  getId = fromString . dataTypeName . dataTypeOf . asProxyTypeOf (error "getID" :: a)

  getWeighting :: MonadState St m => Proxy a -> m Int
  getWeighting a = do
      let key = getId a
      gets weighting >>= \kv ->
          case key `M.lookup` kv  of
            Nothing  -> return 100
            (Just x) -> return x


type GenSt a = StateT St Gen a
type Depth = Int

data GenerateConstraint = GNone
                        | GType Type -- The resulting type
                        | GOnlyLiteralTypes -- for literals
 deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty GenerateConstraint where
  pretty (GType x) = "GType" <+> pretty x
  pretty t = pretty . show $ t


-- To allow a Constant Range, or Expr Range for eaxmple
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
                 -> [(Type -> GenSt Bool, (Key, v))]
                 -> GenSt [(Int, v)]
getPossibilities con vs = do
  mapM (doPossibilities con) vs

  where
  doPossibilities :: GenerateConstraint
                  -> (Type -> GenSt Bool, (Key, v))
                  -> GenSt (Int, v)
  doPossibilities (GType ty) (f,(k,v)) =
   f ty >>= \case
     False -> return (0,v)
     True  -> do
       w <- weightingForKey k
       return (w,v)

  doPossibilities _ (_,(k,v)) = do
   w <- weightingForKey k
   return (w,v)

withDepthDec :: GenSt a -> GenSt a
withDepthDec f = do
  oldDepth <- gets depth
  modify $ \st -> st{ depth = oldDepth - 1 }
  res <- f
  modify $ \st -> st{ depth = oldDepth }
  return res

giveUnmatched :: forall c a. Pretty a => Doc -> a -> c
giveUnmatched msg t = error . show . vcat $ ["Unmatched give" <+> msg
                                            , pretty . show $ t
                                            , pretty t]

runGenerate :: Generate a => St -> IO a
runGenerate st = generate $ evalStateT (give GNone) st

asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}

typeKeys :: [Key]
typeKeys = do
  let names = dataTypeConstrs . dataTypeOf $ TypeAny
  map (fromString . show) names
