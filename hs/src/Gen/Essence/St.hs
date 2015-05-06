{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             ParallelListComp #-}

module Gen.Essence.St where

import Conjure.Language.Definition (Constant, Expression (..))
import Conjure.Prelude
import Data.Data                   hiding (Proxy)
import Data.Map                    (Map)
import Gen.AST.Imports
import Gen.Helpers.Placeholders    (neverNote)
import Test.QuickCheck             (Gen, generate)
import Conjure.Language.Pretty

import qualified Data.Map as M

class Data a => Generate a where
  give  :: GenerateConstraint -> GenSt a

  possible :: (Applicative m, MonadState St m) => a -> TType -> m Bool
  possible a ty = do
      d <- gets depth
      return $ possiblePure a ty d
  possiblePure :: a -> TType -> Depth -> Bool
  possiblePure = error "no default possiblePure"
  {-# MINIMAL give, possible | give, possiblePure  #-}

  getId ::  a -> Key
  getId  = dataTypeName . dataTypeOf

  getWeighting :: MonadState St m => a -> m Int
  getWeighting a = do
      let key = getId a
      gets weighting >>= \kv ->
          case key `M.lookup` kv  of
            Nothing  -> return 100
            (Just x) -> return x


type GenSt a = StateT St Gen a
type Key   = String
type Depth = Int

data GenerateConstraint = GNone
                        | GType TType -- The resulting type
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
       weighting :: Map String Int
    ,  depth     :: Int
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

class OpInfo a where
  possibleOp :: a -> Depth -> TType -> Bool

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

-- getWeights but with
getPossibilities :: GenerateConstraint
                 -> [(TType -> GenSt Bool, (Key, v))]
                 -> GenSt [(Int, v)]
getPossibilities con vs = do
  mapM (doPossibilities con) vs

  where
  doPossibilities :: GenerateConstraint
                  -> (TType -> GenSt Bool, (Key, v))
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
