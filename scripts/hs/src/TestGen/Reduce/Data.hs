{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Reduce.Data where

import TestGen.Prelude
import TestGen.Helpers.Runner(KindI, StatusI)

import System.Random
import System.Random.TF

import qualified Text.PrettyPrint as Pr

import Data.IntSet(IntSet)
import qualified Data.IntSet as IS

import Control.Monad.Trans.Identity(IdentityT)

etrue, efalse :: Expr
etrue  = ELit (EB True)
efalse = ELit (EB False)

type RR a = StateT RState IO a

-- | Check if the spec's hash is contained, (add it if it is not)
containHashAdd :: SpecE -> RR Bool
containHashAdd newE= do
  let newHash = hash (pretty newE)
  is <- gets hashes_
  case newHash `IS.member` is of
    True -> return True
    False -> do
        let is' = newHash `IS.insert` is
        modify (\st -> st{ hashes_=is'} )
        return False

data RState = RState
    { oErrKind_         :: KindI
    , oErrStatus_       :: StatusI
    , oErrEprime_       :: Maybe FilePath
    , mostReduced_      :: Maybe SpecE
    , mostReducedFP_    :: Maybe FilePath
    , outputdir_        :: FilePath
    , otherrrErrorsFound_ :: [FilePath]
    , rgen_             :: TFGen
    , hashes_           :: IntSet
    , rlogs_            :: LogsTree
    } deriving (Show)

instance Pretty RState where
    pretty RState{..} =
        "SS" <+> Pr.braces (
            Pr.sep
                [ nn "oErrKind_ = "  oErrKind_
                , nn "oErrStatus_ =" oErrStatus_
                , nn "oErrEprime_ =" oErrEprime_
                , nn "mostReduced_ =" mostReduced_
                , nn "mostReducedFP_ =" mostReducedFP_
                , nn "otherrrErrorsFound_ =" (vcat $ map pretty otherrrErrorsFound_)
                , nn "rgen_ =" (show rgen_)
                , nn "hashes_ =" (show hashes_)
                ])

instance Default RState where
    def =  RState{oErrKind_         = error "need oErrKind_"
                 ,oErrStatus_       = error "need oErrStatus_"
                 ,oErrEprime_       = Nothing
                 ,outputdir_        = error "need outputdir_"
                 ,mostReduced_      = Nothing
                 ,mostReducedFP_    = Nothing
                 ,otherrrErrorsFound_ = []
                 ,rgen_             = error "need rgen_"
                 ,hashes_           = IS.empty
                 ,rlogs_            = LSEmpty
                 }



infixl 1 *|
(*|) :: a -> Bool -> Maybe a
a  *| c | c = Just a
_  *| _    = Nothing


mkrGen :: Int -> TFGen
mkrGen = mkTFGen


class (Monad r, Applicative r) => HasGen r where
  getGen :: r TFGen
  putGen :: TFGen -> r ()

instance HasGen (StateT RState IO) where
  getGen = gets rgen_
  putGen g = modify $ \st -> st{rgen_=g }


chooseR :: (Random a, HasGen m) => (a,a) -> m a
chooseR ins = do
    rgen  <- getGen
    let (num,rgen') = randomR ins rgen
    putGen rgen'
    return num

-- | Randomly chooses one of the elements
oneofR :: (HasGen m, HasLogger m)  => [a] -> m a
oneofR [] = rrError "oneOfR used with empty list" []
oneofR gs = do
  ix <- chooseR (0,length gs - 1)
  return $ gs !! ix


data EState = EState
  { spec_  :: SpecE
  , sgen_  :: TFGen
  , elogs_ :: LogsTree
  }

type ES a = StateT EState Identity a

instance WithDoms (StateT EState Identity) where
  getSpecEWithDoms = gets spec_

instance HasGen (StateT EState Identity) where
  getGen   = gets sgen_
  putGen g = modify $ \st -> st{sgen_=g }

newEState :: HasGen m => SpecE -> m EState
newEState sp = do
  newSeed <- chooseR (0 :: Int ,2^(24:: Int) )
  return $ EState{spec_=sp,sgen_=mkrGen newSeed,elogs_=LSEmpty}


instance WithDoms (StateT SpecE Identity) where
  getSpecEWithDoms = get


instance HasGen (StateT TFGen Identity) where
  getGen  = get
  putGen  = put


instance HasLogger (StateT RState IO)  where
    getLog = gets rlogs_
    putLog lg = modify $ \st -> st{ rlogs_=lg}


instance HasLogger (StateT EState Identity)  where
    getLog = gets elogs_
    putLog lg = modify $ \st -> st{ elogs_=lg}


instance (HasLogger (StateT EState (IdentityT (StateT RState IO)))) where
    getLog = gets elogs_
    putLog lg = modify $ \st -> st{ elogs_=lg}

instance (HasGen (StateT EState (IdentityT (StateT RState IO)))) where
  getGen   = gets sgen_
  putGen g = modify $ \st -> st{sgen_=g }

instance (WithDoms (StateT EState (IdentityT (StateT RState IO)))) where
  getSpecEWithDoms = gets spec_
