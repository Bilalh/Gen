{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module TestGen.Reduce.Data where

import TestGen.Prelude
import TestGen.Helpers.Runner(KindI, StatusI)

import System.Random
import System.Random.TF

import qualified Text.PrettyPrint as Pr

etrue, efalse :: Expr
etrue  = ELit (EB True)
efalse = ELit (EB False)

type RR a = StateT RState IO a


data RState = RState
    { oErrKind_         :: KindI
    , oErrStatus_       :: StatusI
    , oErrEprime_       :: Maybe FilePath  
    , mostReduced_      :: Maybe SpecE
    , mostReducedFP_    :: Maybe FilePath
    , outputdir_        :: FilePath
    , otherErrorsFound_ :: [FilePath]
    , rgen_             :: TFGen
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
                , nn "otherErrorsFound_ =" (vcat $ map pretty otherErrorsFound_)
                , nn "rgen_ =" (show rgen_)
                ])

instance Default RState where
    def =  RState{oErrKind_         = error "need oErrKind_"
                 ,oErrStatus_       = error "need oErrStatus_"
                 ,oErrEprime_       = Nothing
                 ,outputdir_        = error "need outputdir_"
                 ,mostReduced_      = Nothing
                 ,mostReducedFP_    = Nothing
                 ,otherErrorsFound_ = []
                 ,rgen_             = error "need rgen_"
                 }


mkrGen :: Int -> TFGen
mkrGen = mkTFGen


class (Monad r, Applicative r) => HasGen r where
  getGen :: r TFGen
  putGen :: TFGen -> r ()

instance HasGen (StateT RState IO) where
  getGen = gets rgen_
  putGen g = modify $ \st -> st{rgen_=g }
  

rndRangeM :: (Random a, HasGen m) => (a,a) -> m a
rndRangeM ins = do
    rgen  <- getGen
    let (num,rgen') = randomR ins rgen
    putGen rgen'
    return num


infixl 1 *|
(*|) :: a -> Bool -> Maybe a
a  *| c | c = Just a
_  *| _    = Nothing
