{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module TestGen.Arbitrary.Data where

import Language.E
import Control.Monad.Trans.State.Strict(State)
import Test.QuickCheck
import Data.Map(Map)
import AST.Imports


import qualified Data.Map as M
import qualified Text.PrettyPrint as Pr


type Depth = Int
type GenM  a = State SpecState (Gen a)

data SS = SS
    {
      depth_   :: Depth       --  how many levels to genrate
    , doms_    :: Map Text FG --  Domains
    , nextNum_ :: Int          -- Number to name next var
    } deriving Show
type SpecState=SS
_ss :: Depth -> SS
_ss d = SS{depth_=d, doms_ = M.empty, nextNum_=1}


instance Pretty SS where
    pretty (SS{..}) =
        "SS" <+> Pr.braces (
            Pr.sep [
                  "depth_ ="  <+> (pretty  depth_)
                , ",nextNum_ ="  <+>  (pretty nextNum_)
                , ",doms_ = "
                , pretty doms_
            ]
            )