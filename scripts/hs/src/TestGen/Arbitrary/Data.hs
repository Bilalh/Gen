{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module TestGen.Arbitrary.Data where

import Language.E
import Control.Monad.Trans.State.Strict(State)
import Test.QuickCheck
import Data.Map(Map)
import AST.Imports
import Text.Groom

import qualified Data.Map as M
import qualified Text.PrettyPrint as Pr


type Depth = Int
type GenM  a = State SpecState (Gen a)

data SS = SS
    {
      depth_   :: Depth       --  how many levels to genrate
    , doms_    :: Map Text FG --  Domains
    , nextNum_ :: Int          -- Number to name next var
    , newVars_  :: [(Text,Type) ] -- Domains from e.g. forall
    } deriving Show
type SpecState=SS
_ss :: Depth -> SS
_ss d = SS{depth_=d, doms_ = M.empty, nextNum_=1,newVars_=[] }


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

data Type =
      TInt
    | TBool
    | TMatix  Type
    | TSet    Type
    | TMSet   Type
    | TPar    Type
    | TRel    [Type]
    | TFunc   Type Type
    | TUnamed Text  -- each unamed type is unique
    | TEnum   Text -- as are enums
    | TAny
    deriving(Show, Eq, Ord)

instance Pretty Type where
    pretty  =  pretty . groom
