{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module TestGen.Arbitrary.Data (
     module X
    , Depth
    , GenM
    , Ref
    , SS(..)
    , SpecState
    , _ss
    , prettyDepth
    , Pretty(..)
    , Type(..)
    ) where

import Language.E
import Control.Monad.Trans.State.Strict(State)
import Test.QuickCheck
import Data.Map(Map)
import AST.Imports
import Text.Groom

import qualified Data.Map as M
import qualified Text.PrettyPrint as Pr

import TestGen.Arbitrary.Debug as X


type Depth = Int
type GenM  a = State SpecState (Gen a)
type Ref = Text

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

prettyDepth :: SpecState -> Doc
prettyDepth SS{depth_} = "depth_ " <+> pretty depth_


instance Pretty SS where
    pretty (SS{..}) =
        "SS" <+> Pr.braces (
            Pr.sep [
                  "depth_ ="  <+> (pretty  depth_)
                , ",nextNum_ ="  <+>  (pretty nextNum_)
                , ",doms_ = "
                , pretty doms_
                , ",newVars_ = "
                , vcat $ map pretty newVars_
            ]
            )

data Type =
      TInt
    | TBool
    | TMatix  Type
    | TSet    Type
    | TMSet   Type
    | TFunc   Type Type
    | TTuple  [Type]
    | TRel    [Type] --  tuples 
    | TPar    Type
    | TUnamed Text   -- each unamed type is unique
    | TEnum   Text   -- as are enums
    | TAny
    deriving(Show, Eq, Ord)

instance Pretty Type where
    pretty  =  pretty . groom

instance Pretty [Type] where
    pretty  =  pretty . groom
