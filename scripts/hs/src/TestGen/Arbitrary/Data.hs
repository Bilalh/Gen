{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, ConstraintKinds #-}
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
    , GG
    , addLog
    ) where

import Language.E
import Control.Monad.Trans.State.Strict(State)
import Test.QuickCheck
import Data.Map(Map)
import AST.Imports
import Text.Groom

import qualified Data.Map as M
import qualified Text.PrettyPrint as Pr

import Control.Monad.State.Strict(StateT,evalStateT,State)

import TestGen.Arbitrary.Debug as X
type GG a =  StateT SpecState Gen a

type Depth = Int
-- type GenM  a = State SpecState (Gen a)
type GenM m  = (MonadState SpecState m)


type Ref = Text

data SS = SS
    {
      depth_    :: Depth       --  how many levels to genrate
    , doms_     :: Map Text FG --  Domains
    , nextNum_  :: Int          -- Number to name next var
    , newVars_  :: [(Text,Type) ] -- Domains from e.g. forall
    , logs      :: [String]
    } deriving Show
type SpecState=SS
_ss :: Depth -> SS
_ss d = SS{depth_=d, doms_ = M.empty, nextNum_=1,newVars_=[], logs=[] }


prettyDepth :: SpecState -> Doc
prettyDepth SS{depth_} = "depth_ " <+> pretty depth_

addLog :: String -> GG ()
addLog s = modify (\m -> m{logs=s: logs m } )


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
