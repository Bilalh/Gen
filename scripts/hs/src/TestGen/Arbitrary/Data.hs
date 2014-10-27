{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module TestGen.Arbitrary.Data where

import Language.E  hiding(trace)
import Control.Monad.Trans.State.Strict(State)
import Test.QuickCheck
import Data.Map(Map)
import AST.Imports
import Text.Groom

import qualified Data.Map as M
import qualified Text.PrettyPrint as Pr

import Debug.Trace(trace)

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
    | TRel    [Type] -- tuples
    | TPar    Type
    | TUnamed Text   -- each unamed type is unique
    | TEnum   Text   -- as are enums
    | TAny
    deriving(Show, Eq, Ord)

instance Pretty Type where
    pretty  =  pretty . groom

instance Pretty [Type] where
    pretty  =  pretty . groom

docError :: [Doc] -> a
docError = error . show . vcat

#ifdef TTRACE

tracer :: String -> [Doc] -> a -> a
tracer title docs =  trace
    ( show  $  (" ¦" <+> pretty (padRight 15 ' ' title)) <+> (nest 4 $ vcat docs)  )

tracet :: String -> [Doc] -> Bool
tracet title docs =  tracer title docs True
tracef :: String -> [Doc] -> Bool
tracef title docs =  tracer title docs False

#else

tracer :: String -> [Doc] -> a -> a
tracer _ _ a = a

tracet :: String -> [Doc] -> Bool
tracet _ _  = True
tracef :: String -> [Doc] -> Bool
tracef _ _ =  False

#endif
