{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, ConstraintKinds #-}
{-# LANGUAGE CPP #-}

module TestGen.Arbitrary.Data (
      Depth
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

import AST.Imports
import Language.E
import TestGen.Arbitrary.Helpers.Log

import Test.QuickCheck
import Data.Map(Map)
import Text.Groom

import qualified Data.Map as M
import qualified Text.PrettyPrint as Pr

import Control.Monad.State.Strict(StateT)

type GG a =  StateT SpecState Gen a

type Depth = Int
-- type GenM  a = State SpecState (Gen a)
type GenM m  = (MonadState SpecState m)


type Ref = Text

data SS = SS
    {
      depth_      :: Depth       --  how many levels to genrate
    , doms_       :: Map Text FG --  Domains
    , nextNum_    :: Int          -- Number to name next var
    , newVars_    :: [(Text,Type) ] -- Domains from e.g. forall
    , logs_       :: LogsTree
    , __lc        :: Int
    , beConstant_ :: Bool  -- when true only generate constrant expressions
    }
type SpecState=SS
_ss :: Depth -> SS
_ss d = SS{depth_=d, doms_ = M.empty, nextNum_=1
          ,newVars_=[], logs_=LSEmpty, __lc=0, beConstant_ =False }


prettyDepth :: GG Doc
prettyDepth  = do
    SS{depth_} <- get
    return $ "depth_ " <+> pretty depth_

instance Show SS where
    show (SS{..}) = show $
        "SS" <+> Pr.braces (
            Pr.sep [
                  "depth_ ="  <+> (pretty  depth_)
                , ",nextNum_ ="  <+>  (pretty nextNum_)
                , ",doms_ = "
                , pretty $ groom doms_
                , ",newVars_ = "
                , prettyArr newVars_
            ]
            )

instance Pretty SS where
    pretty (SS{..}) =
        "SS" <+> Pr.braces (
            Pr.sep [
                  "depth_ ="  <+> (pretty  depth_)
                , ",nextNum_ ="  <+>  (pretty nextNum_)
                , ",doms_ = "
                , pretty doms_
                , ",newVars_ = "
                , prettyArr newVars_
                , "__lc ="  <+> (pretty  __lc)
            ]
            )

prettyArr :: Pretty a => [a] -> Doc
prettyArr [] = "[]"
prettyArr vs = vcat $ map pretty vs

data Type =
      TInt
    | TBool
    | TMatix  Type
    | TSet    Type
    | TMSet   Type
    | TFunc   Type Type
    | TTuple  [Type]
    | TRel    [Type]
    | TPar    Type
    | TUnamed Text   -- each unamed type is unique
    | TEnum   Text   -- as are enums
    | TAny
    deriving(Show, Eq, Ord)

instance Pretty Type where
    pretty  =  pretty . groom

instance Pretty [Type] where
    pretty  =  pretty . groom

addLog :: String -> [Doc] ->  GG ()
addLog nm docs = do
    lc <- gets __lc
    -- case makeLog nm  ( ("__lc" <+> pretty lc) : docs) of
    case makeLog nm  docs of
        Nothing -> return ()
        Just l -> modify $ \st -> st{
            logs_ = LSMultiple (logs_ st) (LSSingle l),
            __lc   = lc + 1
        }
