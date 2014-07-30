{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module EssenceSolver.Data where

import Language.E

import Data.Map(Map)

import qualified Text.PrettyPrint as P
import qualified Data.Map as M

data SolverArgs = SolverArgs {
      sEssence :: Spec
    , sParam   :: Maybe Spec
    , sOutPath :: FilePath
    }

data State = State {
      tDoms :: Map Text E -- contains all values left in the domain
    , tConstraints :: [E] -- All constraints from the spec
    , tVars :: Map Text E --  current assigned values
} deriving Show

instance Pretty State where
    pretty State{..} = hang "State" 4 $
       P.braces . vcat . punctuate "," $ [
               -- "tDoms"        <+> "=" <+> (vcat . map pretty $ M.toList tDoms)
               "tConstraints" <+> "=" <+> (vcat . map pretty $ tConstraints )
             , "tVars"        <+> "=" <+> (vcat . map pretty $ M.toList tVars )
       ]


