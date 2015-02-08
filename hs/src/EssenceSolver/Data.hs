{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
module EssenceSolver.Data where


import Data.Map(Map)

import qualified Text.PrettyPrint as P
import qualified Data.Map as M

import Data.Set(Set)
import qualified Data.Set as S

#ifdef TRACE_SOLVE
import Language.E hiding (trace)
import Debug.Trace(trace)
tracePretty :: [Doc] -> a -> a
traceHang :: Doc -> [Doc] -> a -> a

tracePretty ds =  trace (show $ vcat ds)
traceHang msg ds = trace ( show $ hang msg 4 (vcat ds) )
#else
import Language.E
tracePretty :: [Doc] -> a -> a
traceHang :: Doc -> [Doc] -> a -> a

tracePretty _ a  = a
traceHang _ _ a  = a
#endif


type Ref = E

type Env        = [(Text, E)]
type Constraint = E
type DomVals    = (Text, [E])
type DomVal     = (Text, E)

data Trie a =
      TSome Text [a] (Trie a)
    | TNone
    deriving(Show)

mkTrie :: [Text] -> [( Set Text, a )] -> Trie a
mkTrie [] _ = TNone
mkTrie (x:xs) cs =
    let
        (hasX,noX) = partition (\(set,_) ->  x `S.member`  set && S.size set == 1 ) cs
    in
        TSome x (map snd hasX)
            $ mkTrie xs (map (\(s,v) -> (x `S.delete` s, v) ) noX)

instance (Pretty a) => Pretty (Trie a) where
   pretty (TNone) = "TNone"
   pretty (TSome n as t) =
        hang ("TSome" <+> pretty (show n)) 4 (P.sep . punctuate "¸" $ map pretty as)
       <+> "\n" <+> P.parens (pretty t)

-- instance Pretty a => Pretty (Set a) where
--     pretty s = P.braces $ P.sep $ P.punctuate "," $ map pretty (S.toList s)


data SolverArgs = SolverArgs {
      sEssence :: Spec
    , sParam   :: Maybe Spec
    , sOutPath :: FilePath
    , sAllSolutions :: Bool
    }

data SolverState = SolverState {
      tDoms :: Map Text [E] -- contains all values left in the domain
    , tConstraints :: [E] -- All constraints from the spec
    , tVars :: Map Text E --  current assigned values
} deriving Show

instance Pretty SolverState where
    pretty SolverState{..} = hang "State" 4 $
       P.braces . vcat . punctuate "," $ [
               -- "tDoms"        <+> "=" <+> (vcat . map pretty $ M.toList tDoms)
               "tConstraints" <+> "=" <+> (vcat . map pretty $ tConstraints )
             , "tVars"        <+> "=" <+> (vcat . map pretty $ M.toList tVars )
       ]


prettyEnv :: Env -> Doc
prettyEnv [] = hang "" 4 $ "env: []"
prettyEnv vs = hang "" 4 $  "env:" <+> (vcat . map pretty $ vs)


