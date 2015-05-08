{-# LANGUAGE TemplateHaskell #-}
module Gen.Helpers.LineError (line,lineError,notHandled) where

import Language.Haskell.TH
import Conjure.Prelude
import Conjure.Language.Pretty
import qualified Text.PrettyPrint as Pr

-- Idea from http://stackoverflow.com/questions/13379356/

-- | Usage lineError $line [doc,doc]
lineError :: Line -> [Doc] -> a
lineError l ds = error . show . vcat $ pretty l : ds

-- | Usage notHandled $line "description" value
notHandled :: Pretty p => Line -> String -> p -> a
notHandled l s =
 error . show $ Pr.hang ("Not handled:" <+> pretty s ) 4 $
                 vcat [pretty l, pretty s]

type Line = String

line :: Q Exp
line = withFileLine [| p |]

-- This has to be top level function for some reason
p :: String -> String
p s = s

withFileLine :: Q Exp -> Q Exp
withFileLine f = do
    let loc = fileLine =<< location
    appE f loc

fileLine :: Loc -> Q Exp
fileLine loc = do
    let floc = formatLoc loc
    [| $(litE $ stringL floc) |]

formatLoc :: Loc -> String
formatLoc loc = let file = loc_filename loc
                    (lin, col) = loc_start loc
                in concat [file, ":", show lin, ":", show col]
