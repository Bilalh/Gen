{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Arbitrary.Helpers.Log(
    makeLog, LogsTree(..), LogNamed(..), Pretty(..), suppress
) where

import qualified GHC.Generics
import qualified Data.HashSet as S
import qualified Data.DList as DList
import Language.E
import qualified Text.PrettyPrint as P

data LogsTree = LSEmpty | LSSingle !LogNamed | LSMultiple !LogsTree !LogsTree
    deriving (Show, GHC.Generics.Generic)

logTreeToList :: LogsTree -> DList.DList LogNamed
logTreeToList LSEmpty          = DList.empty
logTreeToList (LSSingle   x  ) = DList.singleton x
logTreeToList (LSMultiple x y) = logTreeToList x `DList.append` logTreeToList y

data LogNamed = LogNamed String [Doc]
    deriving (Show, GHC.Generics.Generic)

instance Pretty LogNamed where
    pretty (LogNamed nm docs) = (P.text $ padRight 15 ' '  ('['  :nm ++ "]" ) ) <+>
        (nest 4 $ vcat docs)

instance Pretty LogsTree where
    pretty = id
           . vcat
           . map pretty
           . DList.toList
           . logTreeToList

suppress :: S.HashSet String
suppress = S.fromList
    [ "test"
    , "test2"
    , "withDepthDec"
    ]


makeLog :: String -> [Doc] -> Maybe LogNamed
#ifdef LOGS_TRACE
makeLog nm _ | nm `S.member` suppress = Nothing
makeLog nm docs  = trace
    ( show  $  (" ¦" <+> pretty (padRight 15 ' ' nm)) <+> (nest 4 $ vcat docs)
    $ Just (LogNamed nm docs)
#else
makeLog nm docs | nm `S.member` suppress = Nothing
makeLog nm docs = Just (LogNamed nm docs)
#endif
