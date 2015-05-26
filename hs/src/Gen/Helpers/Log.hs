{-# LANGUAGE CPP, DeriveGeneric, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Helpers.Log
    (
     makeLog
    , HasLogger(..)
    , LogNamed(..)
    , LogsTree(..)
    , Pretty(..)
    , addLog
    , addLogsTree
    , nullLogs
    , rrError
    , suppress
    ) where

#ifdef LOGS_TRACE
import Debug.Trace (trace)
#endif

import Gen.Imports

import qualified Data.DList       as DList
import qualified Data.HashSet     as S
import qualified GHC.Generics
import qualified Text.PrettyPrint as P

class (Monad m, Applicative m) => HasLogger m where
    getLog :: m LogsTree
    putLog :: LogsTree -> m ()

instance (Monad m, Functor m) => HasLogger (StateT () m)  where
    getLog   = return LSEmpty
    putLog _ = return ()

instance  HasLogger Identity  where
    getLog   = return LSEmpty
    putLog _ = return ()

instance  HasLogger IO where
    getLog   = return LSEmpty
    putLog x = putStrLn . show . pretty $ x

nullLogs :: forall (m :: * -> *) a. Monad m => StateT () m a -> m a
nullLogs f = evalStateT f ()


addLog :: HasLogger m => String -> [Doc] ->  m ()
-- addLog nm docs = return ()
addLog nm docs = do
    -- case makeLog nm  ( ("__lc" <+> pretty lc) : docs) of
    ls <- getLog
    case makeLog nm  docs of
        Nothing -> return ()
        Just l -> putLog $ LSMultiple ls (LSSingle l)

addLogsTree :: HasLogger m => LogsTree -> m ()
addLogsTree ls = do
  lg <- getLog
  let nlg = LSMultiple ls lg
  putLog nlg


rrError :: (HasLogger m)  => String -> [Doc] -> m a
rrError title docs = do
    lg <- getLog
    -- addLog "ggError" ["Last log"]
    error . show $ ( P.text $ padRight 15 ' ' title  )
        P.$+$ (nest 4 $ vcat (docs ))
        P.$+$ ""
#ifndef NO_GGERROR_LOGS
        P.$+$ nest 16 "==Logs=="
        P.$+$ (pretty (lg) )
#endif



data LogsTree = LSEmpty | LSSingle !LogNamed | LSMultiple !LogsTree !LogsTree
    deriving (Show, GHC.Generics.Generic)

instance Default LogsTree where
    def = LSEmpty

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
    ( show  $  (" ¦" <+> pretty (padRight 15 ' ' nm)) <+> (nest 4 $ vcat docs) )
    $ Just (LogNamed nm docs)
#else
makeLog nm _ | nm `S.member` suppress = Nothing
makeLog nm docs = Just (LogNamed nm docs)
#endif
