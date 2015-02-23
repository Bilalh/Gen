{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, ConstraintKinds #-}
{-# LANGUAGE RankNTypes, KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module TestGen.Arbitrary.Data (
      addLog
    , Depth
    , Generators(..)
    , GG
    , Pretty(..)
    , Ref
    , SpecState
    , SS(..)
    , TType(..)
    , FuncsNames(..)
    , HasLogger(..)
    , addLogsTree
    , prettyArr
    , nullLogs
    , EssenceMode(..)
    , EssenceConfig(..)
    ) where

import TestGen.Helpers.StandardImports as X
import TestGen.Helpers.Log


import qualified Text.PrettyPrint as Pr


type GG a =  StateT SpecState Gen a

type Depth = Int
type Ref = Text

data EssenceMode =
          TypeCheck_
        | Refine_
        | Solve_
  deriving (Show, Data, Typeable,Eq)

instance Default EssenceMode where
    def = Solve_

data SS = SS
    {
      depth_      :: Depth       --  how many levels to genrate
    , doms_       :: Domains
    , nextNum_    :: Int             -- Number to name next var
    , newVars_    :: [(Text,TType) ] -- Domains from e.g. forall
    , logs_       :: LogsTree
    , __lc        :: Int
    , beConstant_ :: Bool  -- when true only generate constrant expressions

    , generators_ :: Generators


    }

data EssenceConfig = EssenceConfig
      { outputDirectory_ :: FilePath
      , mode_            :: EssenceMode

      , totalTime_       :: Int
      , perSpecTime_     :: Int
      , size_            :: Int  -- should not be greater then 5
      , cores_           :: Int
      , seed_            :: Int

      , totalIsRealTime    :: Bool
      , deletePassing_     :: Bool
      , binariesDirectory_ :: Maybe FilePath
      , oldConjure_        :: Bool
      }

instance Default EssenceConfig where
    def = EssenceConfig
      { outputDirectory_ = error "EssenceConfig outputDirectory not set"
      , mode_            = Solve_

      , totalTime_   = error "EssenceConfig totalTime not set"
      , perSpecTime_ = error "EssenceConfig perSpecTime not set"
      , size_        = 4
      , cores_       = error "EssenceConfig cores_ not set"
      , seed_        = error "EssenceConfig seed_ not set"

      , totalIsRealTime    = True
      , deletePassing_     = False
      , binariesDirectory_ = Nothing
      , oldConjure_        = False
      }

type SpecState=SS

data Generators = Generators
    {
        gen_atype      :: GG TType
    ,   gen_dom        :: GG (Domainn Expr)
    ,   gen_useFunc    :: FuncsNames -> Bool
    }


instance Show SS where
    show (SS{..}) = show $
        "SS" <+> Pr.braces (
            Pr.sep [
                  "depth_ ="  <+> (pretty  depth_)
                , ",nextNum_ ="  <+>  (pretty nextNum_)
                , ",doms_ = "
                , pretty $ groom doms_
                , ",newVars_ = "
                , prettyTypeArr newVars_
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
                , prettyTypeArr newVars_
                , "__lc ="  <+> (pretty  __lc)
            ]
            )

prettyTypeArr :: [(Text,TType)] -> Doc
prettyTypeArr [] = "[]"
prettyTypeArr vs = vcat $ map (\(a,b) -> pretty (a, show b) ) vs

prettyArr :: Pretty a => [a] -> Doc
prettyArr [] = "[]"
prettyArr vs = vcat $ map pretty vs


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

class (Monad m, Applicative m) => HasLogger m where
    getLog :: m LogsTree
    putLog :: LogsTree -> m ()

instance HasLogger (StateT SpecState Gen)  where
    getLog = gets logs_
    putLog lg = modify $ \st -> st{ logs_=lg}

instance (Monad m, Functor m) => HasLogger (StateT () m)  where
    getLog   = return LSEmpty
    putLog _ = return ()

nullLogs :: forall (m :: * -> *) a. Monad m => StateT () m a -> m a
nullLogs f = evalStateT f ()


data FuncsNames  = AallDiff
    | Aindex
    | Aapply

    | Afreq
    | Ahist

    | Amax
    | Amin

    | AtoInt
    | AtoMSet
    | AtoRelation
    | AtoSet

    | Adefined
    | Aimage
    | Ainverse
    | ApreImage
    | Arange

    | Aapart
    | Aparts
    | Aparty
    | Aparticipants
    | Atogether
    | Aubar
    | Aelement
    | Aunion
    | Aintersect
    | Adiff
