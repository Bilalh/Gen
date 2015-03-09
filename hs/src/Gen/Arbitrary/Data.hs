
{-# LANGUAGE FlexibleInstances, ConstraintKinds #-}
{-# LANGUAGE RankNTypes, KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Gen.Arbitrary.Data (
      addLog
    , Depth
    , Generators(..)
    , GG
    , Pretty(..)
    , SpecState
    , SS(..)
    , TType(..)
    , FuncsNames(..)
    , HasLogger(..)
    , addLogsTree
    , prettyArr
    , nullLogs
    , ToolchainOutput(..)
    ) where

import Gen.Helpers.StandardImports as X
import Gen.Helpers.Log


import qualified Text.PrettyPrint as Pr

data ToolchainOutput =
    ToolchainScreen_
  | ToolchainFile_
  | ToolchainNull_
  deriving (Show, Data, Typeable, Eq)

instance Default ToolchainOutput where
    def = ToolchainScreen_

instance Pretty ToolchainOutput where
    pretty = pretty . show

type GG a =  StateT SpecState Gen a

type Depth = Int

data SS = SS
    {
      depth_      :: Depth       --  how many levels to genrate
    , doms_       :: Domains
    , nextNum_    :: Int             -- Number to name next var
    , newVars_    :: [Var] -- Domains from e.g. forall
    , logs_       :: LogsTree
    , __lc        :: Int
    , beConstant_ :: Bool  -- when true only generate constrant expressions

    , generators_ :: Generators


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

prettyTypeArr :: [Var] -> Doc
prettyTypeArr [] = "[]"
prettyTypeArr vs = vcat $ map (\(Var a b) -> pretty (a, show b) ) vs

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
