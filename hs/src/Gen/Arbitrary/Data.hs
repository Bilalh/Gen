{-# LANGUAGE CPP, ConstraintKinds, DeriveDataTypeable, FlexibleInstances,
             KindSignatures, RankNTypes #-}
module Gen.Arbitrary.Data (
      addLog
    , Depth
    , Generators(..)
    , GG
    , Pretty(..)
    , SpecState
    , SS(..)
    , Type(..)
    , FuncsNames(..)
    , HasLogger(..)
    , prettyArr
    ) where

import Gen.Helpers.Log
import Gen.Imports as X
import Test.QuickCheck(Gen)

import qualified Text.PrettyPrint as Pr


type GG a =  StateT SpecState Gen a


data SS = SS
    { depth_      :: Depth       --  how many levels to genrate
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
        gen_atype      :: GG Type
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

instance HasLogger (StateT SpecState Gen)  where
    getLog = gets logs_
    putLog lg = modify $ \st -> st{ logs_=lg}


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
