{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestGen.Helpers.Standardise(Standardise(..)) where

import TestGen.Helpers.StandardImports

import qualified Data.Traversable as V

class (Pretty a, Eq a, Show a) => Standardise a  where
    standardise :: (Monad m, Applicative m) => a -> m a

instance Pretty TType where
    pretty = pretty . show

instance Standardise TType where
    standardise = return

instance Standardise Expr where

    standardise (ELit (EExpr (ELit y) )) = pure ELit <*> standardise y
    -- When we have expr in domains
    -- standardise (EDom (DExpr (EDom y) )) = pure EDom <*> standardise y


    standardise (ELit y)   = pure ELit    <*> standardise y
    standardise (EDom y)   = pure EDom    <*> standardise y

    standardise (EBinOp y) = pure EBinOp  <*> standardise y
    standardise (EUniOp y) = pure EUniOp  <*> standardise y
    standardise (EProc y)  = pure EProc   <*> standardise y

    standardise (EQuan y1 y2 y3 y4) = pure EQuan
                       <*> standardise y1
                       <*> standardise y2
                       <*> standardise y3
                       <*> standardise y4

    standardise (ETyped x y ) = pure ETyped <*> standardise x <*> standardise y

    standardise x@(EVar _)    = return x
    standardise x@(EQVar _)   = return x
    standardise x@EEmptyGuard = return x


instance Standardise QType where
    standardise = return


instance Standardise BinOp where
    standardise (BIn t1 t2)        = pure BIn         <*> standardise t1 <*> standardise t2
    standardise (BOver t1 t2)      = pure BOver       <*> standardise t1 <*> standardise t2
    standardise (BEQ t1 t2)        = pure BEQ         <*> standardise t1 <*> standardise t2
    standardise (BNEQ t1 t2)       = pure BNEQ        <*> standardise t1 <*> standardise t2
    standardise (BLT t1 t2)        = pure BLT         <*> standardise t1 <*> standardise t2
    standardise (BLTE t1 t2)       = pure BLTE        <*> standardise t1 <*> standardise t2
    standardise (BGT t1 t2)        = pure BGT         <*> standardise t1 <*> standardise t2
    standardise (BGTE t1 t2)       = pure BGTE        <*> standardise t1 <*> standardise t2
    standardise (BDiff t1 t2)      = pure BDiff       <*> standardise t1 <*> standardise t2
    standardise (BPlus t1 t2)      = pure BPlus       <*> standardise t1 <*> standardise t2
    standardise (BMult t1 t2)      = pure BMult       <*> standardise t1 <*> standardise t2
    standardise (BDiv t1 t2)       = pure BDiv        <*> standardise t1 <*> standardise t2
    standardise (BPow t1 t2)       = pure BPow        <*> standardise t1 <*> standardise t2
    standardise (BMod t1 t2)       = pure BMod        <*> standardise t1 <*> standardise t2
    standardise (BAnd t1 t2)       = pure BAnd        <*> standardise t1 <*> standardise t2
    standardise (BOr t1 t2)        = pure BOr         <*> standardise t1 <*> standardise t2
    standardise (Bimply t1 t2)     = pure Bimply      <*> standardise t1 <*> standardise t2
    standardise (Biff t1 t2)       = pure Biff        <*> standardise t1 <*> standardise t2
    standardise (Bsubset t1 t2)    = pure Bsubset     <*> standardise t1 <*> standardise t2
    standardise (BsubsetEq t1 t2)  = pure BsupsetEq   <*> standardise t1 <*> standardise t2
    standardise (Bsupset t1 t2)    = pure Bsupset     <*> standardise t1 <*> standardise t2
    standardise (BsupsetEq t1 t2)  = pure BsupsetEq   <*> standardise t1 <*> standardise t2
    standardise (Bintersect t1 t2) = pure Bintersect  <*> standardise t1 <*> standardise t2
    standardise (Bunion t1 t2)     = pure Bunion      <*> standardise t1 <*> standardise t2
    standardise (BlexLT t1 t2)     = pure BlexLT      <*> standardise t1 <*> standardise t2
    standardise (BlexLTE t1 t2)    = pure BlexLTE     <*> standardise t1 <*> standardise t2
    standardise (BlexGT t1 t2)     = pure BlexGT      <*> standardise t1 <*> standardise t2
    standardise (BlexGTE t1 t2)    = pure BlexGTE     <*> standardise t1 <*> standardise t2

instance Standardise UniOp where
    standardise (UBar x) = pure UBar <*> standardise x
    standardise (UNeg x) = pure UNeg <*> standardise x

instance Standardise Proc where
    standardise (PallDiff x)         = pure PallDiff       <*> standardise x
    standardise (Pindex x1 x2)       = pure Pindex         <*> standardise x1 <*> standardise x2
    standardise (Papply x1 x2)       = pure Papply         <*> standardise x1
                                                         <*> mapM standardise x2
    standardise (Pfreq x1 x2)        = pure Pfreq          <*> standardise x1 <*> standardise x2
    standardise (Phist x1 x2)        = pure Phist          <*> standardise x1 <*> standardise x2
    standardise (Pmax x)             = pure Pmax           <*> standardise x
    standardise (Pmin x)             = pure Pmin           <*> standardise x
    standardise (PtoInt x)           = pure PtoInt         <*> standardise x
    standardise (PtoMSet x)          = pure PtoMSet        <*> standardise x
    standardise (PtoRelation x)      = pure PtoRelation    <*> standardise x
    standardise (PtoSet x)           = pure PtoSet         <*> standardise x
    standardise (Pdefined x)         = pure Pdefined       <*> standardise x
    standardise (Pimage x1 x2)       = pure Pimage         <*> standardise x1 <*> standardise x2
    standardise (Pinverse x1 x2)     = pure Pinverse       <*> standardise x1 <*> standardise x2
    standardise (PpreImage x1 x2)    = pure PpreImage      <*> standardise x1 <*> standardise x2
    standardise (Prange x)           = pure Prange         <*> standardise x
    standardise (Papart x1 x2 x3)    = pure Papart         <*> standardise x1 <*> standardise x2
                                                         <*> standardise x3
    standardise (Pparts x)           = pure Pparts         <*> standardise x
    standardise (Pparty x1 x2)       = pure Pparty         <*> standardise x1 <*> standardise x2
    standardise (Pparticipants x)    = pure Pparticipants  <*> standardise x
    standardise (Ptogether x1 x2 x3) = pure Ptogether      <*> standardise x1 <*> standardise x2
                                                         <*> standardise x3

instance Standardise Literal where
    standardise (EB x)          = return $ EB x
    standardise (EI x)          = return $ EI x

    standardise (ETuple xs)     = pure ETuple       <*> mapM standardise xs
    standardise (EMatrix x1 x2) = pure EMatrix      <*> mapM standardise x1 <*> standardise x2
    standardise (ESet x)        = pure ESet         <*> mapM standardise x
    standardise (EMSet x)       = pure EMSet        <*> mapM standardise x

    standardise (EFunction (xs)) = pure EFunction   <*> mapM standardise xs
    standardise (ERelation xs)   = pure ERelation   <*> mapM standardise xs
    standardise (EPartition xs)  = pure EPartition  <*> mapM nor xs
        where
          nor = mapM standardise

    standardise (EExpr (ELit l)) = standardise l
    standardise (EExpr x)        = pure EExpr <*> standardise x

instance Standardise DDomain where
    standardise x = return x  --FIXME when adding expr to domains


instance Standardise FG where
    standardise (FFind x)  = pure FFind  <*> standardise x
    standardise (GGiven x) = pure GGiven <*> standardise x

instance Standardise Doms where
    standardise = V.traverse standardise

instance Standardise SpecE where
    standardise (SpecE x1 x2 x3) = pure SpecE <*> standardise x1
                                              <*> mapM standardise x2
                                              <*> standardise x3

instance Standardise OObjective where
    standardise (Maximising x) = pure Maximising <*> standardise x
    standardise (Minimising x) = pure Minimising <*> standardise x

instance (Standardise a, Standardise b) =>  Standardise (a,b) where
    standardise (a,b) = do
      sa <- standardise a
      sb <- standardise b
      return (sa,sb)

instance (Standardise a, Standardise b, Standardise c)
    =>  Standardise (a,b,c) where
    standardise (a,b,c) = do
      sa <- standardise a
      sb <- standardise b
      sc <- standardise c
      return (sa,sb, sc)

instance Standardise a => Standardise (Maybe a) where
    standardise Nothing  = pure Nothing
    standardise (Just a) = pure Just <*> standardise a
