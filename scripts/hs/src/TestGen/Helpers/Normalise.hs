{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module TestGen.Helpers.Normalise where

import TestGen.Helpers.StandardImports
import TestGen.Arbitrary.Data

import qualified Data.Traversable as V

class (Pretty a, Eq a, Show a) => Normalise a  where
    normalise :: (HasLogger m) => a -> m a

instance Normalise Type where
    normalise = return

instance Normalise Expr where

    normalise (ELit (EExpr (ELit y) )) = pure ELit    <*> normalise y

    normalise (ELit y)   = pure ELit    <*> normalise y
    normalise (EDom y)   = pure EDom    <*> normalise y

    normalise (EBinOp y) = pure EBinOp  <*> normalise y
    normalise (EUniOp y) = pure EUniOp  <*> normalise y
    normalise (EProc y)  = pure EProc   <*> normalise y

    normalise (EQuan y1 y2 y3 y4) = pure EQuan
                       <*> normalise y1
                       <*> normalise y2
                       <*> normalise y3
                       <*> normalise y4


    normalise x@(EVar _)    = return x
    normalise x@(EQVar _)   = return x
    normalise x@EEmptyGuard = return x

instance Normalise QType where
    normalise = return


instance Normalise BinOp where
    normalise (BIn t1 t2)        = pure BIn         <*> normalise t1 <*> normalise t2
    normalise (BOver t1 t2)      = pure BOver       <*> normalise t1 <*> normalise t2
    normalise (BEQ t1 t2)        = pure BEQ         <*> normalise t1 <*> normalise t2
    normalise (BNEQ t1 t2)       = pure BNEQ        <*> normalise t1 <*> normalise t2
    normalise (BLT t1 t2)        = pure BLT         <*> normalise t1 <*> normalise t2
    normalise (BLTE t1 t2)       = pure BLTE        <*> normalise t1 <*> normalise t2
    normalise (BGT t1 t2)        = pure BGT         <*> normalise t1 <*> normalise t2
    normalise (BGTE t1 t2)       = pure BGTE        <*> normalise t1 <*> normalise t2
    normalise (BDiff t1 t2)      = pure BDiff       <*> normalise t1 <*> normalise t2
    normalise (BPlus t1 t2)      = pure BPlus       <*> normalise t1 <*> normalise t2
    normalise (BMult t1 t2)      = pure BMult       <*> normalise t1 <*> normalise t2
    normalise (BDiv t1 t2)       = pure BDiv        <*> normalise t1 <*> normalise t2
    normalise (BPow t1 t2)       = pure BPow        <*> normalise t1 <*> normalise t2
    normalise (BMod t1 t2)       = pure BMod        <*> normalise t1 <*> normalise t2
    normalise (BAnd t1 t2)       = pure BAnd        <*> normalise t1 <*> normalise t2
    normalise (BOr t1 t2)        = pure BOr         <*> normalise t1 <*> normalise t2
    normalise (Bimply t1 t2)     = pure Bimply      <*> normalise t1 <*> normalise t2
    normalise (Biff t1 t2)       = pure Biff        <*> normalise t1 <*> normalise t2
    normalise (Bsubset t1 t2)    = pure Bsubset     <*> normalise t1 <*> normalise t2
    normalise (BsubsetEq t1 t2)  = pure BsupsetEq   <*> normalise t1 <*> normalise t2
    normalise (Bsupset t1 t2)    = pure Bsupset     <*> normalise t1 <*> normalise t2
    normalise (BsupsetEq t1 t2)  = pure BsupsetEq   <*> normalise t1 <*> normalise t2
    normalise (Bintersect t1 t2) = pure Bintersect  <*> normalise t1 <*> normalise t2
    normalise (Bunion t1 t2)     = pure Bunion      <*> normalise t1 <*> normalise t2
    normalise (BlexLT t1 t2)     = pure BlexLT      <*> normalise t1 <*> normalise t2
    normalise (BlexLTE t1 t2)    = pure BlexLTE     <*> normalise t1 <*> normalise t2
    normalise (BlexGT t1 t2)     = pure BlexGT      <*> normalise t1 <*> normalise t2
    normalise (BlexGTE t1 t2)    = pure BlexGTE     <*> normalise t1 <*> normalise t2

instance Normalise UniOp where
    normalise (UBar x) = pure UBar <*> normalise x
    normalise (UNeg x) = pure UNeg <*> normalise x

instance Normalise Proc where
    normalise (PallDiff x)         = pure PallDiff       <*> normalise x
    normalise (Pindex x1 x2)       = pure Pindex         <*> normalise x1 <*> normalise x2
    normalise (Papply x1 x2)       = pure Papply         <*> normalise x1
                                                         <*> mapM normalise x2
    normalise (Pfreq x1 x2)        = pure Pfreq          <*> normalise x1 <*> normalise x2
    normalise (Phist x1 x2)        = pure Phist          <*> normalise x1 <*> normalise x2
    normalise (Pmax x)             = pure Pmax           <*> normalise x
    normalise (Pmin x)             = pure Pmin           <*> normalise x
    normalise (PtoInt x)           = pure PtoInt         <*> normalise x
    normalise (PtoMSet x)          = pure PtoMSet        <*> normalise x
    normalise (PtoRelation x)      = pure PtoRelation    <*> normalise x
    normalise (PtoSet x)           = pure PtoSet         <*> normalise x
    normalise (Pdefined x)         = pure Pdefined       <*> normalise x
    normalise (Pimage x1 x2)       = pure Pimage         <*> normalise x1 <*> normalise x2
    normalise (Pinverse x1 x2)     = pure Pinverse       <*> normalise x1 <*> normalise x2
    normalise (PpreImage x1 x2)    = pure PpreImage      <*> normalise x1 <*> normalise x2
    normalise (Prange x)           = pure Prange         <*> normalise x
    normalise (Papart x1 x2 x3)    = pure Papart         <*> normalise x1 <*> normalise x2
                                                         <*> normalise x3
    normalise (Pparts x)           = pure Pparts         <*> normalise x
    normalise (Pparty x1 x2)       = pure Pparty         <*> normalise x1 <*> normalise x2
    normalise (Pparticipants x)    = pure Pparticipants  <*> normalise x
    normalise (Ptogether x1 x2 x3) = pure Ptogether      <*> normalise x1 <*> normalise x2
                                                         <*> normalise x3

instance Normalise Literal where
    normalise (EB x)          = return $ EB x
    normalise (EI x)          = return $ EI x

    normalise (ETuple xs)     = pure ETuple       <*> mapM normalise xs
    normalise (EMatrix x1 x2) = pure EMatrix      <*> mapM normalise x1 <*> normalise x2
    normalise (ESet x)        = pure ESet         <*> mapM normalise x
    normalise (EMSet x)       = pure EMSet        <*> mapM normalise x

    normalise (EFunction (xs)) = pure EFunction   <*> mapM normalise xs
    normalise (ERelation xs)   = pure ERelation   <*> mapM normalise xs
    normalise (EPartition xs)  = pure EPartition  <*> mapM nor xs
        where
          nor = mapM normalise

    normalise (EExpr (ELit l)) = normalise l
    normalise (EExpr x)        = pure EExpr <*> normalise x

instance Normalise (Literal,Literal) where
    normalise (x,y) = do
      a <- normalise x
      b <- normalise y
      return (a,b)

instance Normalise Domain where
    normalise x = return x  --FIXME when adding expr to domains


instance Normalise FG where
    normalise (Find x)  = pure Find  <*> normalise x
    normalise (Given x) = pure Given <*> normalise x

instance Normalise Doms where
    normalise = V.traverse normalise

instance Normalise SpecE where
    normalise (SpecE x1 x2) = pure SpecE <*> normalise x1 <*> mapM normalise x2
