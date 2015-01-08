{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}

module TestGen.Reduce.Simpler where

import TestGen.Prelude

import TestGen.Arbitrary.Type(typesUnify)

class (Pretty a, Eq a, Show a) => Normlise a  where
    normlise :: (HasLogger m) => a -> m a

instance Normlise Type where
    normlise = return

instance Normlise Expr where
    normlise (EDom y)   = pure EDom <*> normlise y
    normlise (ELit y)   = pure ELit <*> normlise y
    normlise (EBinOp y) = pure EBinOp <*> normlise y

    normlise (EUniOp y) = pure EUniOp <*> normlise y
    normlise (EProc y)  = pure EProc <*> normlise y

    normlise (EQuan y1 y2 y3 y4) = pure EQuan
                       <*> normlise y1
                       <*> normlise y2
                       <*> normlise y3
                       <*> normlise y4


    normlise x@(EVar _)    = return x
    normlise x@(EQVar _)   = return x
    normlise x@EEmptyGuard = return x

instance Normlise QType where
    normlise = return


instance Normlise BinOp where
    normlise (BIn t1 t2)        = pure BIn         <*> normlise t1 <*> normlise t2
    normlise (BOver t1 t2)      = pure BOver       <*> normlise t1 <*> normlise t2
    normlise (BEQ t1 t2)        = pure BEQ         <*> normlise t1 <*> normlise t2
    normlise (BNEQ t1 t2)       = pure BNEQ        <*> normlise t1 <*> normlise t2
    normlise (BLT t1 t2)        = pure BLT         <*> normlise t1 <*> normlise t2
    normlise (BLTE t1 t2)       = pure BLTE        <*> normlise t1 <*> normlise t2
    normlise (BGT t1 t2)        = pure BGT         <*> normlise t1 <*> normlise t2
    normlise (BGTE t1 t2)       = pure BGTE        <*> normlise t1 <*> normlise t2
    normlise (BDiff t1 t2)      = pure BDiff       <*> normlise t1 <*> normlise t2
    normlise (BPlus t1 t2)      = pure BPlus       <*> normlise t1 <*> normlise t2
    normlise (BMult t1 t2)      = pure BMult       <*> normlise t1 <*> normlise t2
    normlise (BDiv t1 t2)       = pure BDiv        <*> normlise t1 <*> normlise t2
    normlise (BPow t1 t2)       = pure BPow        <*> normlise t1 <*> normlise t2
    normlise (BMod t1 t2)       = pure BMod        <*> normlise t1 <*> normlise t2
    normlise (BAnd t1 t2)       = pure BAnd        <*> normlise t1 <*> normlise t2
    normlise (BOr t1 t2)        = pure BOr         <*> normlise t1 <*> normlise t2
    normlise (Bimply t1 t2)     = pure Bimply      <*> normlise t1 <*> normlise t2
    normlise (Biff t1 t2)       = pure Biff        <*> normlise t1 <*> normlise t2
    normlise (Bsubset t1 t2)    = pure Bsubset     <*> normlise t1 <*> normlise t2
    normlise (BsubsetEq t1 t2)  = pure BsupsetEq   <*> normlise t1 <*> normlise t2
    normlise (Bsupset t1 t2)    = pure Bsupset     <*> normlise t1 <*> normlise t2
    normlise (BsupsetEq t1 t2)  = pure BsupsetEq   <*> normlise t1 <*> normlise t2
    normlise (Bintersect t1 t2) = pure Bintersect  <*> normlise t1 <*> normlise t2
    normlise (Bunion t1 t2)     = pure Bunion      <*> normlise t1 <*> normlise t2
    normlise (BlexLT t1 t2)     = pure BlexLT      <*> normlise t1 <*> normlise t2
    normlise (BlexLTE t1 t2)    = pure BlexLTE     <*> normlise t1 <*> normlise t2
    normlise (BlexGT t1 t2)     = pure BlexGT      <*> normlise t1 <*> normlise t2
    normlise (BlexGTE t1 t2)    = pure BlexGTE     <*> normlise t1 <*> normlise t2

instance Normlise UniOp where
    normlise (UBar x) = pure UBar <*> normlise x
    normlise (UNeg x) = pure UNeg <*> normlise x

instance Normlise Proc where
    normlise (PallDiff x)         = pure PallDiff       <*> normlise x
    normlise (Pindex x1 x2)       = pure Pindex         <*> normlise x1 <*> normlise x2
    normlise (Papply x1 x2)       = pure Papply         <*> normlise x1 <*> mapM normlise x2
    normlise (Pfreq x1 x2)        = pure Pfreq          <*> normlise x1 <*> normlise x2
    normlise (Phist x1 x2)        = pure Phist          <*> normlise x1 <*> normlise x2
    normlise (Pmax x)             = pure Pmax           <*> normlise x
    normlise (Pmin x)             = pure Pmin           <*> normlise x
    normlise (PtoInt x)           = pure PtoInt         <*> normlise x
    normlise (PtoMSet x)          = pure PtoMSet        <*> normlise x
    normlise (PtoRelation x)      = pure PtoRelation    <*> normlise x
    normlise (PtoSet x)           = pure PtoSet         <*> normlise x
    normlise (Pdefined x)         = pure Pdefined       <*> normlise x
    normlise (Pimage x1 x2)       = pure Pimage         <*> normlise x1 <*> normlise x2
    normlise (Pinverse x1 x2)     = pure Pinverse       <*> normlise x1 <*> normlise x2
    normlise (PpreImage x1 x2)    = pure PpreImage      <*> normlise x1 <*> normlise x2
    normlise (Prange x)           = pure Prange         <*> normlise x
    normlise (Papart x1 x2 x3)    = pure Papart         <*> normlise x1 <*> normlise x2
                                                        <*> normlise x3
    normlise (Pparts x)           = pure Pparts         <*> normlise x
    normlise (Pparty x1 x2)       = pure Pparty         <*> normlise x1 <*> normlise x2
    normlise (Pparticipants x)    = pure Pparticipants  <*> normlise x
    normlise (Ptogether x1 x2 x3) = pure Ptogether      <*> normlise x1 <*> normlise x2
                                                        <*> normlise x3

instance Normlise Literal where
    normlise (EB x)          = return $ EB x
    normlise (EI x)          = return $ EI x

    normlise (ETuple xs)     = pure ETuple <*> mapM normlise xs
    normlise (EMatrix x1 x2) = pure EMatrix <*> mapM normlise x1 <*> normlise x2
    normlise (ESet x)        = pure ESet <*> mapM normlise x
    normlise (EMSet x)       = pure EMSet <*> mapM normlise x

    normlise (EFunction (xs)) = pure EFunction <*> mapM normlise xs
    normlise (ERelation xs)   = pure ERelation <*> mapM normlise xs
    normlise (EPartition xs)  = pure EPartition <*> mapM nor xs
        where
          nor = mapM normlise

    normlise (EExpr (ELit l)) = return l
    normlise (EExpr x)  = pure EExpr <*> normlise x

instance Normlise (Literal,Literal) where
    normlise (x,y) = do
      a <- normlise x
      b <-normlise y
      return (a,b)

instance Normlise Domain where
    normlise x = return x  --FIXME when adding expr to domains


-- True if a1 is simpler then a2
class (Pretty a, Eq a, Show a, Pretty b, Eq b, Show b, Normlise a, Normlise b) => Simpler a b where
    simplerImp :: (WithDoms m, HasLogger m) => a -> b -> m Bool
    simpler :: (WithDoms m, HasLogger m) => a -> b -> m Bool

    simpler a b = do
      -- addLog "simplerStart" [nn "a" a, nn "b" b]
      na <- normlise a
      nb <- normlise b
      res <- simplerImp na nb
      addLog "simpler" [nn "a" a, nn "b" b
                       , nn "res" res
                       , nn "ga" (groom a), nn "gb" (groom b)
                       , nn "ga" (groom na), nn "gb" (groom nb)
                       ]
      return res

instance Simpler Type Type where
    simplerImp TBool TBool = return False
    simplerImp TBool _     = return True

    simplerImp TInt TBool  = return False
    simplerImp TInt TInt   = return  False
    simplerImp TInt _      = return  True

    simplerImp (TSet x) (TSet y)     = simpler x y
    simplerImp (TMSet x) (TMSet y)   = simpler x y
    simplerImp (TMatix x) (TMatix y) = simpler x y
    simplerImp (TPar x) (TPar y)     = simpler x y

    simplerImp (TFunc x1 x2) (TFunc y1 y2) = do
        a <- simplerImp x1 x2
        b <- simplerImp y1 y2
        return $ a && b

    simplerImp (TTuple x) (TTuple y) = do
        res <- zipWithM simplerImp x y
        return $ and res

    simplerImp (TRel x) (TRel y) = do
        res <- zipWithM simplerImp x y
        return $ and res

    -- simplerImp (TUnamed x) y = _h
    -- simplerImp (TEnum x) y = _h
    simplerImp TAny TBool = return False
    simplerImp TAny TAny  = return False
    simplerImp TAny _     = return True

    simplerImp a b = rrError "simpler"
                  [pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]

instance Simpler Expr Expr where
    simplerImp (ELit a ) (ELit b)   = simpler a b
    simplerImp (ELit a)  (EBinOp b) = simpler a b

    simplerImp a@(EVar _) b = do
      tya <- ttypeOf a
      tyb <- ttypeOf b
      simplerImp tya tyb

    simplerImp a@(EQVar _) b = do
      tya <- ttypeOf a
      tyb <- ttypeOf b
      simplerImp tya tyb

    simplerImp (EBinOp a) (EBinOp b) = simpler a b

    -- simplerImp (EUniOp a) b =_h
    -- simplerImp (EProc a) b =_h
    -- simplerImp (EDom a) b =_h
    -- simplerImp (EQuan a1 a2 a3 a4) b =_h


    simplerImp EEmptyGuard EEmptyGuard = return False
    simplerImp EEmptyGuard b = do
      tyb <- ttypeOf b
      return $ tyb /= TBool


    -- simplerImp _ _ = False
    simplerImp a b = rrError "simpler"
                  [pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]


instance WithDoms m => TypeOf [Literal] m where
    ttypeOf []    = return TAny
    ttypeOf (x:_) = ttypeOf x

instance Simpler BinOp BinOp where
    -- simplerImp (BIn x1 x2) y        = _h
    -- simplerImp (BOver x1 x2) y      = _h

    simplerImp (BEQ x1 x2) (BEQ y1 y2)  = do
      r1 <- simplerImp x1 y1
      r2 <- simplerImp x2 y2
      return $ (r1 && r2 )
                 || (r1 && (x2 == y2) )
                 || (r2 && (x1 == y1) )

    simplerImp (BNEQ x1 x2) (BNEQ y1 y2) = do
      r1 <- simplerImp x1 y1
      r2 <- simplerImp x2 y2
      return $ (r1 && r2 )
                 || (r1 && (x2 == y2) )
                 || (r2 && (x1 == y1) )

    -- simplerImp (BLT x1 x2) y        = _h
    -- simplerImp (BLTE x1 x2) y       = _h
    -- simplerImp (BGT x1 x2) y        = _h
    -- simplerImp (BGTE x1 x2) y       = _h
    -- simplerImp (BDiff x1 x2) y      = _h
    -- simplerImp (BPlus x1 x2) y      = _h
    -- simplerImp (BMult x1 x2) y      = _h
    -- simplerImp (BDiv x1 x2) y       = _h
    -- simplerImp (BPow x1 x2) y       = _h
    -- simplerImp (BMod x1 x2) y       = _h
    -- simplerImp (BAnd x1 x2) y       = _h
    -- simplerImp (BOr x1 x2) y        = _h
    -- simplerImp (Bimply x1 x2) y     = _h
    -- simplerImp (Biff x1 x2) y       = _h
    -- simplerImp (Bsubset x1 x2) y    = _h
    -- simplerImp (BsubsetEq x1 x2) y  = _h
    -- simplerImp (Bsupset x1 x2) y    = _h
    -- simplerImp (BsupsetEq x1 x2) y  = _h
    -- simplerImp (Bintersect x1 x2) y = _h
    -- simplerImp (Bunion x1 x2) y     = _h
    -- simplerImp (BlexLT x1 x2) y     = _h
    -- simplerImp (BlexLTE x1 x2) y    = _h
    -- simplerImp (BlexGT x1 x2) y     = _h
    -- simplerImp (BlexGTE x1 x2) y    = _h


instance Simpler Literal Literal where
    simplerImp (EB _) (EB _) = return False
    simplerImp (EB _) _      = return True

    simplerImp (EI _) (EB _) = return False
    simplerImp (EI _) (EI _) = return False
    simplerImp (EI _) _      = return True

    simplerImp x (EExpr (ELit y)) =  simpler x y

    simplerImp (EExpr x) (EExpr y) =  simpler x y

    simplerImp (ETuple x) (ETuple y) = do
        res <- zipWithM simplerImp x y
        return $ and res

    simplerImp a@(EMatrix x _) b@(EMatrix y _) = do
      tx <- ttypeOf x
      ty <- ttypeOf y
      res <- zipWithM simplerImp x y

      let bo = (and res) || ( (typesUnify tx ty) && length x < length y )
      return bo

    simplerImp a@(ESet x) b@(ESet y)             =  do
        addLog "simplerImp" [nn "a" a, nn "b" b]
        res <- zipWithM simplerImp x y
        return $ and res

    simplerImp (EMSet x) (EMSet y)           =  do
        res <- zipWithM simplerImp x y
        return $ and res


    simplerImp (ERelation x) (ERelation y)   =  do
        res <- zipWithM simplerImp x y
        return $ and res

    -- simplerImp (EFunction x) (EFunction y)   =  and $ zipWith simpler x y
    -- simplerImp (EPartition x) (EPartition y) =  and $ zipWith simpler x y


    -- simplerImp _ _ = False
    simplerImp a b = rrError "simpler Missing case Literal"
                  [pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]

instance Simpler Literal BinOp where
    simplerImp (EB _) _ = return True
    simplerImp (EI _) _ = return True

    -- simplerImp _ _ = False
    simplerImp a b = rrError "simpler"
                  [pretty $ a, pretty $  b
                  , pretty $ groom a, pretty $ groom b ]
