{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Use deriving instance Show  instead of deriving Show
module AST.Expr where

import AST.ToEssence(ToEssence(..))
import AST.FromEssence(FromEssence(..))
import AST.Types
import {-# SOURCE #-} AST.Literal
import {-# SOURCE #-} AST.Domain


import Language.E


instance ToEssence BinOp where

    toEssence (BIn x y) = [eMake| &x' in &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BDiff x y) = [eMake| &x' - &y' |] where
            x' = toEssence x
            y' = toEssence y

    toEssence (BEQ x y) = [eMake| &x' = &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BNEQ x y) = [eMake| &x' != &y' |] where
        x' = toEssence x
        y' = toEssence y


    toEssence (BLT x y) = [eMake| &x' < &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BLTE x y) = [eMake| &x' <= &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BGT x y) = [eMake| &x' > &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BGTE x y) = [eMake| &x' >= &y' |] where
        x' = toEssence x
        y' = toEssence y


    toEssence (BPlus x y) = [eMake| &x' + &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BMult x y) = [eMake| &x' * &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BDiv x y) = [eMake| &x' / &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BPow x y) = [eMake| &x' ** &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BMod x y) = [eMake| &x' % &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BAnd x y) = [eMake| &x' /\ &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BOr x y) = [eMake| &x' \/ &y' |] where
        x' = toEssence x
        y' = toEssence y


    toEssence (Bimply x y) = [eMake| &x' -> &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (Biff x y) = [eMake| &x' <-> &y' |] where
        x' = toEssence x
        y' = toEssence y


    toEssence (Bsubset x y) = [eMake| &x' subset &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BsubsetEq x y) = [eMake| &x' subsetEq &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (Bsupset x y) = [eMake| &x' supset &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BsupsetEq x y) = [eMake| &x' supsetEq &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (Bintersect x y) = [eMake| &x' intersect &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (Bunion x y) = [eMake| &x' union &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BlexLT x y) = [eMake| &x' <lex &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BlexLTE x y) = [eMake| &x' <=lex &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BlexGT x y) = [eMake| &x' >lex &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BlexGTE x y) = [eMake| &x' >=lex &y' |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (BOver _ _ ) = error . show . vcat $ [
              "toEssence Bover missing case"
        ]

    -- toEssence missing = error . show . vcat $ [
    --           "toEssence BinOP missing case"
    --         ,pretty . show $ missing
    --     ]

instance ToEssence UniOp where
    toEssence (UBar x) = [eMake| |&x'| |] where
        x' = toEssence x
    toEssence (UNeg x) = [eMake| -&x' |] where
        x' = toEssence x

instance ToEssence Proc where

    toEssence (PallDiff x ) = [eMake| allDiff(&x') |] where
        x' = toEssence x

    toEssence (Pindex ref@(EVar _) c ) = [eMake| &ref'[&c']  |] where
        ref' = toEssence ref
        c'   = toEssence c

    toEssence (Papply ref@(EVar _) es ) =
        [xMake| functionApply.actual := [ref']
              | functionApply.args   := es' |]
        where
        ref' = toEssence ref
        es'  = map toEssence es


    toEssence (Pfreq x y) = [eMake| freq(&x', &y') |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (Phist x y) = [eMake| hist(&x', &y') |] where
        x' = toEssence x
        y' = toEssence y


    toEssence (Pmin x) = [eMake| min(&x') |] where
        x' = toEssence x

    toEssence (Pmax x) = [eMake| max(&x') |] where
        x' = toEssence x


    toEssence (PtoInt x) = [eMake| toInt(&x') |] where
        x' = toEssence x

    toEssence (PtoMSet x) = [eMake| toMset(&x') |] where
        x' = toEssence x

    toEssence (PtoRelation x) = [eMake| toRelation(&x') |] where
        x' = toEssence x

    toEssence (PtoSet x) = [eMake| toSet(&x') |] where
        x' = toEssence x


    toEssence (Pdefined x) = [eMake| defined(&x') |] where
        x' = toEssence x

    toEssence (Pimage x y) = [eMake| image(&x', &y') |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (Pinverse x y) = [eMake| inverse(&x', &y') |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (PpreImage x y) = [eMake| preImage(&x', &y') |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (Prange x) = [eMake| range(&x') |] where
        x' = toEssence x


    toEssence (Papart x y z) = [eMake| apart(&x', &y', &z') |] where
        x' = toEssence x
        y' = toEssence y
        z' = toEssence z

    toEssence (Pparts x) = [eMake| parts(&x') |] where
        x' = toEssence x

    toEssence (Pparty x y) = [eMake| party(&x', &y') |] where
        x' = toEssence x
        y' = toEssence y

    toEssence (Pparticipants x) = [eMake| participants(&x') |] where
        x' = toEssence x

    toEssence (Ptogether x y z) = [eMake| together(&x', &y', &z') |] where
        x' = toEssence x
        y' = toEssence y
        z' = toEssence z


    toEssence missing = error . show . vcat $ [
              "toEssence Proc missing case"
            ,pretty . show $ missing
        ]



instance ToEssence QType where
    toEssence ForAll = [xMake| reference := [Prim (S "forAll")] |]
    toEssence Exists = [xMake| reference := [Prim (S "exists")] |]
    toEssence Sum    = [xMake| reference := [Prim (S "sum")] |]



instance ToEssence Expr where
    toEssence (EVar n)   = [xMake| reference := [Prim (S n)] |]
    toEssence (EQVar n)  = [xMake| structural.single.reference := [Prim (S n)] |]
    toEssence (ELit lit) = toEssence lit
    toEssence (EBinOp x) = toEssence x
    toEssence (EUniOp x) = toEssence x
    toEssence (EProc x)  = toEssence x


    toEssence (EDom x)   = toEssence x

    toEssence (EQuan qt (BIn v dom) g b)  =
                [eMake| &qt' &v' in &dom' , &g' . &b' |] where
                qt'  = toEssence qt
                v'   = toEssence v
                dom' = toEssence dom
                g'   = toEssence g
                b'   = toEssence b

    toEssence (EQuan qt (BOver v dom) g b)=
                [eMake| &qt' &v' : &dom' , &g' . &b' |] where
                qt'  = toEssence qt
                v'   = toEssence v
                dom' = toEssence dom
                g'   = toEssence g
                b'   = toEssence b

    toEssence EEmptyGuard = Tagged (Tag "emptyGuard") []

    toEssence missing = error . show . vcat $ [
              "toEssence expr missing case"
            ,pretty . show $ missing
        ]


instance Pretty Expr where
    pretty =   pretty  . toEssence

instance Pretty BinOp where
    pretty =   pretty  . toEssence


instance FromEssence BinOp where
    fromEssence [eMatch| &x + &y |] = BPlus <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| &x - &y |] = do
        x' <- fromEssence x
        y' <- fromEssence y
        return $  BDiff x' y'

    fromEssence [eMatch| &x * &y |] = BMult <$> fromEssence x <*> fromEssence y

    fromEssence [eMatch| &x = &y |]  = BEQ  <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| &x != &y |] = BNEQ <$> fromEssence x <*> fromEssence y

    fromEssence [eMatch| &x /\ &y |]  = BAnd <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| &x \/ &y |]  = BOr  <$> fromEssence x <*> fromEssence y

    fromEssence [eMatch| &x < &y  |] = BLT  <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| &x <= &y |] = BLTE <$> fromEssence x <*> fromEssence y

    fromEssence [eMatch| &x >  &y |] = BGT  <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| &x >= &y |] = BGTE <$> fromEssence x <*> fromEssence y

    fromEssence x = Left x

instance FromEssence UniOp where
    fromEssence [eMatch| -&x |]  = UNeg <$> fromEssence x
    fromEssence [eMatch| |&x| |] = UBar <$> fromEssence x
    fromEssence x = Left x

instance FromEssence Proc where
    fromEssence [eMatch| &ele[&indexer] |] =
        Pindex <$> fromEssence ele <*> fromEssence indexer

    fromEssence [eMatch| toInt(&inner) |] =
        PtoInt <$> fromEssence inner

    fromEssence x = Left x

instance FromEssence QType where
    fromEssence [xMatch| [Prim (S "forAll")] := reference |] = return ForAll
    fromEssence [xMatch| [Prim (S "exist")] := reference |]  = return Exists
    fromEssence [xMatch| [Prim (S "sum")] := reference |]    = return Sum
    fromEssence x = Left x

instance FromEssence Expr where
    fromEssence [xMatch| [Prim (S n)] := reference |] = return $ EVar n
    fromEssence [xMatch| [Prim (S n)] := structural.single.reference |] = return $ EQVar n

    fromEssence x@[xMatch| _ := binOp |]    = EBinOp <$> fromEssence x
    fromEssence x@[xMatch| _ := unaryOp |]  = EUniOp <$> fromEssence x
    fromEssence x@[xMatch| _ := operator |] = EProc  <$> fromEssence x


    fromEssence [eMatch| &qt &qvar in &dom , &g . &b |] = do
        qt'   <- fromEssence qt
        qvar' <- fromEssence qvar
        dom'  <- fromEssence dom
        g'    <- fromEssence g
        b'    <- fromEssence b
        return $ EQuan qt' (BIn qvar' dom') g' b'

    fromEssence [eMatch| &qt &qvar : &dom , &g . &b |] = do
        qt'   <- fromEssence qt
        qvar' <- fromEssence qvar
        dom'  <- fromEssence dom
        g'    <- fromEssence g
        b'    <- fromEssence b
        return $ EQuan qt' (BOver qvar' dom') g' b'

    fromEssence (Tagged (Tag "emptyGuard") []) = return EEmptyGuard
    fromEssence x = case fromEssence x of
                                        Right l -> return $ ELit l
                                        Left l  -> Left l

instance FromEssence [Expr] where
        fromEssence [xMatch| xs := suchThat |] = mapM fromEssence xs
        fromEssence x = Left x
