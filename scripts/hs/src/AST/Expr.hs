{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Use deriving instance Show  instead of deriving Show
module AST.Expr where

import AST.ToEssence(ToEssence(..))
import AST.FromEssence(FromEssence(..))
import AST.Data
import {-# SOURCE #-} AST.Literal()
import {-# SOURCE #-} AST.Domain()
import AST.Type()

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

    toEssence (Pindex inn c ) = [eMake| &inner'[&c']  |] where
        inner' = toEssence inn
        c'     = toEssence c


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

    toEssence (ETyped ty x) = [eMake| (&x' : `&ty'`) |]
        where
          x'   = toEssence x
          ty' = toEssence ty

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

instance ToEssence Objective where
    toEssence (Maximising x) = [xMake| topLevel.objective.maximising := [toEssence x] |]
    toEssence (Minimising x) = [xMake| topLevel.objective.minimising := [toEssence x] |]


instance Pretty Expr where
    pretty =   pretty  . toEssence

instance Pretty BinOp where
    pretty =   pretty  . toEssence

instance Pretty UniOp where
    pretty =   pretty  . toEssence

instance Pretty Proc where
    pretty =   pretty  . toEssence

instance Pretty QType where
    pretty =   pretty  . toEssence

instance Pretty Objective where
    pretty =   pretty  . toEssence

instance FromEssence BinOp where
  fromEssence [eMatch| &x in &y |]        = BIn        <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x - &y |]         = BDiff      <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x = &y |]         = BEQ        <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x != &y |]        = BNEQ       <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x < &y |]         = BLT        <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x <= &y |]        = BLTE       <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x > &y |]         = BGT        <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x >= &y |]        = BGTE       <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x + &y |]         = BPlus      <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x * &y |]         = BMult      <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x / &y |]         = BDiv       <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x ** &y |]        = BPow       <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x % &y |]         = BMod       <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x /\ &y |]        = BAnd       <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x \/ &y |]        = BOr        <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x -> &y |]        = Bimply     <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x <-> &y |]       = Biff       <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x subset &y |]    = Bsubset    <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x subsetEq &y |]  = BsubsetEq  <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x supset &y |]    = Bsupset    <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x supsetEq &y |]  = BsupsetEq  <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x intersect &y |] = Bintersect <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x union &y |]     = Bunion     <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x <lex &y |]      = BlexLT     <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x <=lex &y |]     = BlexLTE    <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x >lex &y |]      = BlexGT     <$> fromEssence x <*> fromEssence y
  fromEssence [eMatch| &x >=lex &y |]     = BlexGTE    <$> fromEssence x <*> fromEssence y

  fromEssence x = Left x

instance FromEssence UniOp where
    fromEssence [eMatch| -&x |]  = UNeg <$> fromEssence x
    fromEssence [eMatch| |&x| |] = UBar <$> fromEssence x
    fromEssence x = Left x

instance FromEssence Proc where
    fromEssence [eMatch| &ele[&indexer] |] = Pindex <$> fromEssence ele <*> fromEssence indexer

    fromEssence [eMatch| allDiff(&x) |]          = PallDiff     <$> fromEssence x
    fromEssence [eMatch| freq(&x, &y) |]         = Pfreq        <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| hist(&x, &y) |]         = Phist        <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| min(&x) |]              = Pmin         <$> fromEssence x
    fromEssence [eMatch| max(&x) |]              = Pmax         <$> fromEssence x
    fromEssence [eMatch| toInt(&x) |]            = PtoInt       <$> fromEssence x
    fromEssence [eMatch| toMset(&x) |]           = PtoMSet      <$> fromEssence x
    fromEssence [eMatch| toRelation(&x) |]       = PtoRelation  <$> fromEssence x
    fromEssence [eMatch| toSet(&x) |]            = PtoSet       <$> fromEssence x
    fromEssence [eMatch| defined(&x) |]          = Pdefined     <$> fromEssence x
    fromEssence [eMatch| image(&x, &y) |]        = Pimage       <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| inverse(&x, &y) |]      = Pinverse     <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| preImage(&x, &y) |]     = PpreImage    <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| range(&x) |]            = Prange       <$> fromEssence x
    fromEssence [eMatch| apart(&x, &y, &z) |]    = Papart       <$> fromEssence x <*> fromEssence y
                                                                <*> fromEssence z
    fromEssence [eMatch| parts(&x) |]            = Pparts       <$> fromEssence x
    fromEssence [eMatch| party(&x, &y) |]        = Pparty       <$> fromEssence x <*> fromEssence y
    fromEssence [eMatch| participants(&x) |]     = Pparticipants<$> fromEssence x
    fromEssence [eMatch| together(&x, &y, &z) |] = Ptogether    <$> fromEssence x <*> fromEssence y
                                                                <*> fromEssence z


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
    fromEssence x@[xMatch| _ := operator |] = f x where
                 f [eMatch| |&y| |] = EUniOp . UBar <$> fromEssence y
                 f _                = EProc   <$> fromEssence x


    fromEssence [xMatch| [l] := typed.left
                       | [r] := typed.right |] = ETyped <$> fromEssence r <*> fromEssence l


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
        (dom')  <- fromEssence dom
        g'    <- fromEssence g
        b'    <- fromEssence b
        return $ EQuan qt' (BOver qvar' dom') g' b'

    -- fromEssence [xMatch| [ref] := functionApply.actual
    --                    | es    := functionApply.args |] = do
    --     ref' <- fromEssence ref
    --     es'  <- mapM fromEssence es
    --     return $ EProc $ Papply ref' es'



    fromEssence (Tagged (Tag "emptyGuard") []) = return EEmptyGuard

    fromEssence d@[xMatch| _ := domain |] = EDom <$> fromEssence d
    fromEssence x = case fromEssence x of
                                        Right l -> return $ ELit l
                                        Left l  -> Left l

instance FromEssence [Expr] where
        fromEssence [xMatch| xs := suchThat |] = mapM fromEssence xs
        fromEssence x = Left x

instance FromEssence Objective where
    fromEssence [xMatch| [x] := topLevel.objective.maximising |] =
        Maximising <$> x'  where x' = fromEssence x
    fromEssence [xMatch| [x] := topLevel.objective.minimising |] =
        Minimising <$> x'  where x' = fromEssence x
    fromEssence x = Left x
