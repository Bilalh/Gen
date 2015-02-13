{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Use deriving instance Show  instead of deriving Show
module AST.Expr where

import Conjure.Prelude
import Conjure.Language.Pretty
import Conjure.Language.Definition

import AST.ToEssence(ToEssence(..))
import AST.FromEssence(FromEssence(..))
import AST.Data
import {-# SOURCE #-} AST.Literal()
import {-# SOURCE #-} AST.Domain()
import AST.Type()




instance ToEssence BinOp Expression where

    -- toEssence (BIn x y) = [essence| &x' in &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BDiff x y) = [essence| &x' - &y' |] where
    --         x' = toEssence x
    --         y' = toEssence y

    -- toEssence (BEQ x y) = [essence| &x' = &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BNEQ x y) = [essence| &x' != &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y


    -- toEssence (BLT x y) = [essence| &x' < &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BLTE x y) = [essence| &x' <= &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BGT x y) = [essence| &x' > &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BGTE x y) = [essence| &x' >= &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y


    -- toEssence (BPlus x y) = [essence| &x' + &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BMult x y) = [essence| &x' * &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BDiv x y) = [essence| &x' / &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BPow x y) = [essence| &x' ** &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BMod x y) = [essence| &x' % &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BAnd x y) = [essence| &x' /\ &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BOr x y) = [essence| &x' \/ &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y


    -- toEssence (Bimply x y) = [essence| &x' -> &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (Biff x y) = [essence| &x' <-> &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y


    -- toEssence (Bsubset x y) = [essence| &x' subset &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BsubsetEq x y) = [essence| &x' subsetEq &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (Bsupset x y) = [essence| &x' supset &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BsupsetEq x y) = [essence| &x' supsetEq &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (Bintersect x y) = [essence| &x' intersect &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (Bunion x y) = [essence| &x' union &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BlexLT x y) = [essence| &x' <lex &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BlexLTE x y) = [essence| &x' <=lex &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BlexGT x y) = [essence| &x' >lex &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BlexGTE x y) = [essence| &x' >=lex &y' |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (BOver _ _ ) = error . show . vcat $ [
    --           "toEssence Bover missing case"
    --     ]


instance ToEssence UniOp Expression where
    -- toEssence (UBar x) = [essence| |&x'| |] where
    --     x' = toEssence x
    -- toEssence (UNeg x) = [essence| -&x' |] where
    --     x' = toEssence x

instance ToEssence Proc Expression where

    -- toEssence (PallDiff x ) = [essence| allDiff(&x') |] where
    --     x' = toEssence x

    -- toEssence (Pindex ref@(EVar _) c ) = [essence| &ref'[&c']  |] where
    --     ref' = toEssence ref
    --     c'   = toEssence c

    -- toEssence (Pindex inn c ) = [essence| &inner'[&c']  |] where
    --     inner' = toEssence inn
    --     c'     = toEssence c


    -- toEssence (Papply ref@(EVar _) es ) =
    --     [xMake| functionApply.actual := [ref']
    --           | functionApply.args   := es' |]
    --     where
    --     ref' = toEssence ref
    --     es'  = map toEssence es


    -- toEssence (Pfreq x y) = [essence| freq(&x', &y') |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (Phist x y) = [essence| hist(&x', &y') |] where
    --     x' = toEssence x
    --     y' = toEssence y


    -- toEssence (Pmin x) = [essence| min(&x') |] where
    --     x' = toEssence x

    -- toEssence (Pmax x) = [essence| max(&x') |] where
    --     x' = toEssence x


    -- toEssence (PtoInt x) = [essence| toInt(&x') |] where
    --     x' = toEssence x

    -- toEssence (PtoMSet x) = [essence| toMset(&x') |] where
    --     x' = toEssence x

    -- toEssence (PtoRelation x) = [essence| toRelation(&x') |] where
    --     x' = toEssence x

    -- toEssence (PtoSet x) = [essence| toSet(&x') |] where
    --     x' = toEssence x


    -- toEssence (Pdefined x) = [essence| defined(&x') |] where
    --     x' = toEssence x

    -- toEssence (Pimage x y) = [essence| image(&x', &y') |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (Pinverse x y) = [essence| inverse(&x', &y') |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (PpreImage x y) = [essence| preImage(&x', &y') |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (Prange x) = [essence| range(&x') |] where
    --     x' = toEssence x


    -- toEssence (Papart x y z) = [essence| apart(&x', &y', &z') |] where
    --     x' = toEssence x
    --     y' = toEssence y
    --     z' = toEssence z

    -- toEssence (Pparts x) = [essence| parts(&x') |] where
    --     x' = toEssence x

    -- toEssence (Pparty x y) = [essence| party(&x', &y') |] where
    --     x' = toEssence x
    --     y' = toEssence y

    -- toEssence (Pparticipants x) = [essence| participants(&x') |] where
    --     x' = toEssence x

    -- toEssence (Ptogether x y z) = [essence| together(&x', &y', &z') |] where
    --     x' = toEssence x
    --     y' = toEssence y
    --     z' = toEssence z


    -- toEssence missing = error . show . vcat $ [
    --           "toEssence Proc missing case"
    --         ,pretty . show $ missing
    --     ]



instance ToEssence QType () where
    -- toEssence ForAll = [xMake| reference := [Prim (S "forAll")] |]
    -- toEssence Exists = [xMake| reference := [Prim (S "exists")] |]
    -- toEssence Sum    = [xMake| reference := [Prim (S "sum")] |]



instance ToEssence Expr Expression where
    -- toEssence (EVar n)   = [xMake| reference := [Prim (S n)] |]
    -- toEssence (EQVar n)  = [xMake| structural.single.reference := [Prim (S n)] |]
    -- toEssence (ELit lit) = toEssence lit
    -- toEssence (EBinOp x) = toEssence x
    -- toEssence (EUniOp x) = toEssence x
    -- toEssence (EProc x)  = toEssence x


    -- toEssence (EDom x)   = toEssence x

    -- toEssence (ETyped ty x) = [essence| (&x' : `&ty'`) |]
    --     where
    --       x'   = toEssence x
    --       ty' = toEssence ty

    -- toEssence (EQuan qt (BIn v dom) g b)  =
    --             [essence| &qt' &v' in &dom' , &g' . &b' |] where
    --             qt'  = toEssence qt
    --             v'   = toEssence v
    --             dom' = toEssence dom
    --             g'   = toEssence g
    --             b'   = toEssence b

    -- toEssence (EQuan qt (BOver v dom) g b)=
    --             [essence| &qt' &v' : &dom' , &g' . &b' |] where
    --             qt'  = toEssence qt
    --             v'   = toEssence v
    --             dom' = toEssence dom
    --             g'   = toEssence g
    --             b'   = toEssence b

    -- toEssence EEmptyGuard = Tagged (Tag "emptyGuard") []

    -- toEssence missing = error . show . vcat $ [
    --           "toEssence expr missing case"
    --         ,pretty . show $ missing
    --     ]

instance ToEssence OObjective Objective where
    -- toEssence (Maximising x) = [xMake| topLevel.objective.maximising := [toEssence x] |]
    -- toEssence (Minimising x) = [xMake| topLevel.objective.minimising := [toEssence x] |]


instance Pretty Expr where
    -- pretty =   pretty  . toEssence

instance Pretty BinOp where
    -- pretty =   pretty  . toEssence

instance Pretty UniOp where
    -- pretty =   pretty  . toEssence

instance Pretty Proc where
    -- pretty =   pretty  . toEssence

instance Pretty QType where
    -- pretty =   pretty  . toEssence

instance Pretty OObjective where
    -- pretty =   pretty  . toEssence

instance FromEssence Expression BinOp where
  -- fromEssence [essence| &x in &y |]        = BIn        <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x - &y |]         = BDiff      <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x = &y |]         = BEQ        <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x != &y |]        = BNEQ       <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x < &y |]         = BLT        <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x <= &y |]        = BLTE       <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x > &y |]         = BGT        <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x >= &y |]        = BGTE       <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x + &y |]         = BPlus      <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x * &y |]         = BMult      <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x / &y |]         = BDiv       <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x ** &y |]        = BPow       <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x % &y |]         = BMod       <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x /\ &y |]        = BAnd       <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x \/ &y |]        = BOr        <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x -> &y |]        = Bimply     <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x <-> &y |]       = Biff       <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x subset &y |]    = Bsubset    <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x subsetEq &y |]  = BsubsetEq  <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x supset &y |]    = Bsupset    <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x supsetEq &y |]  = BsupsetEq  <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x intersect &y |] = Bintersect <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x union &y |]     = Bunion     <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x <lex &y |]      = BlexLT     <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x <=lex &y |]     = BlexLTE    <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x >lex &y |]      = BlexGT     <$> fromEssence x <*> fromEssence y
  -- fromEssence [essence| &x >=lex &y |]     = BlexGTE    <$> fromEssence x <*> fromEssence y

  fromEssence x = Left x

instance FromEssence UniOp Expression where
    -- fromEssence [essence| -&x |]  = UNeg <$> fromEssence x
    -- fromEssence [essence| |&x| |] = UBar <$> fromEssence x
    -- fromEssence x = Left x

instance FromEssence Proc Expression where
    -- fromEssence [essence| &ele[&indexer] |] = Pindex <$> fromEssence ele <*> fromEssence indexer

    -- fromEssence [essence| allDiff(&x) |]          = PallDiff     <$> fromEssence x
    -- fromEssence [essence| freq(&x, &y) |]         = Pfreq        <$> fromEssence x <*> fromEssence y
    -- fromEssence [essence| hist(&x, &y) |]         = Phist        <$> fromEssence x <*> fromEssence y
    -- fromEssence [essence| min(&x) |]              = Pmin         <$> fromEssence x
    -- fromEssence [essence| max(&x) |]              = Pmax         <$> fromEssence x
    -- fromEssence [essence| toInt(&x) |]            = PtoInt       <$> fromEssence x
    -- fromEssence [essence| toMset(&x) |]           = PtoMSet      <$> fromEssence x
    -- fromEssence [essence| toRelation(&x) |]       = PtoRelation  <$> fromEssence x
    -- fromEssence [essence| toSet(&x) |]            = PtoSet       <$> fromEssence x
    -- fromEssence [essence| defined(&x) |]          = Pdefined     <$> fromEssence x
    -- fromEssence [essence| image(&x, &y) |]        = Pimage       <$> fromEssence x <*> fromEssence y
    -- fromEssence [essence| inverse(&x, &y) |]      = Pinverse     <$> fromEssence x <*> fromEssence y
    -- fromEssence [essence| preImage(&x, &y) |]     = PpreImage    <$> fromEssence x <*> fromEssence y
    -- fromEssence [essence| range(&x) |]            = Prange       <$> fromEssence x
    -- fromEssence [essence| apart(&x, &y, &z) |]    = Papart       <$> fromEssence x <*> fromEssence y
    --                                                             <*> fromEssence z
    -- fromEssence [essence| parts(&x) |]            = Pparts       <$> fromEssence x
    -- fromEssence [essence| party(&x, &y) |]        = Pparty       <$> fromEssence x <*> fromEssence y
    -- fromEssence [essence| participants(&x) |]     = Pparticipants<$> fromEssence x
    -- fromEssence [essence| together(&x, &y, &z) |] = Ptogether    <$> fromEssence x <*> fromEssence y
    --                                                             <*> fromEssence z


    -- fromEssence x = Left x

instance FromEssence () QType where
    -- fromEssence [xMatch| [Prim (S "forAll")] := reference |] = return ForAll
    -- fromEssence [xMatch| [Prim (S "exists")] := reference |]  = return Exists
    -- fromEssence [xMatch| [Prim (S "sum")] := reference |]    = return Sum
    -- fromEssence x = Left x

instance FromEssence Expression Expr where
    -- fromEssence [xMatch| [Prim (S n)] := reference |] = return $ EVar n
    -- fromEssence [xMatch| [Prim (S n)] := structural.single.reference |] = return $ EQVar n


    -- fromEssence x@[xMatch| _ := binOp |]    = EBinOp <$> fromEssence x
    -- fromEssence x@[xMatch| _ := unaryOp |]  = EUniOp <$> fromEssence x
    -- fromEssence x@[xMatch| _ := operator |] = f x where
    --              f [essence| |&y| |] = EUniOp . UBar <$> fromEssence y
    --              f _                = EProc   <$> fromEssence x


    -- fromEssence [xMatch| [l] := typed.left
    --                    | [r] := typed.right |] = ETyped <$> fromEssence r <*> fromEssence l


    -- fromEssence [essence| &qt &qvar in &dom , &g . &b |] = do
    --     qt'   <- fromEssence qt
    --     qvar' <- fromEssence qvar
    --     dom'  <- fromEssence dom
    --     g'    <- fromEssence g
    --     b'    <- fromEssence b
    --     return $ EQuan qt' (BIn qvar' dom') g' b'

    -- fromEssence [essence| &qt &qvar : &dom , &g . &b |] = do
    --     qt'   <- fromEssence qt
    --     qvar' <- fromEssence qvar
    --     (dom')  <- fromEssence dom
    --     g'    <- fromEssence g
    --     b'    <- fromEssence b
    --     return $ EQuan qt' (BOver qvar' dom') g' b'


    -- fromEssence x@[xMatch| _ := functionApply |] = EProc <$> fromEssence x
    -- fromEssence d@[xMatch| _ := domain |]        = EDom  <$> fromEssence d
    -- fromEssence x@[xMatch| _ := value |]         = ELit  <$> fromEssence x

    -- fromEssence (Tagged (Tag "emptyGuard") []) = return EEmptyGuard
    -- -- fromEssence x = error $ "expr: " ++ (groom $ (pretty &&& id) x)
    -- fromEssence x = Left x

instance FromEssence [Expression] [Expr]  where
        -- fromEssence [xMatch| xs := suchThat |] = mapM fromEssence xs
        -- fromEssence x = error $ "exprs: " ++ (groom $ (pretty &&& id) x)

instance FromEssence Objective OObjective where
    -- fromEssence [xMatch| [x] := topLevel.objective.maximising |] =
    --     Maximising <$> x'  where x' = fromEssence x
    -- fromEssence [xMatch| [x] := topLevel.objective.minimising |] =
    --     Minimising <$> x'  where x' = fromEssence x
    -- fromEssence x = error $ "obj: " ++ (groom $ (pretty &&& id) x)
