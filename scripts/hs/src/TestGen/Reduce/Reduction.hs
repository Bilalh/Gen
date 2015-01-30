{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- module TestGen.Reduce.Reduction(Reduce(..), runReduce) where
module TestGen.Reduce.Reduction where

import TestGen.Reduce.Data
import TestGen.Reduce.Simpler

import TestGen.Prelude

import Data.List(transpose)

import Control.Monad.Trans.Identity(IdentityT)

-- import qualified TestGen.Arbitrary.Arbitrary as A
-- import qualified TestGen.Arbitrary.Domain as A
-- import qualified TestGen.Arbitrary.Expr as A

class (HasGen m, WithDoms m, HasLogger m) => Reduce a m where
    reduce   :: a -> m [a]    -- list of smaller exprs
    single   :: a -> m [Expr] -- smallest literal e.g  [true, false] for  a /\ b
    subterms :: a -> m [Expr] -- a /\ b  -->   [a, b]

    -- reduce a   = error "no default reduce"
    -- single a   = error "no default of single"
    -- subterms a = error "no default of subterms"


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce Expr m where

    reduce EEmptyGuard = return []

    reduce (ELit t) = do
      lits <- reduce t
      return $ map ELit lits

    reduce (EVar t) = typeOfVar t >>= \case
        Just ty -> singleLitExpr ty
        Nothing -> error . show . vcat $ ["Evar not found",pretty t]

    reduce (EQVar t) = typeOfVar t >>= \case
        Just ty -> singleLitExpr ty
        Nothing -> error . show . vcat $ ["Evar not found",pretty t]

    -- reduce (EUniOp t) = _h
    -- reduce (EProc t) = _h

    reduce (EDom t) = do
      ds <- reduce t
      return $ map EDom ds

    reduce (EBinOp op) = do
      -- single op ++ subterms op ++ map EBinOp (reduce op)
      a1 <- single op
      a2 <- subterms op
      a3 <- reduce op
      return $ a1 ++ a2 ++ (map EBinOp a3)

    reduce (ETyped t ex) = do
      exs <- reduce ex
      return $ map (ETyped t) exs

    -- reduce (EQuan t1 t2 t3 t4) = _h


    single EEmptyGuard = return []

    single (EVar t) = typeOfVar t >>= \case
        Just ty -> singleLitExpr ty
        Nothing -> error . show . vcat $ ["Evar not found",pretty t]

    single (EQVar t) = typeOfVar t >>= \case
        Just ty -> singleLitExpr ty
        Nothing -> error . show . vcat $ ["Evar not found",pretty t]

    single (ELit t)   = do
      addLog "singleELit" [nn "t" t]
      single t

    single (EDom t) = single t

    single (EBinOp t)    = single t
    -- single (EUniOp t) = _t
    -- single (EProc t)  = _t

    -- single (EQuan t1 t2 t3 t4) = _t

    single t = error . show .vcat $
               ["no single expr", pretty t, pretty $ groom t]

    subterms (EVar _)  = return []
    subterms (EQVar _) = return []

    subterms (ELit t)      = subterms t
    subterms (EDom t)      = subterms t
    subterms (EBinOp t)    = subterms t
    -- subterms (EUniOp t) = _h
    -- subterms (EProc t)  = _h

    -- subterms (EQuan t1 t2 t3 t4) = _h
    subterms EEmptyGuard = return []

    subterms t = error . show .vcat $
                 ["no single expr", pretty t, pretty $ groom t]

instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce Literal m where

    subterms _ = return []
    single (EExpr t) = single t
    single t = ttypeOf t >>= singleLitExpr

    reduce (EExpr t) = do
      es <- reduce t
      return $ map EExpr es

    reduce (EB _) = return []
    reduce (EI _) = return []

    reduce (ESet [])     = return []
    -- reduce (ESet [_])    = return [ESet []]
    reduce (ESet (t:ts)) = return [ESet [t], ESet ts ]

    reduce (EMSet [])     = return []
    -- reduce (EMSet [_])    = return [EMSet []]
    reduce (EMSet (t:ts)) = return [EMSet [t], EMSet ts ]

    -- FIXME indexes
    reduce (EMatrix [] _ )     = error "reduce empty matrix"
    reduce (EMatrix [a] d )    = do
      reduce a >>= \case
             [] -> return []
             xs -> do
               x <- oneofR xs
               return $ [EMatrix [x] d]

    reduce (EMatrix [a,_] d )  = return $ [EMatrix [a] d]
    reduce (EMatrix (t:ts) _ ) = do
      nts <- mapM (reduce) ts
      case filter (/= []) nts of
        [] -> return [ EMatrix [t] (dintRange 1 1)
                 -- , EMatrix [nts] (dintRange 1 (genericLength nts))
                 , EMatrix ts (dintRange 1 (genericLength ts))]

        nt -> do
          ns <- mapM oneofR nt
          return [ EMatrix [t] (dintRange 1 1)
                 , EMatrix ns (dintRange 1 (genericLength ns))
                 , EMatrix ts (dintRange 1 (genericLength ts))]


    reduce (ETuple t) = do
      ts <- mapM (reduce) t
      case ts of
        [] -> return []
        _  -> do
          let f l (xs,_) | length xs == l = xs
              f l (xs,r)                  = xs ++ (replicate (l - length xs ) r)

          let maxLength = maximum $ map length ts
              ts' = map (f maxLength) (zip ts t)
          -- -- FIXME shuffle?
          let ts'' = take 3 . filter ((==) (length t) . length) . transpose $ ts'
          return $ map ETuple ts''
          -- error . show . vcat $ [pretty maxLength
          --                       , pretty . groom $ ts
          --                       , pretty . groom $ ts']



    -- reduce (EFunction t) = _h
    -- reduce (ERelation t) = _h
    -- reduce (EPartition t) = _h


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce BinOp m where
    reduce (BOr x1 x2)    = reduceBop BOr x1 x2
    reduce (BAnd x1 x2)   = reduceBop BAnd x1 x2
    reduce (Bimply x1 x2) = reduceBop Bimply x1 x2
    reduce (Biff x1 x2)   = reduceBop Biff x1 x2

    reduce (BEQ x1 x2)  = reduceBop BEQ x1 x2
    reduce (BNEQ x1 x2) = reduceBop BNEQ x1 x2
    -- reduce (BLT x1 x2) = _h
    -- reduce (BLTE x1 x2) = _h
    -- reduce (BGT x1 x2) = _h
    -- reduce (BGTE x1 x2) = _h
    -- reduce (BDiff x1 x2) = _h
    -- reduce (BPlus x1 x2) = _h
    -- reduce (BMult x1 x2) = _h
    -- reduce (BDiv x1 x2) = _h
    -- reduce (BPow x1 x2) = _h
    -- reduce (BMod x1 x2) = _h
    -- reduce (Bsubset x1 x2) = _h
    -- reduce (BsubsetEq x1 x2) = _h
    -- reduce (Bsupset x1 x2) = _h
    -- reduce (BsupsetEq x1 x2) = _h
    -- reduce (Bintersect x1 x2) = _h
    -- reduce (Bunion x1 x2) = _h
    -- reduce (BlexLT x1 x2) = _h
    -- reduce (BlexLTE x1 x2) = _h
    -- reduce (BlexGT x1 x2) = _h
    -- reduce (BlexGTE x1 x2) = _h

    -- reduce (BIn x1 x2) = _h
    -- reduce (BOver x1 x2) = _h

    reduce a = error . show . vcat
        $ ["reduce missing case", pretty $ toEssence a, pretty $ groom a ]

    single (BAnd _ _)   = return $ [etrue,  efalse]
    single (BOr _ _)    = return $ [etrue,  efalse]
    single (Bimply _ _) = return $ [etrue,  efalse]
    single (Biff _ _)   = return $ [etrue,  efalse]

    single (BEQ _ _)  = return $ [etrue,  efalse]
    single (BNEQ _ _) = return $ [etrue,  efalse]

    single (BLT _ _)  = return $ [etrue,  efalse]
    single (BLTE _ _) = return $ [etrue,  efalse]
    single (BGT _ _)  = return $ [etrue,  efalse]
    single (BGTE _ _) = return $ [etrue,  efalse]

    single (Bsubset _ _)   = return $ [etrue,  efalse]
    single (BsubsetEq _ _) = return $ [etrue,  efalse]
    single (Bsupset _ _)   = return $ [etrue,  efalse]
    single (BsupsetEq _ _) = return $ [etrue,  efalse]

    single (BlexLT _ _)  = return $ [etrue,  efalse]
    single (BlexLTE _ _) = return $ [etrue,  efalse]
    single (BlexGT _ _)  = return $ [etrue,  efalse]
    single (BlexGTE _ _) = return $ [etrue,  efalse]

    single (BPlus x1 _) = ttypeOf x1 >>= singleLitExpr
    single (BMult x1 _) = ttypeOf x1 >>= singleLitExpr
    single (BDiv x1 _)  = ttypeOf x1 >>= singleLitExpr
    single (BPow x1 _)  = ttypeOf x1 >>= singleLitExpr
    single (BMod x1 _)  = ttypeOf x1 >>= singleLitExpr

    single (Bintersect x1 _) = ttypeOf x1 >>= singleLitExpr
    single (Bunion x1 _)     = ttypeOf x1 >>= singleLitExpr
    single (BDiff x1  _)     = ttypeOf x1 >>= singleLitExpr


    -- single (BIn x1 x2) = _e
    -- single (BOver x1 x2) = _e

    single a = error . show . vcat
        $ ["single missing case", pretty $ toEssence a, pretty $ groom a ]


    subterms (BAnd x1 x2)   = return [x1, x2]
    subterms (BOr x1 x2)    = return [x1, x2]
    subterms (Bimply x1 x2) = return [x1, x2]
    subterms (Biff x1 x2)   = return [x1, x2]

    subterms (BEQ x1 x2)  = subtermsBoolBop x1 x2
    subterms (BNEQ x1 x2) = subtermsBoolBop x1 x2

    subterms (BLT x1 x2)  = subtermsBoolBop x1 x2
    subterms (BLTE x1 x2) = subtermsBoolBop x1 x2
    subterms (BGT x1 x2)  = subtermsBoolBop x1 x2
    subterms (BGTE x1 x2) = subtermsBoolBop x1 x2

    subterms (Bsubset _ _)   = return []
    subterms (BsubsetEq _ _) = return []
    subterms (Bsupset _ _)   = return []
    subterms (BsupsetEq _ _) = return []

    subterms (BlexLT _ _)  = return []
    subterms (BlexLTE _ _) = return []
    subterms (BlexGT _ _)  = return []
    subterms (BlexGTE _ _) = return []


    subterms (BPlus x1 x2) = return [x1, x2]
    subterms (BMult x1 x2) = return [x1, x2]
    subterms (BDiv x1 x2)  = return [x1, x2]
    subterms (BPow x1 x2)  = return [x1, x2]
    subterms (BMod x1 x2)  = return [x1, x2]
    subterms (BDiff x1 x2) = return [x1, x2]

    subterms (Bintersect x1 x2) = return [x1, x2]
    subterms (Bunion x1 x2)     = return [x1, x2]

    -- subterms (BIn x1 x2) = _k
    -- subterms (BOver x1 x2) = _k


    subterms a = error . show . vcat
        $ ["subterms missing case", pretty $ toEssence a, pretty $ groom a ]


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce Domain m where
    reduce _   = return []
    single x   = return [EDom x]
    subterms _ = return []


subtermsBoolBop :: (WithDoms m) => Expr -> Expr ->  m [Expr]
subtermsBoolBop a b = ttypeOf a >>= \case
                      TBool -> return [a,b]
                      _     -> return []
--  src/TestGen/Reduce/Data.hs
reduceBop :: (WithDoms m, HasGen m, HasLogger m) =>
             (Expr -> Expr -> BinOp) -> Expr -> Expr -> m [BinOp]
reduceBop t a b=  do
  addLog "reduceBop" [nn "a" a, nn "b" b]
  fmap (  map (uncurry t) . catMaybes ) . sequence $
       [
         (a, )  -| (single b >>= logArr "reduceBopSingle 1" >>= oneofR, b)
       , (, b)  -| (single a >>= logArr "reduceBopSingle 2" >>= oneofR , a)
       , rr
       ]

   where
     rr :: (WithDoms m, HasGen m, HasLogger m) => m (Maybe (Expr, Expr))
     rr = do
       addLog "rr" []
       ra <- reduce a
       addLog "rr" [nn "ra" (vcat $ map pretty ra)]
       rb <- reduce b
       addLog "rr" [nn "rb" (vcat $ map pretty rb)]
       case (ra, rb) of
        ([], []) -> return Nothing
        ([], xs) -> do
            x <- oneofR xs
            return $ Just (a,x)
        (xs,[]) -> do
            x <- oneofR xs
            return $ Just (x,b)
        (as,bs) -> do
            na <- oneofR as
            nb <- oneofR bs
            return $ Just (na,nb)


infixl 1 -|
(-|) :: (Simpler a e, HasGen m, WithDoms m, HasLogger m) =>
        (a -> (c,d) ) -> (m a, e) -> m (Maybe ((c,d)))
f  -| (a,e) = do
   aa <-a
   simpler1 aa e >>= \case
     True  -> return $ Just (f aa)
     False -> return Nothing




-- | return the simplest literals, two at most
singleLit :: (HasGen m, HasLogger m, WithDoms m) => Type -> m [Expr]
singleLit TInt = do
  pure EI <*> chooseR (-5, 5) >>= return . (\a ->  [ELit a ] )

singleLit TBool = oneofR [EB True, EB False] >>= return . (\a ->  [ELit a ] )

-- of TAny are empty
singleLit (TMatix TAny) = error "singleLit TMatix TAny"
singleLit (TSet TAny)   = return [ELit $ ESet []]
singleLit (TMSet TAny)  = return [ELit $ EMSet []]

singleLit (TMatix x) = do
  s <- singleLit x
  let si = EMatrix (map (EExpr ) s) (dintRange 1 (genericLength s))
  let res = case s of -- Return the other combination
             []    -> error "singleLit empty matrix"
             [e]   -> [si,  EMatrix (map (EExpr ) [e,e]) (dintRange 1 2)]
             (e:_) -> [EMatrix [(EExpr ) e] (dintRange 1 1), si]

  return (map ELit res)

singleLit l@(TSet x) = do
  ty <- ttypeOf l
  let empty = ETyped ty (ELit $ ESet [])

  si <- pure ESet <*> (singleLit x >>= (return . map (EExpr)))
  return [ ELit si, empty]

singleLit l@(TMSet x) = do
  ty <- ttypeOf l
  let empty = ETyped ty $ (ELit $ EMSet [])

  si <- pure EMSet <*> (singleLit x >>= (return . map (EExpr)))
  d  <- (singleLit x >>= (return . map (EExpr) ))
  let dupped = EMSet $ concat $ replicate 2 d

  chosen <- oneofR [ELit si,ELit dupped]
  return [chosen, empty]

singleLit l@(TFunc x1 x2) = do
  ty <- ttypeOf l
  let empty = ETyped ty $ (ELit $ EFunction [])

  as <- singleLit x1 >>= (return . map (EExpr))
  bs <- singleLit x2 >>= (return . map (EExpr))
  let mu = EFunction (zip as bs)

  return [ ELit mu, empty]

singleLit (TTuple x) = do
  lits <- mapM singleLit x
  picked <- mapM oneofR lits
  return [ELit $ ETuple ( map EExpr picked)]

singleLit l@(TRel x) = do
  ty <- ttypeOf l
  let empty = ETyped ty  (ELit $ ERelation [])

  lits <- mapM singleLit x

  let minLength = minimum $ map length lits
      lits'     = map (take minLength) lits
      tuples    = map ETuple $ map (map EExpr) $ transpose lits'
      rel       = ELit $ ERelation $ map (EExpr . ELit)  tuples

  return [rel, empty]


singleLit l@(TPar x) = do
  ty <- ttypeOf l
  let empty = ETyped ty (ELit $ EPartition [])

  lits <- concatMapM (singleLit) [x,x]
  let lits' = take 3 $  nub2 lits

  -- Choose if all the elements should go in one part
  chooseR (True,False) >>= \case
          True  -> do
            let par = ELit $ EPartition [map EExpr lits]
            return $ [par, empty]
          False -> do
            point <- chooseR (0,length lits')
            let (as,bs) = splitAt point lits'
                par     = ELit $ EPartition [map EExpr  as, map EExpr bs]
            return $ [par, empty]


-- singleLit (TUnamed x) = _x
-- singleLit (TEnum x) = _x

singleLit TAny = rrError "singleLit of TAny" []
singleLit ty   = rrError "singleLit" [nn "ty" ty ]

singleLitExpr :: (HasGen m, HasLogger m, WithDoms m) => Type -> m [Expr]
singleLitExpr ty = do
  addLog "singleLitExpr" [nn "ty" ty]
  singleLit $ ty

runReduce :: (HasGen m, Standardise a, HasLogger m, Reduce a (StateT EState (IdentityT m)) )
             => SpecE -> a -> m [a]
runReduce spe x = do
  addLog "runReduce" []
  state <- (newEState spe)
  olg <- getLog
  (res,resState) <- runIdentityT $ flip runStateT state{elogs_=olg} $ do
                    nx <- standardise x
                    reduce nx

  putLog (elogs_ resState)
  addLog "endReduce" []
  return res
