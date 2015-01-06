{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

-- module TestGen.Reduce.Reduction(Reduce(..), runReduce) where
module TestGen.Reduce.Reduction where
    
import TestGen.Reduce.Data
import TestGen.Reduce.Simpler
 
import TestGen.Prelude

import Data.List(transpose)

    
-- import qualified TestGen.Arbitrary.Arbitrary as A
-- import qualified TestGen.Arbitrary.Domain as A
-- import qualified TestGen.Arbitrary.Expr as A

-- import qualified Data.Map as M
-- import qualified Test.QuickCheck as QC


class (HasGen m, WithDoms m) => Reduce a m where
    reduce   :: a -> m [a]    -- list of smaller exprs
    single   :: a -> m [Expr] -- smallest literal e.g  [true, false] for  a /\ b
    subterms :: a -> m [Expr] -- a /\ b  -->   [a, b]

    -- reduce a   = error "no default reduce"
    -- single a   = error "no default of single"
    -- subterms a = error "no default of subterms"

instance (HasGen m, WithDoms m) =>  Reduce Expr m where
    reduce (EBinOp op) = do
      -- single op ++ subterms op ++ map EBinOp (reduce op) 
      a1 <- single op
      a2 <- subterms op
      a3 <- reduce op 
      return $ a1 ++ a2 ++ (map EBinOp a3)
      
    
    reduce _   = return [] -- no reductions possible   
    single _   = error "no single expr"
    subterms _ = return [] 
    

instance (HasGen m, WithDoms m) =>  Reduce BinOp m where
    reduce (BOr x1 x2)    = return $ reduceBoolBop BOr x1 x2
    reduce (BAnd x1 x2)   = return $ reduceBoolBop BAnd x1 x2
    reduce (Bimply x1 x2) = return $ reduceBoolBop Bimply x1 x2
    reduce (Biff x1 x2)   = return $ reduceBoolBop Biff x1 x2

    -- reduce (BIn x1 x2) = _h
    -- reduce (BOver x1 x2) = _h
    -- reduce (BEQ x1 x2) = _h
    -- reduce (BNEQ x1 x2) = _h
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

    reduce _ = return $ []
    -- reduce a = error . show . vcat   
    --     $ ["reduce missing case", pretty $ toEssence a, pretty $ groom a ]
    
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
                             
    -- single (BDiff x1 x2) = _e
    -- single (BPlus x1 x2) = _e
    -- single (BMult x1 x2) = _e
    -- single (BDiv x1 x2) = _e
    -- single (BPow x1 x2) = _e
    -- single (BMod x1 x2) = _e

    -- single (Bintersect x1 x2) = _e
    -- single (Bunion x1 x2) = _e

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


reduceBoolBop :: (Expr -> Expr -> b) -> Expr -> Expr -> [b]
reduceBoolBop t a b= map ( uncurry t ) $  catMaybes
        [ (a, etrue) *| simpler etrue b , (a,efalse)  *| simpler efalse b
        , (etrue,b)  *| simpler etrue a , (efalse, b) *| simpler efalse a ]


subtermsBoolBop :: (WithDoms m) => Expr -> Expr ->  m [Expr]
subtermsBoolBop a b = ttypeOf a >>= \case
                      TBool -> return [a,b]
                      _     -> return []

                               
-- return the simplest literals
-- two at most
singleLit :: (HasGen m) => Type -> m [Literal]
singleLit TInt = do
  -- p <- chooseR (1,5)
  -- n <- chooseR (-5, -1)
  -- let nums = [0, p, n]
  -- return $ map ( EI ) nums
  pure EI <*> chooseR (-5, 5) >>= return . (: [])
  
singleLit TBool = oneofR [EB True, EB False] >>= return . (: [])

-- TODO empty matrix
singleLit (TMatix x) = do
  s <- singleLit x
  let si = EMatrix s (dintRange 1 (genericLength s))
  let res = case s of -- Return the other combination
             []    -> error "singleLit empty matrix"
             [e]   -> [si,  EMatrix [e,e] (dintRange 1 2)]
             (e:_) -> [EMatrix [e] (dintRange 1 1), si]
             
  return res
    
singleLit (TSet x) = do
  si <- pure ESet <*> singleLit x 
  return [ESet [], si]
  
singleLit (TMSet x) = do
  si <- pure ESet <*> singleLit x
  d <- singleLit x
  let dupped =ESet $ concat $ replicate 2 d
  chosen <- oneofR [si,dupped]
  return [ESet [], chosen ]  
                      
singleLit (TFunc x1 x2) = do
  let empty = EFunction []
  as <- singleLit x1
  bs <- singleLit x2
  let mu = EFunction (zip as bs)
  return [empty, mu]
              
singleLit (TTuple x) = do
  lits <- mapM singleLit x
  picked <- mapM oneofR lits
  return [ETuple picked]
  
singleLit (TRel x) = do
  let empty = ERelation []
  lits <- mapM singleLit x

  let minLength = minimum $ map length lits
      lits'     = map (take minLength) lits
      tuples    = map ETuple $ transpose lits'
  return [empty, ERelation tuples]
  
  
singleLit (TPar x) = do
  let empty = EPartition []
  lits <- concatMapM (singleLit) [x,x]
  let lits' = take 3 $  nub2 lits 
  chooseR (True,False) >>= \case
          True  -> return $ [empty, EPartition [lits] ]
          False -> do            
              point <- chooseR (0,length lits')
              let (as,bs) = splitAt point lits'
              return $ [empty, EPartition [as, bs]]
  

-- singleLit (TUnamed x) = _x
-- singleLit (TEnum x) = _x

singleLit TAny = error "singleLit of TAny"
singleLit _ = $notImplemented

              
runReduce spe x = do
  state <- newEState spe
  return $ runIdentity $ flip evalStateT state $ reduce x 
