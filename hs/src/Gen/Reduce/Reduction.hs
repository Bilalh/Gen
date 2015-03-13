{-# LANGUAGE FlexibleContexts, FlexibleInstances, KindSignatures,
             MultiParamTypeClasses, ParallelListComp, PatternGuards, QuasiQuotes,
             TupleSections #-}
module Gen.Reduce.Reduction where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Conjure.Language.Definition
import Conjure.Language.Expression.Op
import Data.Data
import Data.Generics.Biplate
import Data.List                        (splitAt)
import Data.Maybe                       (fromJust)
import Gen.Arbitrary.Type               (typesUnify)
import Gen.AST.TH
import Gen.Prelude
import Gen.Reduce.Data
import Gen.Reduce.Simpler
import Data.Generics.Uniplate.Data ()

import qualified Data.Foldable as F
import qualified Data.Traversable as T

class (HasGen m, WithDoms m, HasLogger m) => Reduce a m where
    reduce   :: a -> m [a]    -- list of smaller exprs
    single   :: a -> m [Expr] -- smallest literal e.g  [true, false] for  a /\ b
    subterms :: a -> m [Expr] -- a /\ b  -->   [a, b]


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce Expr m where

    reduce EEmptyGuard = return []

    reduce (EVar (Var _ ty)) = do
      singleLitExpr ty

    reduce (ETyped t ex) = do
      exs <- reduce ex
      return $ map (ETyped t) exs

    reduce a@(EQuan _ _ _ _ _) = single a

    reduce (EOp e) = do
      a1 <- single e
      a2 <- subterms e
      a3 <- reduce e
      return $ a1 ++ a2 ++ (map EOp a3)

    reduce (ECon e) = do
      a1 <- single e
      a2 <- subterms e
      a3 <- reduce e
      return $ a1 ++ a2 ++ (map ECon a3)

    single EEmptyGuard  = return []
    single (EDom t)     = single t
    single (EOp t)      = single t
    single (ETyped _ e) = single e
    single (ECon e)     = single e

    single e@(EMetaVar _)    = rrError "single EMetaVar" [pretty e]
    single (EVar (Var _ ty)) = singleLitExpr ty

    single (ELit t)   = do
      addLog "singleELit" [nn "t" t]
      single t


    single (EQuan Sum t2 to t3 t4) = do
      i1 <- singleLitExpr TInt >>= oneofR
      i2 <- singleLitExpr TInt >>= oneofR
      i3 <- singleLitExpr TInt >>= oneofR
      return [i1
            , EQuan Sum t2 to EEmptyGuard i2
            , EQuan Sum t2 to t3 i3
            , EQuan Sum t2 to EEmptyGuard t4]

    single (EQuan t1 t2 to t3 t4) = do
          return [EQuan t1 t2 to EEmptyGuard etrue
                 , EQuan t1 t2 to t3 etrue
                 , EQuan t1 t2 to EEmptyGuard t4]


    subterms (ELit t)     = subterms t
    subterms (EDom t)     = subterms t
    subterms (EOp t)      = subterms t
    subterms (ETyped _ t) = subterms t

    subterms (EVar _)          = return []
    subterms EEmptyGuard       = return []
    subterms (EQuan _ _ _ _ _) = return []
    subterms (ECon _)          = return []
    subterms (EMetaVar _)      = return []


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce Constant m where

    subterms _ = return []
    single t = ttypeOf t >>= singleLitExpr

    reduce (ConstantBool _) = return []
    reduce (ConstantInt _)  = return []


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce Literal m where

    subterms _ = return []
    single t = ttypeOf t >>= singleLitExpr

    reduce (AbsLitSet [])     = return []
    reduce (AbsLitSet (t:ts)) = return [AbsLitSet [t], AbsLitSet ts ]

    reduce (AbsLitMSet [])     = return []
    reduce (AbsLitMSet (t:ts)) = return [AbsLitMSet [t], AbsLitMSet ts ]

    -- FIXME indexes
    reduce (AbsLitMatrix _ [] )     = rrError "reduce empty matrix" []
    reduce (AbsLitMatrix d [a] )    = do
      reduce a >>= \case
             [] -> return []
             xs -> do
               x <- oneofR xs
               return $ [AbsLitMatrix d [x] ]

    reduce (AbsLitMatrix d [a,_] )  = return $ [AbsLitMatrix d [a] ]
    reduce (AbsLitMatrix _ (t:ts) ) = do
      nts <- mapM (reduce) ts
      case filter (/= []) nts of
        [] -> return [ AbsLitMatrix  (dintRange 1 1) [t]
                 -- , AbsLitMatrix [nts] (dintRange 1 (genericLength nts))
                 , AbsLitMatrix  (dintRange 1 (length ts)) ts]

        nt -> do
          ns <- mapM oneofR nt
          return [ AbsLitMatrix  (dintRange 1 1) [t]
                 , AbsLitMatrix  (dintRange 1 (genericLength ns)) ns
                 , AbsLitMatrix  (dintRange 1 (genericLength ts)) ts]


    reduce (AbsLitTuple t) = do
      ts <- mapM (reduce) t
      case ts of
        [] -> return []
        _  -> do
          let f l (xs,_) | length xs == l = xs
              f l (xs,r)                  = xs ++ (replicate (l - length xs ) r)

          let maxLength = maximum $ map length ts
              ts' = map (f maxLength) (zip ts t)
          -- -- TODo shuffle?
          let ts'' = take 3 . filter ((==) (length t) . length) . transpose $ ts'
          return $ map AbsLitTuple ts''
          -- error . show . vcat $ [pretty maxLength
          --                       , pretty . groom $ ts
          --                       , pretty . groom $ ts']



    reduce (AbsLitFunction _)  = return []
    reduce (AbsLitRelation _)  = return []
    reduce (AbsLitPartition _) = return []


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce (Op Expr) m where

    single o = ttypeOf o >>= singleLitExpr

    -- Generic versions only works on things like [essencee| 1 ** 2 |]  i.e
    -- EOp (MkOpPow (OpPow (ECon (ConstantInt 1)) (ECon (ConstantInt 2))))
    -- where the sub terms are the direct child of the Op.
    -- It does not work on things like [essencee| 1 + 2 |]


    subterms [opp| &a + &b |]  = return []
    subterms [opp| &a - &b |]  = return []
    subterms [opp| &a * &b |]  = return []
    subterms [opp| &a /\ &b |] = return []
    subterms [opp| &a \/ &b |] = return []

    subterms e  =  do
      resType <- ttypeOf e
      let subs = F.toList e
      tys <- mapM ttypeOf subs
      -- FIXME typesUnify or typesEqual?
      let allowed  = [ x | x<-subs | ty <- tys, typesUnify resType ty  ]
      return allowed


    reduce [opp| &a + &b |]  = return []
    reduce [opp| &a - &b |]  = return []
    reduce [opp| &a * &b |]  = return []
    reduce [opp| &a /\ &b |] = return []
    reduce [opp| &a \/ &b |] = return []

    reduce x = do
      let subs = F.toList x
      case subs of
        -- [a,b] -> reduceBop _d a b
        _ -> return []


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce (Domainn Expr) m where
    reduce _   = return []
    single x   = return [EDom x]
    subterms _ = return []


reduceBop :: (WithDoms m, HasGen m, HasLogger m) =>
             (Expr -> Expr -> Op Expr) -> Expr -> Expr -> m [Op Expr]
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
            ea <- oneofR as
            eb <- oneofR bs
            return $ Just (ea,eb)


infixl 1 -|
(-|) :: (Simpler a e, HasGen m, WithDoms m, HasLogger m) =>
        (a -> (c,d) ) -> (m a, e) -> m (Maybe ((c,d)))
f  -| (a,e) = do
   aa <-a
   simpler1 aa e >>= \case
     True  -> return $ Just (f aa)
     False -> return Nothing



-- | return the simplest literals, two at most
singleLit :: (HasGen m, HasLogger m, WithDoms m) => TType -> m [Expr]
singleLit TInt = do
  pure ConstantInt<*> chooseR (-5, 5) >>= return . (\a ->  [ECon a ] )

singleLit TBool = oneofR [ConstantBool True, ConstantBool False]
              >>= return . (\a ->  [ECon a ] )

-- of TAny are empty
singleLit (TMatix TAny) = rrError "singleLit TMatix TAny" []
singleLit (TSet TAny)   = return [ELit $ AbsLitSet []]
singleLit (TMSet TAny)  = return [ELit $ AbsLitMSet []]

singleLit (TMatix x) = do
  s <- singleLit x
  let si = AbsLitMatrix  (dintRange 1 (genericLength s)) s
  let res = case s of -- Return the other combination
             []    -> error "singleLit empty matrix"
             [e]   -> [si,  AbsLitMatrix (dintRange 1 2) ([e,e])]
             (e:_) -> [AbsLitMatrix (dintRange 1 1) [e], si]

  return (map ELit res)

singleLit l@(TSet x) = do
  ty <- ttypeOf l
  let empty = ETyped ty (ELit $ AbsLitSet [])

  si <- pure AbsLitSet <*> (singleLit x )
  return [ ELit si, empty]

singleLit l@(TMSet x) = do
  ty <- ttypeOf l
  let empty = ETyped ty $ (ELit $ AbsLitMSet [])

  si <- pure AbsLitMSet <*> (singleLit x)
  d  <- singleLit x
  let dupped = AbsLitMSet $ concat $ replicate 2 d

  chosen <- oneofR [ELit si,ELit dupped]
  return [chosen, empty]

singleLit l@(TFunc x1 x2) = do
  ty <- ttypeOf l
  let empty = ETyped ty $ (ELit $ AbsLitFunction [])

  as <- singleLit x1
  bs <- singleLit x2
  let mu = AbsLitFunction (zip as bs)

  return [ ELit mu, empty]

singleLit (TTuple x) = do
  lits <- mapM singleLit x
  picked <- mapM oneofR lits
  return [ELit $ AbsLitTuple picked]

singleLit l@(TRel x) = do
  ty <- ttypeOf l
  let empty = ETyped ty  (ELit $ AbsLitRelation [])

  lits <- mapM (singleLit) x

  let minLength = minimum $ map length lits
      lits'     = map (take minLength) lits
      rel       = ELit $ AbsLitRelation $  lits

  return [rel, empty]


singleLit l@(TPar x) = do
  ty <- ttypeOf l
  let empty = ETyped ty (ELit $ AbsLitPartition [])

  lits <- concatMapM (singleLit) [x,x]
  let lits' = take 3 $  nub2 lits

  -- Choose if all the elements should go in one part
  chooseR (True,False) >>= \case
          True  -> do
            let par = ELit $ AbsLitPartition [lits]
            return $ [par, empty]
          False -> do
            point <- chooseR (0,length lits')
            let (as,bs) = splitAt point lits'
                par     = ELit $ AbsLitPartition [as, bs]
            return $ [par, empty]


-- singleLit (TUnamed x) = _x
-- singleLit (TEnum x) = _x

singleLit TAny = rrError "singleLit of TAny" []
singleLit ty   = rrError "singleLit" [nn "ty" ty ]

singleLitExpr :: (HasGen m, HasLogger m, WithDoms m) => TType -> m [Expr]
singleLitExpr ty = do
  addLog "singleLitExpr" [nn "ty" ty]
  singleLit $ ty


runReduce :: (HasGen m, Standardise a, HasLogger m, Reduce a (StateT EState (IdentityT m)) )
             => Spec -> a -> m [a]
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


__run :: forall t a (t1 :: * -> *).
         (Pretty a, Foldable t1) =>
         (t -> StateT EState Identity (t1 a)) -> t -> IO (t1 a)
__run f ee = do
  let spe   :: Spec   = $never
      seed            = 32
      state :: EState = newEStateWithSeed seed spe
      res             = runIdentity $ flip evalStateT state $ f ee
  mapM_ (print  . pretty )  res
  return res




_replaceOpChildren :: Op Expr -> [Expr] -> Op Expr
_replaceOpChildren op news = fst . flip runState news $ f1 <$> T.mapM fff ch1
   where
     (ch1, f1) = biplate op
     fff _ = do
       (x:xs) <- get
       put xs
       return x

_replaceOpChildren_ex :: Op Expr
_replaceOpChildren_ex = _replaceOpChildren
  [opp| 8 ** 3  |]  [  [essencee| 4 |], [essencee| 2 |] ]
