{-# LANGUAGE FlexibleContexts, FlexibleInstances, KindSignatures,
             MultiParamTypeClasses, ParallelListComp, PatternGuards, QuasiQuotes,
             TupleSections, RankNTypes #-}
module Gen.Reduce.Reduction where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Conjure.Language.Definition
import Conjure.Language.Expression.Op
import Data.Data
import Data.List                        (splitAt)
import Data.Maybe                       (fromJust)
import Gen.Arbitrary.Type               (typesUnify)
import Gen.AST.TH
import Gen.AST.Ops
import Gen.Prelude
import Gen.Reduce.Data
import Gen.Reduce.Simpler
import Gen.Reduce.Inners
import Data.Generics.Uniplate.Data
import Gen.Reduce.Inners

import qualified Data.Foldable as F
import qualified Data.Traversable as T

class (HasGen m, WithDoms m, HasLogger m) => Reduce a m where
    reduce   :: a -> m [a]    -- list of smaller exprs
    single   :: a -> m [Expr] -- smallest literal e.g  [true, false] for  a /\ b
    subterms :: a -> m [Expr] -- a /\ b  -->   [a, b]


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce Expr m where

    reduce EEmptyGuard       = return []
    reduce EMetaVar{}        = return []
    reduce (EVar (Var _ ty)) = singleLitExpr ty

    reduce (ETyped ty _) = do
      singleLitExpr ty


    reduce (EDom e) = do
      a1 <- single e
      a2 <- reduce e
      a3 <- subterms e
      return $ a1 ++ a3 ++ (map EDom a2)

    reduce (ECon e) = do
      a1 <- single e
      a2 <- reduce e
      a3 <- subterms e
      return $ a1 ++ a3 ++ (map ECon a2)

    reduce (ELit e) = do
      a1 <- single e
      a2 <- reduce e
      a3 <- subterms e
      return $ a1 ++ a3 ++ (map ELit a2)

    reduce (EOp e) = do
      a1 <- single e
      a2 <- reduce e
      a3 <- subterms e
      return $ nub2 $ a1 ++ a3 ++ (map EOp a2)


    reduce e@EQuan{} = do
        a1 <- single e
        -- a2 <- reduce _f
        a3 <- subterms e
        return $ a1 ++ a3



    single EEmptyGuard  = return []
    single (ECon t)     = single t
    single (ELit t)     = single t
    single (EOp t)      = single t
    single (EDom t)     = single t

    single (ETyped ty _)     = singleLitExpr ty
    single (EVar (Var _ ty)) = singleLitExpr ty
    single e@(EMetaVar _)    = rrError "single EMetaVar" [pretty e]



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

    single t = ttypeOf t >>= singleLitExpr

    subterms _ = return []
    reduce _   = return []


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce Literal m where
    single t   = ttypeOf t >>= singleLitExpr
    subterms x = return . map ELit .  innersExpand doSubs $ x

    reduce li = do
        rLits <- reduceAllChildren li
        return . innersExpand doSubs $ rLits

      where
      reduceAllChildren :: (Monad m, Applicative m, HasGen m, WithDoms m, HasLogger m)
                        => Literal  -> m Literal
      reduceAllChildren lit  = fmap (\(ELit l) -> l )  $  descendM f (ELit lit)
        where
          f e = do
            r <- reduce e
            oneofR r


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce (Domainn Expr) m where
    reduce _   = return []
    single x   = return [EDom x]
    subterms _ = return []


instance (HasGen m, WithDoms m, HasLogger m) =>  Reduce (Op Expr) m where

    single o = ttypeOf o >>= singleLitExpr

    -- Generic versions only works on things like [essencee| 1 ** 2 |]  i.e
    -- EOp (MkOpPow (OpPow (ECon (ConstantInt 1)) (ECon (ConstantInt 2))))
    -- where the sub terms are the direct child of the Op.
    -- It does not work on things like [essencee| 1 + 2 |]


    subterms e@[opp| &a + &b |]  = subterms_op e [a,b]
    subterms e@[opp| &a * &b |]  = subterms_op e [a,b]
    subterms e@[opp| &a /\ &b |] = subterms_op e [a,b]
    subterms e@[opp| &a \/ &b |] = subterms_op e [a,b]

    subterms e  =  do
      let subs = F.toList e
      subterms_op e subs


    reduce [opp| &a + &b |]  = reduce_op2 (\[c,d] ->  [opp| &c + &d |]) [a,b]
    reduce [opp| &a * &b |]  = reduce_op2 (\[c,d] ->  [opp| &c * &d |]) [a,b]
    reduce [opp| &a /\ &b |] = reduce_op2 (\[c,d] ->  [opp| &c /\ &d |]) [a,b]
    reduce [opp| &a \/ &b |] = reduce_op2 (\[c,d] ->  [opp| &c \/ &d |]) [a,b]

    reduce e = do
      let subs = F.toList e
      reduce_op e subs


subterms_op :: forall (m :: * -> *) a t.
               (TTypeOf t, TTypeOf a, Applicative m, Monad m) =>
               a -> [t] -> m [t]
subterms_op e subs =  do
  resType <- ttypeOf e
  tys <- mapM ttypeOf subs
  -- FIXME typesUnify or typesEqual?
  let allowed  = [ x | x<-subs | ty <- tys, typesUnify resType ty  ]
  return allowed

reduce_op :: forall (m :: * -> *).
             (HasGen m, WithDoms m, HasLogger m) =>
             Op Expr -> [Expr] -> m [Op Expr]
reduce_op x subs = reduce_op2 (_replaceOpChildren x) subs

reduce_op2 :: forall (m :: * -> *).
              (HasGen m, WithDoms m, HasLogger m) =>
              ([Expr] -> Op Expr) -> [Expr] -> m [Op Expr]
reduce_op2 f subs = do
  case subs of
    [a,b] -> reduceBop f a b
    _ -> return []


doSubs :: forall a. [a] -> [[a]]
doSubs xs =  heads_tails . init $ inits xs
  where
  heads_tails [] = []
  heads_tails (_:es) = [e | (e,i) <- zip es [0..], (i >= length es - 2) || (i < 2)  ]



reduceBop :: (WithDoms m, HasGen m, HasLogger m) =>
             ( [Expr] -> Op Expr) -> Expr -> Expr -> m [Op Expr]
reduceBop t a b=  do
  addLog "reduceBop" [nn "a" a, nn "b" b]
  singles <- fmap (  map (\(aa,bb) -> t [aa, bb] ) . catMaybes ) . sequence $
       [
         (a, )  -| (single b >>= logArr "reduceBopSingle 1" >>= oneofR, b)
       , (, b)  -| (single a >>= logArr "reduceBopSingle 2" >>= oneofR , a)

       ]

  reduced <- rr

  return $ singles ++ reduced

   where
     rr :: (WithDoms m, HasGen m, HasLogger m) => m ( [Op Expr] )
     rr = do
       addLog "rr" []
       ra <- reduce a
       addLog "rr" [nn "ra" (vcat $ map pretty ra)]
       rb <- reduce b
       addLog "rr" [nn "rb" (vcat $ map pretty rb)]
       case (ra, rb) of
        ([],[])   -> return $ []

        (as, bs) -> do
          xa <- giveVals a as
          xb <- giveVals b bs

          return [ t vs | vs <- sequence [xa,xb]
                 , or $ zipWith (\z1 z2 -> runIdentity $ simpler1 z1 z2) vs [a,b]]


     giveVals defaul []  = return [defaul]
     giveVals defaul [x] = return [x,defaul]
     giveVals _ (x:xs) = do
       x2 <- oneofR xs
       return [x,x2]


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

-- __depths :: forall t c'.
--             (DepthOf c', Pretty c', Ord c') =>
--             (t -> StateT EState Identity [c']) -> t -> IO ()
__depths :: forall c' c'1.
            (DepthOf c'1, DepthOf c', Pretty c'1, Pretty c', Ord c') =>
            (c'1 -> StateT EState Identity [c']) -> c'1 -> IO ()
__depths f ee = do
  let spe   :: Spec   = $never
      seed            = 32
      state :: EState = newEStateWithSeed seed spe
      res             = runIdentity $ flip evalStateT state $ f ee
  putStrLn . show . pretty . vcat .  map pretty .  sort . map (depthOf &&& id) $  res
  putStrLn "---"
  putStrLn . show . pretty . (depthOf &&& id) $ ee


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


--http://stackoverflow.com/questions/3015962/zipping-with-padding-in-haskell
data OneOrBoth a b = OneL a | OneR b | Both a b

class Align f where
  align :: (OneOrBoth a b -> c) -> f a -> f b -> f c

instance Align [] where
  align f []     []     = []
  align f (x:xs) []     = f (OneL x)   : align f xs []
  align f []     (y:ys) = f (OneR y)   : align f [] ys
  align f (x:xs) (y:ys) = f (Both x y) : align f xs ys

liftAlign2 f a b = align t
  where t (OneL l)   = f l b
        t (OneR r)   = f a r
        t (Both l r) = f l r

zipPad a b = liftAlign2 (,) a b

liftAlign3 f a b c xs ys = align t (zipPad a b xs ys)
  where t (OneL (x,y))   = f x y c
        t (OneR r)       = f a b r
        t (Both (x,y) r) = f x y r

zipPad3 a b c = liftAlign3 (,,) a b c
