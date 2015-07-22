{-# LANGUAGE KindSignatures, MultiParamTypeClasses, PatternGuards,
             QuasiQuotes, RankNTypes, TupleSections #-}
module Gen.Reduce.Reduction where

import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Zipper    (Zipper, fromZipper, hole, replaceHole,
                                         zipperBi)
import Data.List                        (splitAt)
import Gen.AST.TH
import Gen.Helpers.Log
import Gen.Helpers.SizeOf
import Gen.Helpers.TypeOf
import Gen.Imports
import Gen.Reduce.Data
import Gen.Reduce.Inners
import Gen.Reduce.Simpler

import qualified Data.Foldable                 as F
import qualified Data.Generics.Uniplate.Zipper as Zipper
import qualified Data.Traversable              as T


class (HasGen m,  HasLogger m,  Simpler a a) => Reduce a m where
    reduce   :: a -> m [a]    -- list of smaller exprs
    single   :: a -> m [Expr] -- smallest literal e.g  [true, false] for  a /\ b

    subterms :: a -> m [Expr]
      -- replace the term with one it's children
      -- a /\ b   -->  [a, b]
      -- [a, b,c] -->  [], [a], [a,b]

    mutate :: a -> m [Expr] --
       -- Try type specific changes
       -- e.g partitions/relation reducing the number of items in each part

    mutate  _ = return []

    reduceChecks :: a -> [a] -> m [a]
    reduceChecks a rs = do
      return $ filter (\x -> runIdentity $ simpler1 x a) rs



instance (HasGen m,  HasLogger m) =>  Reduce Type m where
    reduce _   = return []
    single _   = $(neverNote "Reduce Type m ~ single called ")
    subterms _ = return []

instance (HasGen m,  HasLogger m) =>  Reduce Expr m where

    reduce EEmptyGuard       = return []
    reduce EMetaVar{}        = return []
    reduce (EVar (Var _ ty)) = singleLitExpr ty

    reduce (ETyped _ (ECon (ConstantBool _))) = return []
    reduce (ETyped _ (ECon (ConstantInt _)))  = return []
    reduce (ETyped _ (ECon (ConstantAbstract x))) | isLitEmpty x = return []
    reduce (ETyped _ (ELit x)) | isLitEmpty x= return []
    reduce (ETyped ty _) = do
      singleLitExpr ty


    reduce x@(EDom e) = do
      a1 <- single e
      a2 <- reduce e
      a3 <- subterms e
      reduceChecks x $ a1 ++ a3 ++ (map EDom a2)

    reduce (ECon ConstantBool{}) = return []
    reduce (ECon ConstantInt{})  = return []
    reduce x@(ECon e) = do
      a1 <- single e
      a2 <- reduce e
      a3 <- subterms e
      reduceChecks x $ a1 ++ a3 ++ (map ECon a2)

    reduce (ELit e) | isLitEmpty e = return []
    reduce x@(ELit e) = do
      a1 <- single e
      a3 <- subterms e
      a2 <- reduce e
      a4 <- mutate e
      reduceChecks x $ a1 ++ a3 ++ (map ELit a2) ++ a4

    reduce x@(EOp e) = do
      a1 <- single e
      a2 <- reduce e
      a3 <- subterms e
      reduceChecks x $ a1 ++ a3 ++ (map EOp a2)


    reduce e@EQuan{} = do
        a1 <- single e
        a3 <- subterms e
        reduceChecks e $ a1 ++ a3

    reduce e@(EComp inner gens cons) = do
      sin     <- single e
      subs    <- subterms e
      r_cons  <- reduceList cons
      r_inner <- reduce inner
      let res = concat [  if null r_cons then [EComp i gens []]
                          else [EComp i gens cs | cs <- [] : r_cons ]
                       |  i <- r_inner  ]

      addLog  $line [ ]
      addLog "reduce EComp" [pretty e]
      addLog "single" (map pretty sin)
      addLog "subterms" (map pretty subs)
      addLog "r_cons" (map prettyArr r_cons)
      addLog "r_inner" (map pretty r_inner)
      addLog "r_res" (map pretty res)
      addLog "r_lengths" [ nn "res"  (length res)
                       , nn "r_cons"  (length r_cons)
                       , nn "r_inner"  (length r_inner)
                       , nn "single"  (length sin)
                       , nn "subterms"  (length subs)
                       ]
      reduceChecks e  $ sin ++ subs ++ res


    single EEmptyGuard  = return []
    single (ECon t)     = single t
    single (ELit t)     = single t
    single (EOp t)      = single t
    single (EDom t)     = single t

    single (ETyped ty _)     = singleLitExpr ty
    single (EVar (Var _ ty)) = singleLitExpr ty
    single e@(EMetaVar _)    = rrError "single EMetaVar" [pretty e]

    single (EComp inner gens _) = do
      r_inner <- single inner
      return $ map (\i -> EComp i gens []) r_inner



    single (EQuan Sum t2 to t3 t4) = do
      i1 <- singleLitExpr TypeInt >>= oneofR
      i2 <- singleLitExpr TypeInt >>= oneofR
      i3 <- singleLitExpr TypeInt >>= oneofR
      return [i1
            , EQuan Sum t2 to EEmptyGuard i2
            , EQuan Sum t2 to t3 i3
            , EQuan Sum t2 to EEmptyGuard t4]

    single (EQuan t1 t2 to t3 t4) = do
          return [EQuan t1 t2 to EEmptyGuard (ECon $ ConstantBool True)
                 , EQuan t1 t2 to t3 (ECon $ ConstantBool True)
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

    subterms (EComp inners gens cons) = do
      let l_cs = [] : reduceLength cons
      return $ [ EComp inners gens c | c <- l_cs ]


-- Making Reduce (AbsLiteral c) is a lot of work
-- so a basic version for Reduce (AbsLiteral Constant)
instance (HasGen m,  HasLogger m) =>  Reduce Constant m where
    single t = ttypeOf t >>= singleLitExpr

    subterms (ConstantAbstract AbsLitTuple{}) = return []
    subterms (ConstantAbstract x) = do
      return . map (ECon . ConstantAbstract) . innersExpand reduceLength $ x
    subterms _ = return []

    -- Don't try to reduce empty literals
    reduce (ConstantAbstract li) | isLitEmpty li = return []

    -- FIXME finish?
    reduce (ConstantAbstract AbsLitTuple{}) =  return []
    reduce (ConstantAbstract _)             =  return []
    reduce _                                =  return []

    mutate (ConstantAbstract xs)  = mutate1 xs
      where
        w =ECon . ConstantAbstract
        mutate1 (AbsLitPartition xx) = mutate_2d (w . AbsLitPartition) xx
        mutate1 (AbsLitFunction _)   = return [] -- lots of effort
        mutate1 _ = return []
    mutate _ = return []


instance (HasGen m,  HasLogger m) =>  Reduce (AbstractLiteral Expr) m where
    single t   = ttypeOf t >>= singleLitExpr

    subterms AbsLitTuple{} = return []
    subterms x = return . map ELit .  innersExpand reduceLength $ x

    -- Don't try to reduce empty literals
    reduce li | isLitEmpty li = return []

    reduce li@AbsLitTuple{} = do
      rLits <- getReducedChildren (ELit) li
      let lss = map (replaceChildren li) (transposeFill rLits)
      reduceChecks li $ lss

    reduce li = do
      rLits <- getReducedChildren (ELit) li
      let lss = map (replaceChildren li) (transposeFill rLits)
      let res = concatMap (innersExpand reduceLength1) lss
      reduceChecks li $ res


    mutate (AbsLitPartition xs) = mutate_2d (ELit . AbsLitPartition) xs
    mutate (AbsLitFunction xs)  = do
      reductions <- mapM reduceTuple ixs
      let fixedOthers = [ if xi == ei then e else Fixed x
                        | ( (x,xi), (e, ei))  <- zip ixs reductions
                        ]
      let fixed = map fixReduced fixedOthers
      let fixed_i = zip fixed [0..]
      let expanded =  map (expand ixs)  fixed_i

      addLog "given" (map pretty xs)
      addLog "reductions" (map pretty reductions)
      addLog "fixedOthers" (map pretty fixedOthers)
      addLog "fixed" (map pretty fixed)

      return $ concatMap (map (ELit . AbsLitFunction)) expanded

      where
        ixs = zip xs [0 :: Int ..]

        reduceTuple ( (a,b) ,i) = do
           ra <- reduce a
           rb <- reduce b
           return ( Reduced (ra,rb) (a, b) , i)

        fixReduced :: RFuncChoices -> RFuncChoices
        fixReduced (Reduced (as,bs) (da,db)) =
            let fixa = [ (ra,db)  |  ra <- as ]
                fixb = [ (da,rb)  |  rb <- bs ]
            in  ReducedFixed (fixa ++ fixb)

        fixReduced x = x

        expand :: [((Expr,Expr), Int)] -> (RFuncChoices, Int)
               -> [[(Expr,Expr)]]
        expand ys (ReducedFixed xx, i)  =
            [ [ if i == yi then x else y | (y,yi) <- ys ] | x <- xx ]

        expand _ _ = $(neverNote "expand invaild argument")

    mutate _ = return []

type RFuncChoices = RChoices ([Expr],[Expr])  (Expr,Expr)
data RChoices a b = Reduced a b
                  | Fixed b
                  | ReducedFixed [b]
                    deriving Show


instance Pretty RFuncChoices where
    pretty (Reduced (xa,xb) x2)  = hang ("Reduced" <+> pretty x2) 4 $
                                     "fst" <+> (sep . punctuate  "," $ map pretty xa)
                                 <+> "snd" <+> (sep . punctuate  "," $ map pretty xb)
    pretty (Fixed x)        = "Fixed" <+> pretty x
    pretty (ReducedFixed x) = "ReducedFixed" <+>  (vcat . map pretty)  x

mutate_2d :: forall a (m :: * -> *) b.
             (Monad m, Eq a) =>
             ([[a]] -> b) -> [[a]] -> m [b]
mutate_2d wrap xs = do
  let reductions = map (\(x,i) -> (reduceLength x,i) ) ixs
      res = map f reductions

  return $ map wrap (concat res)

  where
    ixs = zip xs allNats
    f (es,ei) = [ [ if xi == ei then e else x | (x,xi) <-ixs ]  | e <- es ]


getReducedChildren :: (Monad m, Applicative m, HasGen m,  HasLogger m)
                   => (z -> Expr) -> z -> m ([([Expr], Expr)])
getReducedChildren zToExpr lit = do
  start <- withGen_new []
  fin <- flip runStateT start $ descendM fff (zToExpr lit)
  return $ reverse . withGen_val . snd $ fin
  where
     fff (x :: Expr) = do
       xs :: [([Expr],Expr)] <- gets withGen_val
       c <- reduce x
       -- let ht = heads_tails c
       let ht = c
       withGen_put ((ht,x) : xs)

       return x


transposeFill :: (Eq a) => [([a], a)] -> [[a]]
transposeFill ee =
  let len    = maximum $ map (length . fst) ee
      padded = [ ll ++ (replicate (len - length ll) defVal)
               | (ll, defVal) <- ee ]
  in  transpose padded


replaceChildren :: Data c => c -> [Expr] -> c
replaceChildren lit news = fst . flip runState news $ f1
                           <$> T.mapM fff ch1
   where
    (ch1, f1) = biplate lit
    fff _ = do
      (x:xs) <- get
      put xs
      return x


instance (HasGen m,  HasLogger m) => Reduce (Domain () Expr) m where
  reduce li = do
    rLits <- getReducedChildren (EDom) li
    let lss = map (replaceChildren li) (transposeFill rLits)
    let res = concatMap (innersExpand reduceLength1) lss

    si   <- single li
    subs <- subterms li
    mu   <- mutate li
    -- return $ mconcat  $
    reduceChecks li $ mconcat  $
               [ [ x | (EDom x) <- si]
               , res
               , [ x | (EDom x) <- subs]
               , [ x | (EDom x) <- mu]
               ]

  single x@DomainAny{}       = return [EDom x]
  single x@DomainBool        = return [EDom x]
  single x@DomainEnum{}      = return [EDom x]
  single x@DomainUnnamed{}   = return [EDom x]
  single x@DomainOp{}        = return [EDom x]
  single x@DomainReference{} = return [EDom x]
  single x@DomainMetaVar{}   = return [EDom x]

  single DomainInt{} = do
      i <- chooseR (0, 5)
      return $ [EDom $ DomainInt [RangeSingle (ECon $ ConstantInt i)]]

  single (DomainTuple x) = do
    xs <- mapM single x
    return [ EDom $ DomainTuple vs
           | vs <- take 2 $ transpose . map (map unEDom) $  xs]

  single (DomainMatrix x1 x2) = do
   doms <- single x1 >>= pure . map unEDom
   inn <- single x2  >>= pure . map unEDom

   return . concat . (flip map) (take 2 inn) $ \b ->
       [ EDom $ DomainMatrix a b | a <- (take 2 doms ++ [x1] ) ]

  single (DomainSet _ _ x3) = do
    inn <- single x3  >>= pure . map unEDom
    return [ EDom $ DomainSet () def a | a <- take 2 $ inn ]

  -- TODO single MSet attrs
  single (DomainMSet _ attrs x3) = do
    inn <- single x3  >>= pure . map unEDom
    return $ [ EDom $ DomainMSet () attrs a | a <- take 2 $ inn ]

  single (DomainPartition _ _ x3) = do
    inn <- single x3  >>= pure . map unEDom
    return [ EDom $ DomainPartition () def a | a <- take 2 $ inn ]

  single (DomainSequence _ _ x3) = do
    inn <- single x3  >>= pure . map unEDom
    return [ EDom $ DomainSequence () def a | a <- take 2 $ inn ]

  single (DomainFunction _ _ x1 x2) = do
   aa <- single x1  >>= pure . map unEDom
   bb <- single x2  >>= pure . map unEDom

   return . concat . (flip map) (take 2 aa ++ [x1]) $ \a ->
       [ EDom $ DomainFunction () def a b | b <- (take 2 bb ++ [x2] )
       , x1 /= a || x2 /= b ]

  single (DomainRelation _ _ x3)  = do
    xs <- mapM single x3
    return [ EDom $ DomainRelation () def vs
           | vs <- take 2 $ transpose . map (map unEDom) $  xs]

  --FIXME other types
  single x@(DomainRecord _)             = return [EDom x]
  single x@(DomainVariant _)            = return [EDom x]

  subterms x = return . map (EDom) . innersExpand reduceLength $ x

  mutate (DomainFunction r _ a b)   = return [EDom $ DomainFunction r def a b ]
  mutate (DomainSet r _ x3)         = return [EDom $ DomainSet r def x3 ]
  mutate (DomainSequence r _ x3)    = return [EDom $ DomainSequence r def x3 ]
  mutate (DomainRelation r _ x3)    = return [EDom $ DomainRelation r def x3 ]
  mutate (DomainPartition r _ x3)   = return [EDom $ DomainPartition r def x3 ]

  -- mutate (DomainMSet r x2 x3)        = _f
  mutate _ = return []

unEDom :: Expr -> Domain () Expr
unEDom (EDom b) = b
unEDom b        = lineError $line [ "not an Domain" <+> pretty b]



instance (HasGen m,  HasLogger m) =>  Reduce (Op Expr) m where

    single o = ttypeOf o >>= singleLitExpr

    -- Generic versions only works on things like [essencee| 1 ** 2 |]  i.e
    -- EOp (MkOpPow (OpPow (ECon (ConstantInt 1)) (ECon (ConstantInt 2w))))
    -- where the sub terms are the direct child of the Op.
    -- It does not work on things like [essencee| 1 + 2 |]


    subterms e@[opp| &a + &b |]  = subterms_op e [a,b]
    subterms e@[opp| &a * &b |]  = subterms_op e [a,b]
    subterms e@[opp| &a /\ &b |] = subterms_op e [a,b]
    subterms e@[opp| &a \/ &b |] = subterms_op e [a,b]

    subterms e  =  do
      let subs = F.toList e
      subterms_op e subs


    -- Why are these 4 lines needed?
    reduce e@[opp| &a + &b |]  = reduce_op2 ( m2t $ \(c,d) ->  [opp| &c + &d |])  [a,b] >>= reduceChecks e
    reduce e@[opp| &a * &b |]  = reduce_op2 (m2t  $ \(c,d) ->  [opp| &c * &d |])  [a,b] >>= reduceChecks e
    reduce e@[opp| &a /\ &b |] = reduce_op2 (m2t  $ \(c,d) ->  [opp| &c /\ &d |]) [a,b] >>= reduceChecks e
    reduce e@[opp| &a \/ &b |] = reduce_op2 (m2t  $ \(c,d) ->  [opp| &c \/ &d |]) [a,b] >>= reduceChecks e

    reduce e = do
      let subs = F.toList e
      res <- reduce_op e subs
      chs <- reduceChecks e res
      addLog $line [  ]
      addLog "op" [ pretty  e]
      addLog "op_subs" (map pretty subs)
      addLog "op_res"  (map pretty res)
      addLog "op_chs"  (map pretty chs)
      addLog "lengths" [ nn "res" (length res)
                       , nn "subs" (length subs)
                       , nn "chs"  (length chs)]
      return chs


subterms_op :: forall (m :: * -> *) a t.
               (TTypeOf t, TTypeOf a, Applicative m, Monad m) =>
               a -> [t] -> m [t]
subterms_op e subs =  do
  resType <- ttypeOf e
  tys <- mapM ttypeOf subs
  -- These should be the same, but only the last two give the different (wrong) answer
  let allowed  = map fst . filter (\(_,ty) -> typesUnify [resType,ty] ) $ zip subs tys
  -- let allowed  = [ x | (x,ty)<-zip subs tys, typesUnify [resType, ty] ]
  -- let allowed  = map fst [ (x,ty) | x<-subs | ty <- tys, typesUnify [resType, ty]  ]
  -- let allowed  = [ x | x<-subs | ty <- tys, typesUnify [resType, ty]  ]
  return allowed


reduce_op :: forall (m :: * -> *). (HasGen m,  HasLogger m)
          => Op Expr -> [Expr] -> m [Op Expr]
reduce_op x subs = reduce_op2 (replaceOpChildren x) subs


reduce_op2 :: forall (m :: * -> *). (HasGen m,  HasLogger m)
           => ([Expr] -> Op Expr) -> [Expr] -> m [Op Expr]
reduce_op2 f subs = do
  rs <- mapM reduceAdd subs

  addLog "reduce_op2 subs" (map pretty subs)
  mapM_ (\xx -> addLog "rs#" (map pretty xx)  )  rs

  case all (== []) rs of
    True   -> return []
    False -> do
      xrs <- zipWithM giveVals subs rs

      let res_ = [ f vs | vs <- sequence xrs
                -- , or $ zipWith (\z1 z2 -> runIdentity $ simpler1 z1 z2) vs subs
                ]
      let res   = filter (\x -> runIdentity $ simpler1 x (f subs)  ) res_
      addLog "reduce_op2 rs" (map prettyArr rs)
      mapM_ (\xx -> addLog "reduce_op2 xrs" (map pretty xx)  )  xrs

      addLog "reduce_op2 res" (map pretty res)
      addLog "reduce_op2 lengths"
             [ nn "res_" (length res_)
             , nn "res"  (length res)
             , nn "rs"   (length rs)
             , nn "xrs"   (length xrs)
             ]
      return res

  where
  giveVals :: (HasGen m, HasLogger m)
           => t -> [t] -> m [t]
  giveVals defaul []  = return [defaul]
  giveVals defaul [x] = return [x,defaul]
  giveVals _ [x,y]    = return [x,y]
  giveVals _ (x:xs)   = do
       return (x:xs)
    -- return [x,last xs]

  reduceAdd e = do
    re <- reduce e
    return $ re ++ [e]


infixl 1 -|
(-|) :: (Simpler a e, HasGen m,  HasLogger m) =>
        (a -> (c,d) ) -> (m a, e) -> m (Maybe ((c,d)))
f  -| (a,e) = do
   aa <-a
   simpler1 aa e >>= \case
     True  -> return $ Just (f aa)
     False -> return Nothing


-- | Return the two shortest & two longest sub sequence of the elements
reduceLength :: Eq a =>  [a] -> [[a]]
-- reduceLength xs =  heads_tails . init $ inits xs
reduceLength xs = filter (/=[]) $ init $ inits xs

reduceLength1 :: Eq a =>  [a] -> [[a]]
-- reduceLength1 xs =  heads_tails  $ inits xs
reduceLength1 xs = filter (/=[]) $ inits xs

heads_tails :: forall t. [t] -> [t]
heads_tails [] = []
heads_tails (_:es) = [e | (e,i) <- zip es [0..], (i >= length es - 2) || (i < 2)  ]



-- | return the simplest literals, two at most
singleLit :: (HasGen m, HasLogger m) =>Type -> m [Expr]
singleLit TypeInt = do
  pure ConstantInt<*> chooseR (0, 5) >>= return . (\a ->  [ECon a ] )

singleLit TypeBool = oneofR [ConstantBool True, ConstantBool False]
              >>= return . (\a ->  [ECon a ] )

-- of TypeAny are empty
singleLit (TypeMatrix _ TypeAny) = rrError "singleLit TypeMatrix TypeAny" []
singleLit (TypeSet TypeAny)      = return [ELit $ AbsLitSet []]
singleLit (TypeMSet TypeAny)     = return [ELit $ AbsLitMSet []]

singleLit (TypeMatrix TypeInt x) = do
  s <- singleLit x
  let si = AbsLitMatrix  (dintRange 1 (genericLength s)) s
  let res = case s of -- Return the other combination
             []    -> error "singleLit empty matrix"
             [e]   -> [si,  AbsLitMatrix (dintRange 1 2) ([e,e])]
             (e:_) -> [AbsLitMatrix (dintRange 1 1) [e], si]

  return (map ELit res)

singleLit (TypeList x) = do
  s <- singleLit x
  let si = AbsLitMatrix  (dintRange 1 (genericLength s)) s
  let res = case s of -- Return the other combination
             []    -> error "singleLit empty matrix"
             [e]   -> [si,  AbsLitMatrix (dintRange 1 2) ([e,e])]
             (e:_) -> [AbsLitMatrix (dintRange 1 1) [e], si]

  return (map ELit res)

singleLit l@(TypeSet x) = do
  ty <- ttypeOf l
  let empty = ETyped ty (ELit $ AbsLitSet [])

  si <- pure AbsLitSet <*> (singleLit x )
  return [ ELit si, empty]

singleLit l@(TypeMSet x) = do
  ty <- ttypeOf l
  let empty = ETyped ty $ (ELit $ AbsLitMSet [])

  si <- pure AbsLitMSet <*> (singleLit x)
  d  <- singleLit x
  let dupped = AbsLitMSet $ concat $ replicate 2 d

  chosen <- oneofR [ELit si,ELit dupped]
  return [chosen, empty]

singleLit l@(TypeFunction x1 x2) = do
  ty <- ttypeOf l
  let empty = ETyped ty $ (ELit $ AbsLitFunction [])

  as <- singleLit x1
  bs <- singleLit x2
  let mu = AbsLitFunction (zip as bs)

  return [ ELit mu, empty]

singleLit (TypeTuple x) = do
  lits <- mapM singleLit x
  picked <- mapM oneofR lits
  return [ELit $ AbsLitTuple picked]

singleLit l@(TypeRelation x) = do
  ty <- ttypeOf l
  let empty = ETyped ty  (ELit $ AbsLitRelation [])

  litsm  <- mapM (singleLit) x
  lits   <- mapM oneofR litsm
  let rel= ELit $ AbsLitRelation $  [lits]

  return [rel, empty]


singleLit l@(TypePartition x) = do
  ty <- ttypeOf l
  let empty = ETyped ty (ELit $ AbsLitPartition [])

  litsAll <- concatMapM (singleLit) [x,x]
  num <- chooseR (1,3)
  let lits = take num $  nub2 litsAll

  addLog "singleLit-Par-lits" (map pretty lits)

  -- Choose if all the elements should go in one part
  chooseR (True,False) >>= \case
          True  -> do
            let par = ELit $ AbsLitPartition [lits]
            return $ [empty, par]
          False -> do
            point <- chooseR (0,length lits)
            let (as,bs) = splitAt point lits
                par     = ELit $ AbsLitPartition [ pa | pa <- [as, bs], pa /= [] ]
            return $ [empty, par]



singleLit TypeAny = rrError "singleLit of TypeAny" []
singleLit ty   = rrError "singleLit~Error" [nn "ty" ty, nn "groomed" (groom ty) ]

singleLitExpr :: (HasGen m, HasLogger m) =>Type -> m [Expr]
singleLitExpr ty = do
  addLog "singleLitExpr" [nn "ty" ty]
  singleLit $ ty


replaceOpChildren :: Op Expr -> [Expr] -> Op Expr
replaceOpChildren op news = fst . flip runState news $ f1 <$> T.mapM fff ch1
   where
     (ch1, f1) = biplate op
     fff _ = do
       (x:xs) <- get
       put xs
       return x


reduceList :: forall (m :: * -> *)
           .  (HasGen m, HasLogger m)
           => [Expr] -> m [[Expr]]
reduceList [] = return []
reduceList as = do
  let lzip = case zipperBi as of
           Nothing -> docError [pretty $line, pretty $ groom as   ]
           Just lz -> lz
  vs <- forM (allSiblings lzip) $ \ x -> do
    let cur = hole x
    rs <- reduce cur
    let ls = map (flip replaceHole x) rs
    return $ map fromZipper ls

  return . concat $ vs

    where
      allSiblings :: Zipper [Expr] Expr -> [Zipper [Expr] Expr]
      allSiblings z = z : maybe [] allSiblings (Zipper.right z)


runReduce :: (HasGen m, HasLogger m, Reduce a (StateT EState (IdentityT m)) )
          => Spec -> a -> m [a]
runReduce spe x = do
  addLog "runReduce" []
  state <- (newEState spe)
  olg <- getLog
  (res,resState) <- runIdentityT $ flip runStateT state{elogs_=olg} $ do
                    reduce x

  putLog (elogs_ resState)
  addLog "endReduce" []
  return res


runSingle :: forall (m :: * -> *) a.
    (Reduce a (StateT EState (IdentityT m)), HasGen m, HasLogger m)
 => Spec -> a -> m [Expr]
runSingle spe x = do
  addLog "runSingle" []
  state <- (newEState spe)
  olg <- getLog
  (res,resState) <- runIdentityT $ flip runStateT state{elogs_=olg} $ do
                    single x

  putLog (elogs_ resState)
  addLog "endSingle" []
  return res

m2t :: Show a => ((a,a) -> b) -> [a] -> b
m2t f [a,b] = f (a,b)
m2t _ x     = error . show . vcat $ ["m2t not two elements", pretty . show $ x ]



-- For ghci

__run  :: forall t a (t1 :: * -> *).
         (Pretty a, Foldable t1, Pretty t) =>
         Bool -> (t -> StateT EState Identity (t1 a)) -> t -> IO (t1 a)
__run b f ee  = do
  res <- __run1 b f ee
  mapM_ (print  . pretty )  res
  putStrLn "---"
  putStrLn . show . pretty  $ ee
  return res

__run1 :: forall t a (t1 :: * -> *).
         (Foldable t1) => Bool ->
         (t -> StateT EState Identity (t1 a)) -> t -> IO (t1 a)
__run1 b f ee = do
  let spe   :: Spec   = $never
      seed            = 323
      state :: EState = newEStateWithSeed seed spe
      (res, fstate)   = runIdentity $ flip runStateT state $ f ee
  if b then
      return ()
  else
      print . pretty $ (elogs_ fstate)
  return res


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


_replaceOpChildren_ex :: Op Expr
_replaceOpChildren_ex = replaceOpChildren
  [opp| 8 ** 3  |]  [  [essencee| 4 |], [essencee| 2 |] ]


-- instance Pretty [Expr] where
--     pretty = prettyBrackets  . pretty . vcat . map pretty

-- instance Pretty [[Expr]] where
--     pretty = prettyBrackets  . pretty . vcat . map pretty

-- instance Pretty [Literal] where
--     pretty = prettyBrackets  . pretty . vcat . map pretty

isLitEmpty :: AbstractLiteral a -> Bool
isLitEmpty (AbsLitMatrix _ [])  = True
isLitEmpty (AbsLitPartition xs) = all null xs
isLitEmpty (AbsLitRelation xs)  = all null xs
isLitEmpty lit                  = null $ F.toList lit
