{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gen.Essence.Expr where

import Conjure.Language.Definition
import Conjure.Language.Expression.Op
import Gen.Essence.Constant           ()
import Gen.Essence.Domain             ()
import Gen.Essence.Literal            ()
import Gen.Essence.Op                 ()
import Gen.Essence.Range              ()
import Gen.Essence.Rnd
import Gen.Essence.St
import Gen.Essence.Type               ()
import Gen.Helpers.TypeOf
import Gen.Helpers.SizeOf
import Gen.Imports

import qualified Data.Foldable as F
import qualified Data.Map      as M
import qualified Text.PrettyPrint as Pr


-- | Should specify The type at the top level
-- | i.e.  give (GType TypeBool) or
instance Generate Expr where
  give GNone = giveUnmatched "Generate Expr" GNone
  give g  = do
    logDepthCon $line g
    sanity "Generate Expr"
    let defs =
          [ (possible (Proxy :: Proxy Constant),               (K_ECon,  doConstant))
          , (possible (Proxy :: Proxy (AbstractLiteral Expr)), (K_ELit,  doLitetal ))
          , (possible (Proxy :: Proxy Var),                    (K_EVar,  EVar      <$> give g))
          , (possible (Proxy :: Proxy LVar),                   (K_LVar,  wrapLVar  <$> give g))
          , (possible (Proxy :: Proxy (Op Expr)),              (K_EOp,   EOp       <$> give g))
          , (possible (Proxy :: Proxy ListComp),               (K_EComp, wrapComp  <$> give g))
          ]


    parts <- getPossibilitiesKeyed g defs
    frequency3 parts

    where
    doConstant = do
      (con, nty :: Type) <- give g
      case con of
        (ConstantAbstract x) ->


          case isEmptyInside x of
            False -> do
              logDebug2 $line ["not Empty " <+> pretty x]
              logDebug2 $line ["not Empty groomed" <+> pretty (groom x)]
              return $ ECon con
            True -> do
              logDebug2 $line  ["is Empty " <+> pretty x]
              return $ ETyped nty (ECon con)
        _ -> return $ ECon con


    isEmptyInside :: AbstractLiteral Constant -> Bool
    isEmptyInside (AbsLitMatrix _  [])               = True
    isEmptyInside (AbsLitPartition xs) | any null xs = True
    isEmptyInside (AbsLitRelation xs)  | any null xs = True
    isEmptyInside (AbsLitMatrix _  xs) =
        or . map isEmptyInside . mapMaybe isAbst $ xs
    isEmptyInside lit = case F.toList lit of
                          [] -> True
                          xs -> or . map isEmptyInside . mapMaybe isAbst $ xs


    isAbst :: Constant -> Maybe (AbstractLiteral Constant)
    isAbst (ConstantAbstract x) = Just x
    isAbst _                    = Nothing



    doLitetal = do
      (lit :: AbstractLiteral Expr, nty :: Type) <- give g
      case isEmpty lit of
        False -> do
          logDebug2 $line ["not Empty " <+> pretty lit]
          logDebug2 $line ["not Empty groomed" <+> pretty (groom lit)]
          logDebug2 $line ["debug"]
          return $ ELit lit
        True  -> do
          logDebug2 $line  ["is Empty " <+> pretty lit]
          return $ ETyped nty (ELit lit)

    isEmpty (AbsLitMatrix _ [])  = True
    isEmpty (AbsLitPartition xs) = all null xs
    isEmpty (AbsLitRelation xs)  = all null xs
    isEmpty lit                  = null $ F.toList lit



    wrapComp (a,b,c) = EComp a b c

    wrapLVar (LVar v) = EVar v

  possiblePure _ _ _ = True
  requires _ _       = []


instance Generate ListComp where
  give GNone = give (GType $ TypeMatrix TypeInt TypeBool)

  give (GType (TypeMatrix TypeInt a)) = do
    gen_num <- choose3 (1,2)
    gen_var <- vectorOf3 gen_num (dgive (GType a))
    let (gens, vars) = unzip gen_var

    inner <- withVars vars $ dgive (GType a)
    cs_num <-choose3 (0,2)
    cs    <- withWeights [(K_EVar, 0)]
           . withVars vars
           . vectorOf3 cs_num
           $ dgive (GType TypeBool)

    return (inner, gens, cs)

  give t = giveUnmatched "Generate ListComp" t

  possiblePure _ (Just ty@(TypeMatrix TypeInt _)) d = (fromIntegral d) >= depthOf ty + 1
  possiblePure _ _ _ = False
  requires _ _       = []


instance Generate (EGen, Var) where
  give GNone       = give (GType TypeInt)
  give con@GType{} = do
    dom <- give con
    domTy <- ttypeOf dom

    name <- nextVarName "l"
    let var = Var name domTy

    return $ (GenDom (Single $ Name name) dom, var)


  give t = giveUnmatched "Generate EGen" t

  possible _ _ = return True
  requires _ _ = []

instance Generate Var where
  give (GType ty) = do
    ds <- gets doms_
    let ks = M.toList . M.filter (== ty) . M.map (typeOfDom . domOfGF) $ ds
    let choices =  map (return . uncurry Var) ks
    oneof3 choices

  give t = giveUnmatched "Generate Var" t

  possible _ (GType ty) = do
    ds <- gets doms_
    F.foldrM f False ds

    where
    f _  True  = return True
    f gf False = do
       b <- ttypeOf gf
       return $ b == ty

  possible _ _ = return False
  requires _ _ = []

instance Generate LVar where
  give (GType ty) = do
    vars <- gets newVars_
    let zipped  = zipWith (\v i -> (i,v) ) vars [1..]
    let choices = [ (i,pure (LVar v))  | (i, v@(Var _ vty) ) <- zipped, vty == ty ]
    frequency3 choices

  give t = giveUnmatched "Generate Var" t

  possible _ (GType ty) = do
    vars <- gets newVars_
    F.foldrM f False vars

    where
    f _  True  = return True
    f (Var _ vty) False = return $ vty == ty

  possible _ _ = return False
  requires _ _ = []

-- FIXME Having these Instances is a bad idea
instance Pretty [Expr] where
    pretty = Pr.brackets  . prettyArr

instance Pretty [EGen] where
    pretty = Pr.brackets  . prettyArr
