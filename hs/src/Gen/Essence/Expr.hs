{-# LANGUAGE ParallelListComp #-}
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


instance Generate Expr where
  give g  = do
    sanity "Generate Expr"
    let defs =
          [ (possible (Proxy :: Proxy Constant),               (K_ECon,  doConstant))
          , (possible (Proxy :: Proxy (AbstractLiteral Expr)), (K_ELit,  doLitetal ))
          , (possible (Proxy :: Proxy Var),                    (K_EVar,  EVar      <$> give g))
          , (possible (Proxy :: Proxy LVar),                   (K_LVar,  wrapLVar  <$> give g))
          , (possible (Proxy :: Proxy (Op Expr)),              (K_EOp,   EOp       <$> give g))
          , (possible (Proxy :: Proxy ListComp),               (K_EComp, wrapComp  <$> give g))
          ]


    parts <- getPossibilities g defs
    frequency3 parts

    where
    doConstant = do
      (con, nty :: Type) <- give g
      case con of
        (ConstantAbstract x) -> case isEmpty x of
                                  False -> return $ ECon con
                                  True  -> return $ ETyped nty (ECon con)
        _ -> return $ ECon con

    doLitetal = do
      (lit, nty :: Type) <- give g
      case isEmpty lit of
        False -> return $ ELit lit
        True  -> return $ ETyped nty (ELit lit)

    isEmpty (AbsLitMatrix _ []) = True
    isEmpty lit = F.toList lit == []


    wrapComp (a,b,c) = EComp a b c

    wrapLVar (LVar v) = EVar v

  possiblePure _ _ _ = True


instance Generate ListComp where
  give GNone = give (GType $ TypeMatrix TypeInt TypeBool)

  give (GType (TypeMatrix TypeInt a)) = do
    gen_num <- choose3 (1,2)
    gen_var <- vectorOf3 gen_num (dgive (GType a))
    let (gens, vars) = unzip gen_var

    inner <- withVars vars $ give (GType a)
    cs_num <- choose3 (0,2)
    cs    <- withVars vars $ vectorOf3 cs_num $ dgive (GType TypeBool)

    return (inner, gens, cs)

  give t = giveUnmatched "Generate ListComp" t

  possiblePure _ (Just ty@(TypeMatrix TypeInt _)) d = (fromIntegral d) >= depthOf ty
  possiblePure _ _ _ = False

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


-- FIXME Having these Instances is a bad idea
instance Pretty [Expr] where
    pretty = Pr.brackets  . prettyArr

instance Pretty [EGen] where
    pretty = Pr.brackets  . prettyArr
