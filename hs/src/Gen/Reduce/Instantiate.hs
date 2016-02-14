module Gen.Reduce.Instantiate where

import Gen.Imports
import Conjure.Language.Definition
import Conjure.Language.Domain

instantiateGenerators :: Expr -> [Expr]
instantiateGenerators comp@(EComp _ gens _)  =
  case (mapM allow gens ) of
    Nothing -> []
    Just [] -> []
    Just xs -> concatMap process xs

  where
    allow :: EGen -> Maybe (Text, [Constant])
    allow (GenDom (Single (Name name)) dom) = allow1 name dom
    allow _ = Nothing

    allow1 name DomainBool{} = Just (name, [ ConstantBool True
                                         , ConstantBool False] )

    allow1 name (DomainInt rs) =
      case rangesInts rs of
        Nothing -> Nothing
        Just xs ->
          let vals = nub2 $ take 2 xs ++ (reverse . take 2 $ xs)
          in  Just (name, [ ConstantInt v  | v <- vals] )

    allow1 _ _ = Nothing

    process :: (Text, [Constant]) -> [Expr]
    process (name, vals) = [ instantiateExpr [(name,v)] comp  | v <- vals ]



instantiateGenerators _ = []

instantiateExpr :: [(Text, Constant)] ->  Expr -> Expr
instantiateExpr vals expr =
    let
        inline p@(EVar (Var nm _) ) =
            let x = ECon <$> lookup nm vals
            in fromMaybe p x
        inline p =  p

    in transformBi inline (expr)
