{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
             MultiParamTypeClasses #-}
module Gen.AST.Spec where

import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Prelude
import Data.Map                    (Map)
import Gen.AST.Data
import Gen.AST.Translate           ()
import Text.Groom                  (groom)

import qualified Data.Map as M


data Spec = Spec Domains [Expr] (Maybe (OObjective, Expr) )
    deriving(Show, Generic, Typeable, Eq, Data, Ord)

instance Hashable  (Map Text (Int, GF)) where
    hashWithSalt salt m = hashWithSalt salt (M.toList m)

instance Serialize Spec
instance Hashable  Spec
instance ToJSON    Spec where toJSON = genericToJSON jsonOptions
instance FromJSON  Spec where parseJSON = genericParseJSON jsonOptions

data GF = Givenn (Domain () Expr)
        | Findd  (Domain () Expr)
    deriving(Show, Generic, Typeable, Eq, Data, Ord)

instance Serialize GF
instance Hashable  GF
instance ToJSON    GF where toJSON = genericToJSON jsonOptions
instance FromJSON  GF where parseJSON = genericParseJSON jsonOptions

type Domains = Map Text (Int,GF)


data OObjective = Maximisingg
                | Minimisingg
    deriving(Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize OObjective
instance Hashable  OObjective
instance ToJSON    OObjective where toJSON = genericToJSON jsonOptions
instance FromJSON  OObjective where parseJSON = genericParseJSON jsonOptions

instance Pretty OObjective where
    pretty Maximisingg  = "Maximising "
    pretty Minimisingg  = "Minimising "

instance Pretty GF where
    pretty (Givenn x) = "Givenn " <+> pretty x
    pretty (Findd x)  = "Findd "  <+> pretty x

instance Translate Spec Model where
    toConjure = toModel
    fromConjure = fromModel

instance Pretty Spec where
    pretty = pretty . (toConjureNote "Pretty Spec" :: Spec -> Model )


instance Translate (Text, GF) (FindOrGiven, Name, Domain () Expression)  where
    fromConjure (x,(Name t),cdom) | x == Find || x == Given = do
      dom <- fromConjure cdom
      let kind = if x == Find then Findd else Givenn
      return $ (t, kind dom)

    fromConjure x = fail ("fromConjure Expr " <+>  pretty x <+> (pretty . groom) x)

    toConjure (t,(Findd dom)) = do
      cdom <- toConjure dom
      return (Find, Name t, cdom)

    toConjure (t,(Givenn dom)) = do
      cdom <- toConjure dom
      return (Given, Name t, cdom)

instance Translate OObjective Objective where
    toConjure Maximisingg  = return Maximising
    toConjure Minimisingg  = return  Minimising

    fromConjure Maximising = return  Maximisingg
    fromConjure Minimising = return  Minimisingg

instance Translate (Maybe (OObjective,Expr)) (Maybe Statement) where
    toConjure Nothing = return Nothing
    toConjure (Just (a,b)) = do
      aa <- toConjure a
      bb <- toConjure b
      return $ Just $ Objective aa bb


    fromConjure Nothing = return Nothing
    fromConjure (Just (Objective o e)) = do
      oo <- fromConjure o
      ee <- fromConjure e
      return $ Just (oo, ee)

    fromConjure x = fromConjureFail "(Maybe (OObjective,Expr)) (Maybe Statement)" x

fromModel :: MonadFail m => Model -> m Spec
fromModel Model{mStatements} = do
  doms ::  [(Text, (Int, GF))] <- (flip evalStateT) 0 $ forM (mapMaybe getDoms mStatements) $ \cc -> do
            (t,gf) ::  (Text, GF) <- fromConjure cc
            modify (+1)
            i <- get
            return $ (t, (i :: Int, gf))

  cs :: [Expr] <- mapM fromConjure . concat .  mapMaybe getCs $ mStatements
  obj :: (Maybe (OObjective,Expr)) <- fromConjure . atMostOne .  mapMaybe getObj $ mStatements
  return $ Spec (M.fromList doms) cs obj

    where
      getDoms (Declaration (FindOrGiven a b c) ) = Just (a, b, c)
      getDoms _ = Nothing

      getCs :: Statement -> Maybe [Expression]
      getCs (SuchThat xs) = Just (xs)
      getCs _ = Nothing

      getObj o@(Objective _ _ ) = Just o
      getObj _ = Nothing

      atMostOne [x] = Just x
      atMostOne []  = Nothing
      atMostOne x   = fail . vcat $ ("mutiple objectives") :  map pretty x

toModel :: MonadFail m => Spec -> m Model
toModel (Spec doms exprs obj) = do
    let ds =   sortBy (comparing (fst . snd  ) ) $ M.toList doms
    tuples   <- mapM toConjure [ (t,gf) | (t,(_,gf)) <- ds]
    let cdoms = map toDom tuples
    cexprs <- mapM toConjure exprs
    cObj <- toConjure obj
    return $ def{mStatements=(cdoms ++  toSuchThat cexprs  ++ (maybeToList cObj) ) }


    where
      toSuchThat [] =  []
      toSuchThat xs =  [SuchThat xs]

      toDom (x,t,cdom) = Declaration $ FindOrGiven x t cdom

domOfGF :: GF -> (Domain () Expr)
domOfGF (Givenn x) = x
domOfGF (Findd x)  = x


instance PrettyWithQuan Spec where
  prettyWithQuan (Spec doms exprs obj) = vcat $
      [ pretty (def :: LanguageVersion)
      , prettyDomains  doms
      , "such that" <++> vcat (punctuate "," $  map prettyWithQuan exprs)
      , ""
      ]  ++ maybeToList (fmap prettyObjective obj)

    where
      prettyObjective :: (OObjective,Expr) -> Doc
      prettyObjective (o,expr) = (f o)  <+> prettyWithQuan expr
        where
          f Maximisingg = "maximising"
          f Minimisingg = "minimising"

      prettyDomains x = vcat $ map f $ sortBy (comparing (fst . snd  ) ) $ M.toList x
        where
          f (name,(_,Givenn dom)) =  "given" <+> pretty name <+> ":" <+> pretty dom
          f (name,(_,Findd dom))  =  "find"  <+> pretty name <+> ":" <+> pretty dom
