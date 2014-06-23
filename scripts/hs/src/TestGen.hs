{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Language.E
-- import Language.E.NormaliseSolution(normaliseSolutionEs)
import Language.E.Pipeline.ReadIn(writeSpec)

import Data.Set(Set)
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Monad.Trans.State.Strict(StateT)
import Text.Groom(groom)

data GenState = GenState
        { gFinds :: [(Text, E)]   -- Domains of finds
        -- , gLogs :: !LogTree

        } deriving (Show)

class (Monad m,  MonadState GenState m) => MonadGen m where
    {}



test :: MonadGen m => m Text
test = do
    fs <- gets gFinds
    modify $ \ st -> st { gFinds = ("a",[dMake| int(1..2) |]) : fs  }
    return ""


chooseFindsDomains = do
    fs <- gets gFinds
    modify ( \s-> s{ gFinds = ("f",[dMake| int(4..4) |]) : fs }  )
    return ""

makeEs :: StateT GenState IO [E]
makeEs = do
    a <- chooseFindsDomains
    fs <- gets gFinds
    modify ( \s-> s{ gFinds = ("a",[dMake| int(1..2) |]) : fs }  )
    return [ [eMake| 3 |] ]

run :: IO [E]
run = do
    --let finds = [mkFind  ( mkName "d", [dMake| int(1..2) |] )]
    (res,st) <- runStateT makeEs GenState{gFinds=[]}
    return $ res

main :: IO ()
main = do
    es <- run

    spec <- mkSpec es
    writeSpec "a.essence" spec

mkSpec :: [E] -> IO Spec
mkSpec es = do

    let spec = Spec (LanguageVersion "Essence" [1,3])
         . listAsStatement
         -- . normaliseSolutionEs
         $ es
    print .  pretty $ spec
    return spec


pullFinds :: [E] -> [(E,E)]
pullFinds es = mapMaybe pullFind es
    where pullFind [xMatch| [name] := topLevel.declaration.find.name
                          | [dom]  := topLevel.declaration.find.domain |] = Just (name,dom)
          pullFind _ = Nothing

pullGivens :: [E] -> [(E,E)]
pullGivens es = mapMaybe pullGiven es
    where pullGiven [xMatch| [name] := topLevel.declaration.given.name
                           | [dom]  := topLevel.declaration.given.domain |] = Just (name,dom)
          pullGiven _ = Nothing


onlyNamesAsText :: [(E,E)] -> Set Text
onlyNamesAsText = S.fromList . map f
    where f ([xMatch|[Prim (S name)] := reference |], _) = name

appendToName :: Text -> E -> E
appendToName end [xMatch|[Prim (S name)] := reference |]  = [xMake| reference := [Prim (S $ T.append name end )]  |]

getName :: E -> Text
getName [xMatch| [Prim (S name)] := reference  |] = name

mkName :: Text -> E
mkName name = [xMake| reference :=  [Prim (S name)]  |]

mkFind :: (E,E) -> E
mkFind (name,dom) =[xMake| topLevel.declaration.find.name   := [name]
                         | topLevel.declaration.find.domain := [dom]
                         |]

mkGiven :: (E,E) -> E
mkGiven (name,dom) =[xMake| topLevel.declaration.given.name   := [name]
                          | topLevel.declaration.given.domain := [dom]
                          |]

mkAttr :: (T.Text, Maybe E) -> E
mkAttr (n, Nothing) = [xMake| attribute.name.reference := [Prim (S n)] |]
mkAttr (n, Just v ) = [xMake| attribute.nameValue.name.reference := [Prim (S n)]
                            | attribute.nameValue.value          := [v]
                            |]

