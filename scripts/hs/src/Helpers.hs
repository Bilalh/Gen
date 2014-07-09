{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
module Helpers where

import Data.Time(formatTime,getCurrentTime)
import System.Locale(defaultTimeLocale)
import Language.E
import Data.Set(Set)
import qualified Data.Set as S
import qualified Data.Text as T

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
          f (e,_) = error . show $ "onlyNamesAsText*f: not a name" <+> pretty e

appendToName :: Text -> E -> E
appendToName end [xMatch|[Prim (S name)] := reference |]  =
    [xMake| reference := [Prim (S $ T.append name end )]  |]
appendToName e _ = error . show $ "appendToName: not a name" <+> pretty e

getName :: E -> Text
getName [xMatch| [Prim (S name)] := reference  |] = name
getName e = error . show $ "getName: not a name" <+> pretty e

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

mkAttr :: (T.Text, Maybe Integer) -> E
mkAttr (n, Nothing) = [xMake| attribute.name.reference := [Prim (S n)] |]
mkAttr (n, Just v ) = [xMake| attribute.nameValue.name.reference := [Prim (S n)]
                            | attribute.nameValue.value          := [mkInt v]
                            |]

addAttr :: E -> E -> E
addAttr a (Tagged "domain" [Tagged tt ( [xMatch| att := attributes.attrCollection |]:rs) ] ) =
   let att2 = a : att in
   (Tagged "domain" [Tagged tt ( [xMake| attributes.attrCollection := att2 |]:rs ) ] )

addAttr a b = error . show . vcat $ "addAttr attr dom" : map prettyAsPaths  [a,b]

_attTest :: Doc
_attTest = do
    let ee= [dMake| function int --> int |]
    let f = mkAttr ("size", Just $  4)
    let res = addAttr f ee
    pretty res
    prettyAsPaths  $ res
mkInt :: Integer -> E
mkInt j =  [xMake| value.literal := [Prim (I j)] |]


timestamp :: IO Int
timestamp = do
    epochInt <- (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int
    return epochInt
