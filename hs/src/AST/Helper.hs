{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns #-}
module AST.Helper where

{-
import Language.E
import qualified Data.Text as T


type AttrE = E
mkAttr :: (T.Text, Maybe Integer) -> AttrE
mkAttr (n, Nothing) = [xMake| attribute.name.reference := [Prim (S n)] |]
mkAttr (n, Just v ) = [xMake| attribute.nameValue.name.reference := [Prim (S n)]
                            | attribute.nameValue.value          := [mkInteger v]
                            |]

addAttr :: AttrE -> E -> E
addAttr a (Tagged "domain" [Tagged tt ( [xMatch| att := attributes.attrCollection |]:rs) ] ) =
   let att2 = a : att in
   (Tagged "domain" [Tagged tt ( [xMake| attributes.attrCollection := att2 |]:rs ) ] )

addAttr a b = error . show . vcat $ "addAttr attr dom" : map prettyAsPaths  [a,b]

addAttrs :: [AttrE] -> E -> E
addAttrs attrs (Tagged "domain" [Tagged tt ( [xMatch| att := attributes.attrCollection |]:rs) ] ) =
   let att2 = attrs ++  att in
   (Tagged "domain" [Tagged tt ( [xMake| attributes.attrCollection := att2 |]:rs ) ] )

addAttrs a b = error . show . vcat $ "addAttrs dom attrs" : map prettyAsPaths  (b : a)

combineAttrs :: [(Text, Maybe Integer)] -> [(Text, Bool)] -> [AttrE]
combineAttrs a1 a2 =
    let
        b1 = filter ( isJust . snd )
        b2 = map (\(a,_) -> (a,Nothing)) .  filter ( (==True) . snd )
    in
        map mkAttr (b1 a1  ++ b2 a2)


mkInteger :: Integer -> E
mkInteger j =  [xMake| value.literal := [Prim (I j)] |]

getInteger :: E -> Integer
getInteger [xMatch| [Prim (I j)] := value.literal |] = j
getInteger e = error . show  $ vcat ["Not a int", pretty e]

getIntegerMaybe :: E -> Maybe Integer
getIntegerMaybe [xMatch| [Prim (I j)] := value.literal |] = Just j
getIntegerMaybe _ = Nothing

fetchAttrValue :: E -> Text -> Maybe Integer
fetchAttrValue [xMatch| as := attrCollection |] n = listToMaybe $ mapMaybe f as
  where f [xMatch| [Prim (S n')] := attribute.nameValue.name.reference
                 | [Prim (I v)]  := attribute.nameValue.value.value.literal|] = if n==n'
                                                                                  then Just v
                                                                                  else Nothing
        f _ = Nothing
fetchAttrValue _ _ = Nothing

fetchAttr :: E -> Text -> Bool
fetchAttr [xMatch| as := attrCollection |] n = or $ map f as
  where f [xMatch| [Prim (S n')] := attribute.name.reference |] = if n==n'
                                                                    then True
                                                                    else False
        f _ = False
fetchAttr _ _ = False


mkSpec :: [E] -> Spec
mkSpec es =
    let spec = Spec (LanguageVersion "Essence" [1,3])
         . listAsStatement
         -- . normaliseSolutionEs
         $ es
    in spec


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

-}
