{-# LANGUAGE  DeriveGeneric,  OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Main where

import Bug

import Language.E
import Language.E.GenerateParams.Typedefs
import Language.E.Pipeline.ReadIn(readSpecPreambleFromFile)
import Language.E.Pipeline.Driver ( driverConjureSingle )

import System.Environment(getArgs)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import qualified GHC.Generics as GG

import Data.Set(Set)
import qualified Data.Set as S


data Info =
  Info { ordering :: [Text]
       , givens   :: S.Set Text
       , finds    :: S.Set Text 
       } deriving (Show,GG.Generic)

instance A.FromJSON Info
instance A.ToJSON Info


getJSONInfo :: FilePath ->  IO (Maybe Info)
getJSONInfo fp = fmap A.decode $ B.readFile fp

main :: IO ()
main =  do
    args <- getArgs
    case args of
        [fp,out_fp, info_fp] -> do 
            minfo <- getJSONInfo info_fp
            case minfo of 
                Just info -> main' fp out_fp info 
                Nothing -> error "Json invaild"
        _    -> error "EssenceGivensToFinds <in-essence> <out-essence> <info-fp>"

main' :: EssenceFP -> EssenceFP  -> Info -> IO ()

main' in_fp out_fp info  = do
    spec  <- readSpecPreambleFromFile in_fp
    putStrLn  "Creating Essence specification of the param"
    driverConjureSingle False True
        (Just out_fp)
        $ runCompE "Make Essence from where" (prepareParamSpecification spec info)



prepareParamSpecification :: (MonadConjure m) => Essence -> Info ->  m Spec
prepareParamSpecification (Spec v es) Info{..} = do
    --mkLog "start " ( vcat . map prettyAsPaths $ (statementAsList es))
    mkLog "start " (pretty es)

    let es' = filter removeStuff  (statementAsList es)
   
    let (result,_) = head .  runCompE "Transform" $ bottomUpE' (changer givens)  (listAsStatement es')
    let res = either (bug . pretty) id result

    mkLog "result" (pretty . sort . statementAsList $ res)
    return $ Spec v res

changer :: MonadConjure m =>  Set Text -> E ->  m E
changer givens 
  [xMatch| dom             := topLevel.declaration.given
         | [Prim (S name)] := topLevel.declaration.given.name.reference |] | name `S.notMember` givens   =
 return [xMake|  topLevel.declaration.find := dom   |]


changer _ [xMatch| dom := topLevel.where    |]  =
 return [xMake|  topLevel.suchThat := dom |]

changer _ e = return e

removeStuff :: E -> Bool
removeStuff [xMatch| _ := topLevel.declaration.find |] = False
removeStuff [xMatch| _ := topLevel.suchThat |]         = False
removeStuff [xMatch| _ := topLevel.objective |]        = False
removeStuff _ = True


