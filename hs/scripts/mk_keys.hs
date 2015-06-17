{-# LANGUAGE LambdaCase, OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}

import Conjure.Language.AbstractLiteral (AbstractLiteral (AbsLitSet))
import Conjure.Language.Constant        (Constant (ConstantBool))
import Conjure.Language.Definition      (Expression (Constant), Objective(Maximising),AbstractPattern(Single))
import Conjure.Language.Domain
import Conjure.Language.Type            (Type (TypeAny))
import Conjure.Prelude                  (padRight,def)
import Control.Applicative              ((<$>))
import Control.Exception                (IOException, catch)
import Data.Data
import Data.List                        (intercalate, sort)
import NeatInterpolation
import Prelude

import qualified Data.Set as S

import Conjure.Language.Expression.Op.Internal.Generated (Op (..))


key_templete :: String -> String -> String
key_templete keys_data key_isString =
  [string|
    --This is an auto-generated file created by make keys
    {-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
    module Gen.Essence.Data.Key where

    import Gen.Imports
    import qualified Data.Aeson as A

    data Key = K_Unused
             | $keys_data
         deriving (Eq, Ord, Show, Data, Typeable, Generic, Enum)

    instance Hashable Key

    instance ToJSON Key where
      toJSON = A.String . stringToText . tail . tail . show

    instance FromJSON Key where
      parseJSON (A.String s) = return $ fromString . textToString $ s
      parseJSON _            = mzero


    instance Pretty Key where
      pretty = pretty . show

    instance IsString Key where
      $key_isString
      fromString t = error $ "Unknown Key: " ++ t ++
                     "\n Add new keys to src/Gen/Essence/Data/key_extra_names.txt if needed."
  |]

main :: IO ()
main = do
  ls <- getNames
  -- Don't why there are some dups in getNames
  let names = nub2 $ sort $ concat $ ls : dataNames

  let keys         = [ "K_" ++ n | n <- names ]
      keys_data    = intercalate "\n| " keys
      key_isString = concat
                            [ let a = padRight 25 ' ' ("\"" ++ n ++ "\"")
                              in [string| fromString $a = K_$n |]
                            | n <- "Unused" : names ]
  let res = key_templete keys_data key_isString
  writeOut "src/Gen/Essence/Data/key.hs" res


writeOut :: FilePath -> String -> IO ()
writeOut outFile outText= do
  outText' <- catch (Just <$> readFile outFile)
                    (\ (_ :: IOException) -> return Nothing )
  if and [ Just (length outText) /= (length <$> outText')
         , Just outText /= outText'
         ]
      then do
          putStrLn $ "Generating " ++ outFile
          writeFile outFile outText
      else
          putStrLn $ "Reusing " ++ outFile


dataNames :: [[String]]
dataNames = [ strs TypeAny
            , strs (ConstantBool True)
            , strs (AbsLitSet [] :: AbstractLiteral Constant)
            , strs (Constant      $ ConstantBool True)
            , strs (DomainBool   :: Domain () Constant)
            , strs (RangeOpen    :: Range Constant)
            , strs (def          :: SetAttr Constant)
            , strs (def          :: SizeAttr Constant)
            , strs (def          :: MSetAttr Constant)
            , strs (def          :: OccurAttr Constant)
            , strs (def          :: FunctionAttr Constant)
            , strs (def          :: JectivityAttr)
            , strs (def          :: PartialityAttr)
            , strs (def          :: SequenceAttr Constant)
            , strs (def          :: RelationAttr Constant)
            , strs (def          :: BinaryRelationAttrs)
            , strs (BinRelAttr_Reflexive  :: BinaryRelationAttr)
            , strs (def          :: PartitionAttr Constant)
            , strs (Single ""    :: AbstractPattern)
            , strs (Maximising   :: Objective)
            , [ if s == "Op" then s else drop 2 s
              | s <- strs (error "OP" :: Op Constant ) ]
            , [ "Int_" ++ show i  | i :: Integer <- [0..10]  ]
            ]

getNames :: IO [String]
getNames = do
  str <- readFile "src/Gen/Essence/Data/key_extra_names.txt"
  return $ lines str

strs :: (Data a) => a -> [String]
strs a = do
  let tyName = tyconUQname $ dataTypeName $  dataTypeOf  a
  let names = dataTypeConstrs . dataTypeOf $ a
  tyName : map show names



-- | nub is O(N^2) this is O(NlogN)
nub2 :: (Ord a) => [a] -> [a]
nub2 l = go S.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `S.member` s then go s xs
                                      else x : go (S.insert x s) xs
