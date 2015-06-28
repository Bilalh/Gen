module Gen.Essence.Weightings where

import Gen.Essence.St
import Gen.Imports
import Gen.IO.Formats
import Conjure.Language.Expression.Op

import qualified Data.Map      as M

save :: (MonadIO m,MonadFail m) => Directory -> [(String,KeyMap)] -> m ()
save dir f = do
  liftIO $ createDirectoryIfMissing True dir
  forM_ f $ \(name,mapping) -> do
    writeToJSON (dir </> name <.> ".json" ) mapping


defaults :: [(String,KeyMap)]
defaults = [("default", def)]

-- | Every Key that could be used
every :: [(String,KeyMap)]
every =  (\x -> [("all", x)]  ) . KeyMap . M.union def  . M.fromList $  zip allKeys (repeat 100)

-- | Try to generate essence that close to essence
eprimeish :: [(String,KeyMap)]
eprimeish = do
  let all_ops = fieldKeys (Proxy :: Proxy (Op Constant))
      all_tys = fieldKeys (Proxy :: Proxy Type)
      all_map = M.fromList ( [ (k, 0)  | k <- all_ops ++ all_tys ] )

  let keep = [ K_TypeInt, K_TypeBool, K_TypeMatrix
             , K_OpEq, K_OpPow, K_OpIndexing
             ]

  let res = KeyMap $ M.fromList (map (\x -> (x,100)) keep) `M.union`  all_map

  [("eprimeish", res)]

byType :: Int -> [(String,KeyMap)]
byType size = do
  let all_tys = fieldKeys (Proxy :: Proxy Type)
  let to_test = [ K_TypeInt, K_TypeTuple, K_TypeMatrix
                , K_TypeSet, K_TypeMSet, K_TypeFunction
                , K_TypeRelation, K_TypePartition]

  let subs_ = [  K_TypeBool : ts | ts <- subsequences to_test
              ,       length ts == (size -1)
              ]
  let subs  = [ (names xs, typeFixes xs) | xs <- subs_ ]

  let all_tys_map = M.fromList ( [ (ty, 0)  | ty <- all_tys ] )

  let maps = [ (name, KeyMap $ M.fromList (map (\x -> (x,100)) xs)
                               `M.union`  all_tys_map )
             | (name,xs) <- subs  ]

  maps

  where

  names = intercalate "-" . map (drop 6 .  show)

  typeFixes xs | K_TypeMatrix `elem` xs || K_TypeMSet `elem` xs  =
    if K_TypeInt `notElem` xs then
        K_TypeInt : xs
    else
        xs

  typeFixes xs = xs
