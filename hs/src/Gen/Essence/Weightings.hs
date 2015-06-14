module Gen.Essence.Weightings where

import Gen.Essence.St
import Gen.Imports
import Gen.IO.Formats

import qualified Data.Map      as M

save :: (MonadIO m,MonadFail m) => Directory -> [(String,KeyMap)] -> m ()
save dir f = do
  liftIO $ createDirectoryIfMissing True dir
  forM_ f $ \(name,mapping) -> do
    writeToJSON (dir </> name <.> ".json" ) mapping


defaults :: [(String,KeyMap)]
defaults = [("default", def)]

every :: [(String,KeyMap)]
every =  (\x -> [("all", x)]  ) . KeyMap . M.union def  . M.fromList $  zip allKeys (repeat 100)

byType :: Int -> [(String,KeyMap)]
byType size = do
  let all_tys = fieldKeys (Proxy :: Proxy Type)
  let to_test = [ K_TypeInt, K_TypeTuple, K_TypeMatrix
                , K_TypeSet, K_TypeMSet, K_TypeFunction
                , K_TypeRelation, K_TypePartition]

  let subs_ = [  K_TypeBool : ts | ts <- subsequences to_test
              ,       length ts == (size -1)
              ]
  let subs  = [ (names xs, matrixHandle xs) | xs <- subs_ ]

  -- let KeyMap defMap = def
  -- let all_tys_map = M.fromList ( [ (ty, 0)  | ty <- all_tys ] )  `M.union`  defMap
  let all_tys_map = M.fromList ( [ (ty, 0)  | ty <- all_tys ] )

  let maps = [  (name, KeyMap $ M.fromList (map (\x -> (x,100)) xs)  `M.union`  all_tys_map )
             | (name,xs) <- subs  ]

  maps

  where

  names = intercalate "-" . map (drop 6 .  show)

  matrixHandle xs | K_TypeMatrix `elem` xs =
    if K_TypeInt `notElem` xs then
        K_TypeInt : xs
    else
        xs

  matrixHandle xs = xs
