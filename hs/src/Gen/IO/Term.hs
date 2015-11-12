module Gen.IO.Term where

import Gen.Imports
import Gen.IO.TermSize
import System.IO.Error(catchIOError)
import System.Environment(lookupEnv)

getColumns :: MonadIO m => m Int
getColumns = do
    cs :: Maybe Int <-  liftIO $ lookupEnv "COLUMNS" >>= \case
            Just s  -> return $ readMay s
            Nothing -> return Nothing

    case cs of
      Just i -> return i
      Nothing -> do
        (_,cols) <- liftIO $ catchIOError getTermSize
            (\_ -> return (40,80) )
        return cols


helpArg :: MonadIO m => m String
helpArg = do
  len <- liftIO getHelpLength
  return $ "--help=" ++ show len

replaceOldHelpArg :: MonadIO m => [String] -> m [String]
replaceOldHelpArg ins = mapM f ins
  where
    f "-h"     = helpArg
    f "-?"     = helpArg
    f "--help" = helpArg
    f other    = return other

getHelpLength :: MonadIO m => m Int
getHelpLength = do
    cols <- liftIO getColumns

    let helpLength =  getHelpLength' cols
    return helpLength
  where
    getHelpLength' :: Int -> Int
    getHelpLength' len
        | len > 121 = 121
        | len < 60  = 60
        | otherwise = len
