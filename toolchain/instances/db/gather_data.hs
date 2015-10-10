import Control.Arrow
import Data.List
import Data.Maybe
import System.Environment

main = do
    conjure_repo <- getEnv "REPOSITORY_BASE"
    args <- getArgs
    case args of
        [arg] -> interact
                    $ unlines
                    . (\ xs -> "BEGIN TRANSACTION;" : xs ++ ["COMMIT;"] )
                    . map (toSQL conjure_repo arg)
                    . mapMaybe parse
                    . filter nonHash
                    . lines
        _ -> error "Give me a spec name"

nonHash = not . ('#' `elem`)

parse line = case splitOn (== ':') blah of
    [attribute, value] -> Just [ eprime
                               , param
                               , filter (/=' ') attribute
                               , value
                               ]
    _                  -> Nothing

    where
        (eprime, line2) = second (drop 1) $ span (/='-') line
        param           = takeWhile (/='.') line2
        blah            = drop 1 $ dropWhile (/='\t') line

toSQL conjure_repo essence xs
    = id
    $ ("INSERT OR REPLACE INTO Experiment ( eprime, paramHash, attribute, value) VALUES " ++)
    $ (\ i -> "(" ++ i ++ ");" )
    $ intercalate "," $ map show (xs)


splitOn     :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'
