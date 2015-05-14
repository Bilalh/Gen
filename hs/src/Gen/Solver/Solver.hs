{-# LANGUAGE QuasiQuotes #-}
module Gen.Essence.Solver where

import Gen.Imports
import Conjure.UI.IO
import Conjure.Language.Definition
import Conjure.Language.Instantiate
import Conjure.Language.TH

type Solution = Model

data SolveState = SolveState{
      assigned :: [(Name,Expression)]
    , exprs :: [Expression]
    } deriving (Show)

instance Pretty SolveState where
    pretty = pretty . groom

solve :: (MonadFail m, MonadLog m ) =>  Model -> m (Maybe Solution)
solve model = do
  let doms  =  [ (n, dom) | (Declaration (FindOrGiven Find n dom)) <- mStatements model ]

  let start = SolveState{
        assigned = []
      , exprs    = concat [ xs | (SuchThat xs) <- mStatements model ]
      }

  fin :: (Bool, SolveState) <- (flip runStateT) (start) $ do
      end <- forM doms $ \(name,dom) -> do
        nextVal dom >>= \case
          Nothing -> return True
          Just val -> do
            modify $ \st -> st{assigned = (name,val) : assigned st }
            return True

      return $ and end


  logInfo ("fin" <+> pretty fin )
  -- return model
  case fin of
    (False, _) -> return Nothing
    (True, SolveState{..}) -> do
                logInfo (vcat $ map pretty $ assigned)
                return $ Just $ createSolution $ assigned


nextVal :: (MonadState SolveState m)
        => Domain () Expression
        -> m (Maybe Expression)
nextVal _ = return $  Just [essence| ({} : `set of bool`) |]


isTrue :: Constant -> Bool
isTrue (ConstantBool True) = True
isTrue _                   = False

createSolution :: [(Name,Expression)] -> Solution
createSolution xs = def{mStatements= [ Declaration $ (Letting n) e  | (n,e) <-xs ] }


run :: IO ()
run = do
  let fp = "/Users/bilalh/Desktop/Results/_notable/solver/a.essence"
  model <- readModelFromFile fp
  solution <- runLoggerIO LogNone $ solve model
  print . pretty $ solution
