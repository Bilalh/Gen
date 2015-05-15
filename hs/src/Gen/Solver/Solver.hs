{-# LANGUAGE QuasiQuotes, TupleSections #-}
module Gen.Solver.Solver where

import Conjure.Language.Definition
import Conjure.Language.Instantiate
import Conjure.UI.IO
import Gen.Imports
import Gen.Solver.Enumerate

import qualified Data.Set as S

type Solution = Model

data Trie a =
      TSome Name [a] (Trie a)
    | TNone
    deriving(Show)


solveMain :: (MonadFail m, MonadLog m ) =>  Model -> m (Maybe Solution)
solveMain model = do
  -- Get the domains
  let domsE = [ (n, dom) | (Declaration (FindOrGiven Find n dom)) <- mStatements model ]
  doms :: [(Name, [Constant])] <- forM domsE $ \(name,dom) -> do
                    res  <- instantiateDomain [] dom
                    vals <- enumerateDomain res
                    return (name,vals)

  -- Get the constraints, and categorise by variables used
  let exprs = concat [ xs | (SuchThat xs) <- mStatements model ]
  let ordered = [ (S.fromList $  allVarsUsed (S.fromList $ (map fst) domsE) c, c)
                | c <- exprs
                ]
  let trie = mkTrie ( map fst domsE) ordered

  logInfo $ "doms" <+> (vcat $ map (pretty . groom)  doms)
  logInfo $ "trie" <+> (pretty $ groom trie)

  let assigments = dfsSolve doms trie
  logInfo $ "assigments" <+> ( pretty $ groom assigments)

  case assigments of
    Nothing    -> return Nothing
    (Just assigned) -> do
      return $ Just $ createSolution $ assigned



dfsSolve :: [(Name,[Constant])] -> Trie Expression -> Maybe [(Name,Constant)]
dfsSolve a b = solve a b []
  where
  solve :: [(Name,[Constant])] -> Trie Expression -> [(Name,Constant)] -> Maybe [(Name,Constant)]
  solve [] _ []  = Nothing   -- No Variables
  solve [] _ env = Just env  -- Assigned all variables successfully


  -- Variables without any constraints
  -- Assign the first value in its domain
  solve ds@(_:_) TNone env = let vs =  map f ds in
          case all isJust vs of
              True  -> Just $ catMaybes vs ++ env
              False -> Nothing

      where f (_, [])    = Nothing
            f (t, (e:_)) = Just (t, e)

  -- dfs search
  solve ( (dname, dvals) : drest) trie@(TSome _ cs trest) env =
      case dvals of
      []     -> Nothing  -- no values left in the domain

      (x:xs) -> let newEnv = updateEnv env (dname,x) in
          case violates cs newEnv of
              True  -> solve ( (dname, xs) : drest ) trie env
              False ->
                  case solve ( drest ) trest newEnv of
                      Just jenv -> Just jenv
                      Nothing -> solve ( (dname, xs) : drest ) trie env


  updateEnv :: [(Name,Constant)] -> (Name,Constant) -> [(Name,Constant)]
  updateEnv env val = val : env


-- Returns True if any constraint is not satisfied
violates  :: [Expression] -> [(Name,Constant)] -> Bool
violates xs vals =
  all violate xs

  where
  violate x =
    let (Just res) = instantiateExpression (map ( \(a,b) -> (a,Constant b) ) vals) x
    in  case res of
        (ConstantBool False)  -> True
        (ConstantBool True) -> False


allVarsUsed ::  Set Name -> Expression ->  [Name]
allVarsUsed varNames expr  =
    [ nm
    | (Reference nm _) <- universe expr
    , nm `S.member` varNames
    ]


mkTrie :: [Name] -> [( Set Name, a )] -> Trie a
mkTrie [] _ = TNone
mkTrie (x:xs) cs =
    let
        (hasX,noX) = partition (\(set,_) ->  x `S.member`  set && S.size set == 1 ) cs
    in
        TSome x (map snd hasX)
            $ mkTrie xs (map (\(s,v) -> (x `S.delete` s, v) ) noX)


createSolution :: [(Name,Constant)] -> Solution
createSolution xs = def{mStatements= [ Declaration $ (Letting n) (Constant e)
                                     | (n,e) <-xs ] }


run :: IO ()
run = do
  let fp = "/Users/bilalh/Desktop/Results/_notable/solver/a.essence"
  model <- readModelFromFile fp
  solution <- runLoggerIO LogNone $ solveMain model
  print . pretty $ solution


-- Unused

data SolveState = SolveState{
      assigned    :: [(Name,Expression)]
    , constraints :: [Expression]
    , domValues   :: [(Name, [Constant])]
    } deriving (Show)

instance Pretty SolveState where
    pretty = pretty . groom
