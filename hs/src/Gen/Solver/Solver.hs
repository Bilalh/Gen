{-# LANGUAGE QuasiQuotes, TupleSections #-}
module Gen.Solver.Solver(solver,solverMain, SolverArgs(..), Solution) where

import Conjure.Language.Definition
import Conjure.Language.Instantiate
import Conjure.UI.IO
import Gen.Imports
import Gen.Solver.Enumerate
import Conjure.Language.NameResolution (resolveNames)
import Gen.Helpers.InlineLettings

import qualified Data.Set as S

type Solution  = Model
type DomValues = [(Name, [Constant])]
type Assigment = (Name, Constant)

data Trie a =
      TSome Name [a] (Trie a)
    | TNone
    deriving(Show)

data SolverArgs = SolverArgs {
      essencePath   :: FilePath
    , solutionPath  :: FilePath
    , printSolution :: Bool
    } deriving Show


solverMain :: SolverArgs -> IO ()
solverMain SolverArgs{..} = do
  model <- readModelFromFile essencePath
  runLoggerIO LogNone $ solver model >>= \case
    Nothing -> do
      liftIO $ putStrLn "No solution found"
    (Just solution) -> do
      liftIO $ putStrLn $ "Solution @ " ++ solutionPath
      liftIO $ when printSolution $ print . pretty $ solution
      writeModel (Just solutionPath) solution


solver :: (MonadFail m, MonadLog m ) =>  Model -> m (Maybe Solution)
solver modelStart = do
  modelNamed <- ignoreLogs . runNameGen $ resolveNames modelStart
  let model = inlineLettings modelNamed

  -- Get the domains
  let domsE = [ (n, dom) | (Declaration (FindOrGiven Find n dom)) <- mStatements model ]
  doms :: DomValues <- forM domsE $ \(name,dom) -> do
                    res  <- instantiateDomain [] dom
                    vals <- enumerateDomain res
                    return (name,vals)

  -- Get the constraints, and categorise by variables used
  let exprs = concat [ xs | (SuchThat xs) <- mStatements model ]
  let ordered = [ (S.fromList $  allVarsUsed (S.fromList $ (map fst) domsE) c, c)
                | c <- exprs
                ]
  let trie = mkTrie ( map fst domsE) ordered

  let noVars = [ x | (s, x) <- ordered, S.null s ]

  logInfo $ "ordered" <+> (pretty $ groom  ordered)
  logInfo $ "noVars"  <+> (vcat $ map pretty noVars)
  logInfo $ "doms"    <+> (vcat $ map (pretty . groom)  doms)
  logInfo $ "trie"    <+> (pretty $ groom trie)

  case violates noVars [] of
    True  -> return Nothing
    False -> do
      -- Do the search
      let assigments = dfsSolve doms trie
      logInfo $ "assigments" <+> ( pretty $ groom assigments)

      case assigments of
        Nothing    -> return Nothing
        (Just assigned) -> do
          return $ Just $ createSolution $ assigned


-- The search
dfsSolve :: DomValues-> Trie Expression -> Maybe [Assigment]
dfsSolve a b = solve a b []
  where
  solve :: DomValues -> Trie Expression -> [Assigment] -> Maybe [Assigment]
  solve [] _ []  = Nothing   -- No Variables
  solve [] _ env = Just env  -- Assigned all variables successfully


  -- Variables without any constraints
  -- Assign the first value in it's domain
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
          case solve drest trest newEnv of
            Just jenv -> Just jenv
            Nothing -> solve ( (dname, xs) : drest ) trie env


  updateEnv :: [Assigment] -> (Name,Constant) -> [Assigment]
  updateEnv env val = val : env


-- Returns True if any constraint is not satisfied
violates  :: [Expression] -> [Assigment] -> Bool
violates xs vals =
  any violate xs

  where
  violate x =
    case instantiateExpression (map ( \(a,b) -> (a,Constant b) ) vals) x of
      Nothing -> lineError $line ["can not instantiateExpression"
                                 , "expr:" <+> pretty x
                                 , "vals:" <+> (vcat $ map pretty vals)
                                 ]
      (Just (ConstantBool False)) -> True
      (Just (ConstantBool True)) -> False
      (Just c) -> lineError $line ["Not a constant" <+> pretty c ]


allVarsUsed ::  Set Name -> Expression ->  [Name]
allVarsUsed varNames expr  =
    [ nm
    | (Reference nm _) <- universe expr
    , nm `S.member` varNames
    ]


mkTrie :: [Name] -> [(Set Name,a)] -> Trie a
mkTrie [] _ = TNone
mkTrie (x:xs) cs =
  let (hasX,noX) = partition (\(set,_) ->  x `S.member`  set && S.size set == 1 ) cs
  in  TSome x (map snd hasX)
        $ mkTrie xs (map (\(s,v) -> (x `S.delete` s, v) ) noX)


createSolution :: [(Name,Constant)] -> Solution
createSolution xs = def{mStatements= [ Declaration $ (Letting n) (Constant e)
                                     | (n,e) <- sortOn fst xs ] }

_run :: FilePath -> IO ()
_run fp = do
  model <- readModelFromFile fp
  solution <- runLoggerIO LogNone $ solver model
  print fp
  print . pretty $ model
  print . pretty $ solution
  putStrLn "---"
  putStrLn ""

_test :: IO ()
_test = do
  let fps = ["/Users/bilalh/Desktop/Results/_notable/solver/e.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/a.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/b.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/c.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/d.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/f.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/g.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/h.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/n.essence"
            ]
  mapM_ _run fps

_z :: IO ()
_z = _run "/Users/bilalh/Desktop/Results/_notable/solver/h.essence"