{-# LANGUAGE QuasiQuotes, TupleSections #-}
module Gen.Solver.Solver where

import Conjure.Language.Definition
import Conjure.Language.Instantiate
import Conjure.UI.IO
import Gen.Imports
import Gen.Solver.Enumerate
import Conjure.Process.LettingsForComplexInDoms
import Conjure.Language.NameResolution (resolveNames)

import qualified Data.Set as S

type Solution  = Model
type DomValues = [(Name, [Constant])]
type Assigment = (Name, Constant)

data Trie a =
      TSome Name [a] (Trie a)
    | TNone
    deriving(Show)


solveMain :: (MonadFail m, MonadLog m ) =>  Model -> m (Maybe Solution)
solveMain modelStart = do
  modelNamed <- ignoreLogs . runNameGen $ resolveNames modelStart
  model <- inlineLettingDomainsForDecls modelNamed

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
    let (Just res) = instantiateExpression (map ( \(a,b) -> (a,Constant b) ) vals) x
     in case res of
          (ConstantBool False) -> True
          (ConstantBool True)  -> False


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
                                     | (n,e) <- sortOn snd xs ] }

run :: FilePath -> IO ()
run fp = do
  model <- readModelFromFile fp
  solution <- runLoggerIO LogNone $ solveMain model
  print fp
  print . pretty $ model
  print . pretty $ solution
  putStrLn "---"
  putStrLn ""

main :: IO ()
main = do
  let fps = ["/Users/bilalh/Desktop/Results/_notable/solver/e.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/a.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/b.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/c.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/d.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/f.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/g.essence"
            ,"/Users/bilalh/Desktop/Results/_notable/solver/h.essence"
            ]
  mapM_ run fps

z :: IO ()
z = run "/Users/bilalh/Desktop/Results/_notable/solver/h.essence"
