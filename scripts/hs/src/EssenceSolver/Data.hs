module EssenceSolver.Data where

import Language.E

data SolverState = SolverState {
      sEssence :: Spec
    , sParam   :: Maybe Spec
    , sOutPath :: FilePath
    }
