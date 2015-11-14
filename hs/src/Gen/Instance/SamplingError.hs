{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric,
             DeriveTraversable, TypeFamilies #-}
module Gen.Instance.SamplingError where

import Conjure.Language.Pretty
import Gen.Imports             hiding (fail)

data SamplingErr = ErrRejectedPoint Doc
                 | ErrDuplicatedPoint Doc
                 | ErrCompact Doc
                 | ErrDB Doc
                 | ErrNoValuesLeft Doc
                 | ErrFailedToGenerateParam Doc
                 | ErrFailedRunSolve Doc
                 | ErrRace Doc
                 | ErrRaceSrInit Doc
                 | ErrGather Doc
                 | ErrFail Doc
                 | ErrAllSolutions Doc
  deriving (Eq, Show)

instance  Pretty SamplingErr  where
  pretty (ErrRejectedPoint d)         = hang "ErrRejectedPoint" 4 d
  pretty (ErrDuplicatedPoint d)       = hang "ErrDuplicatedPoint" 4 d
  pretty (ErrCompact d)               = hang "ErrCompact" 4 d
  pretty (ErrDB d)                    = hang "ErrDB" 4 d
  pretty (ErrNoValuesLeft d)          = hang "ErrNoValuesLeft" 4 d
  pretty (ErrFailedToGenerateParam d) = hang "ErrFailedToGenerateParam" 4 d
  pretty (ErrFailedRunSolve d)        = hang "ErrFailedRunSolve" 4 d
  pretty (ErrRace d)                  = hang "ErrRace" 4 d
  pretty (ErrRaceSrInit d)            = hang "ErrRaceSrInit" 4 d
  pretty (ErrGather d)                = hang "ErrGather" 4 d
  pretty (ErrFail d)                  = hang "ErrFail" 4 d
  pretty (ErrAllSolutions d)          = hang "ErrAllSolutions" 4 d
  -- pretty x                            = hang "noPretty" 4 (pretty $ groom x)