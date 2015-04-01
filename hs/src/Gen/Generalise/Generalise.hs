module Gen.Generalise.Generalise where

import Gen.Generalise.Data
import Gen.Generalise.Runner
import Gen.Reduce.Data  hiding (RState(..))
import Gen.Prelude
import Gen.IO.Formats

import qualified Data.HashMap.Strict as H

generaliseMain :: GState -> IO GState
generaliseMain ee = do
  let base = specDir_ ee
      fp   =  base </> "spec.spec.json"

  sp :: Spec <- readFromJSON fp
  noteFormat "Starting with" [pretty sp]

  (sfin,state) <- (flip runStateT) ee $
      return sp
      >>= (note "generaliseLiterals") generaliseLiterals
      >>= \ret -> get >>= \g -> addLog "FinalState" [pretty g] >> return ret


  noteFormat "State" [pretty state]
  noteFormat "Start" [pretty sp]
  noteFormat "Final" [pretty sfin]

  return (state)

  where
  note tx f s = do
      noteFormat ("@" <+> tx <+> "Start") []

      newSp <- f s
      noteFormat ("@" <+> tx <+> "End") [pretty newSp]

      return newSp


generaliseLiterals :: Spec -> EE Spec
generaliseLiterals  sp = do
  return sp

recordResult :: RunResult -> EE ()
recordResult r= do
  modify $ \st -> st{choicesToUse_ =Just (resErrChoices_ r) }
  return ()
