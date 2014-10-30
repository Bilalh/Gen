{-# LANGUAGE PatternGuards #-}
module TestGen.Arbitrary.Helpers.Z where
import Control.Monad.State.Strict(State, gets, runStateT)

data MyState = MyState
    { counter :: Int
    } deriving (Show)


a :: State MyState String
a = do
    i <- gets counter
    case i of
        0 -> return "hello"
        1 -> return "bye"
-- 
-- a' :: State MyState String
-- a' | i <- gets counter, i == 0 = return "hello"



runA = runStateT a ( MyState{counter=0} )
