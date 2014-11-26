module TestGen.Examples.QC where 
    
    
import AST.SpecE
import Language.E hiding(trace)


import TestGen.Arbitrary.Domain(dom)
import TestGen.Prelude(SpecState, Generators,Domain, listOfBounds,SS(depth_),FG ,ArbSpec(..))


import qualified Data.Map as M
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Property as QC

import System.Directory(createDirectoryIfMissing, getHomeDirectory)
import System.FilePath((</>), (<.>), takeFileName)
import System.IO(IOMode(..),hPutStrLn, openFile, hClose)
import System.Random(randomRIO)

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Text.Groom(groom)

main = undefined



data Example = Example
    { myInt  :: Int 
    , myList :: [String]
    } deriving (Show)

data Gens =  Gens
    { gen1 :: Gen Int
    , gen2 :: Gen String }

arbExample :: Gens -> Gen Example
arbExample gens = do 
    i  <- gen1 gens 
    xs <- vectorOf i (gen2 gens)
    return Example{myInt=i, myList=xs}

newtype E2 = E2 Example 
    deriving (Show)

class Arbitrary a => Genable a where 
    tyGen :: a -> Gens
    getExample :: a -> Example

instance Genable E2 where 
    tyGen _ = Gens (return 1) (return "d")
    getExample (E2 ex) = ex
    
instance Arbitrary E2 where
    arbitrary = E2 <$> arbExample (tyGen (undefined :: E2))

prop_example :: Genable a => a -> Property
prop_example a =  do
    let example = getExample a
    let len = length (myList example) 
    monadicIO $ do 
        -- result of running some program
        successful <- run $ (\e -> return False) example  
        case successful of
            True  -> return ()
            False -> fail "failure "

exampleRun = 
     quickCheck (prop_example :: E2 -> Property )

prop_example2 :: Genable a => a -> a -> Property
prop_example2 _  a =  do
    let example = getExample a
    let len = length (myList example) 
    monadicIO $ do 
        -- result of running some program
        successful <- run $ (\e -> return False) example  
        case successful of
            True  -> return ()
            False -> fail "failure "


exampleRun2 unused = do
    quickCheck (prop_example2 unused)
    
    
newtype DD = DD (M.Map Text FG) 
    deriving(Show)

instance Arbitrary DD where
    arbitrary = do
        let depth = 0
            state = def{depth_= depth `div` 2}
            domsCount = (1, 2)
        (doms,_) <- runStateT  ( listOfBounds domsCount dom) state
        let withNames =  zipWith (\d i -> (name i , Find d)) doms [1 :: Int ..]
        let mappings  = M.fromList withNames
        return $ DD mappings
    
        where name i =  T.pack $  "var" ++  (show  i)
    
    
prop_DD_is_nonempty :: DD -> Property
prop_DD_is_nonempty (DD ds) = 
    counterexample
        ( show ds )
        (M.size ds >= 0 )
    
newtype AA a = AC [a]
    deriving (Show)

instance Arbitrary a => Arbitrary (AA a) where
    arbitrary = do
        let state = def{depth_= 5}
        u <- choose (1,100)
        (xs,_) <- runStateT  ( listOfBounds (1,u) (lift arbitrary) ) state
        return $ AC  xs
        
prop_AA_is_nonempty (AC ds)  = do 
    counterexample
        ( show ds )
        (length ds >= 0 )
