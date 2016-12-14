import Prelude hiding(lookup)
import Test.HUnit
import System.Exit
import Data.Map.Lazier


mymap :: Map Int String
mymap = fromList $ map (\i->(i, show i)) [1..]
 
tests = TestList [
    TestCase $ assertEqual "Present 1"      (lookup   1 mymap) (Just "1" ),
    TestCase $ assertEqual "Present 111"    (lookup 111 mymap) (Just "111"),
    TestCase $ assertEqual "Not Present -1" (lookup(-1) mymap) Nothing 
    ] 
    
      
main = do
    result <- runTestTT tests
    let allpassed = (errors result + failures result) == 0
    if allpassed then exitSuccess else exitFailure   