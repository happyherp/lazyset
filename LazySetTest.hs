import Test.HUnit
import System.Exit
import LazySet

tests = TestList [basic, infinity]


---- BASIC -----

basic = TestList [noZero, oneToTen, noEleven]

toTenList = [1..10]
toTenSet = fromList toTenList

noZero   = TestCase $ assertBool "Zero not in there" $not (member 0 toTenSet)
oneToTen = TestCase $ assertBool "1 to 10 present in set." $ all (\i -> member i toTenSet) toTenList
noEleven = TestCase $ assertBool "11 not in there" $not (member 11 toTenSet)


----- INFINITY

                     
evenNumberSet = fromList $ filter even [1..]                     
                     
infinity = TestList [
    TestCase ( assertBool "Even numbers are in there " 
        (all (\i -> member i evenNumberSet) [2,4,6,100,10^4])),
    TestCase ( assertBool "Odd numbers are in there " 
        (all (\i ->not $ member i evenNumberSet) [1,3,5,99,10^4+1]))
    ]
        
                     
main = do
    result <- runTestTT tests
    let allpassed = (errors result + failures result) == 0
    if allpassed then exitSuccess else exitFailure   