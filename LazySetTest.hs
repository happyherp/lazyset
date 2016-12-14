import Test.HUnit
import System.Exit
import Data.Set.Lazy

tests = TestList [basic, fizzbuzztest, infinity]



---- BASIC -----

basic = TestList [noZero, oneToTen, noEleven]
    where   
    toTenList = [1..10]
    toTenSet = fromList toTenList

    noZero   = TestCase $ assertBool "Zero not in there" $not (member 0 toTenSet)
    oneToTen = TestCase $ assertBool "1 to 10 present in set." $ all (\i -> member i toTenSet) toTenList
    noEleven = TestCase $ assertBool "11 not in there" $not (member 11 toTenSet)
    
    
----- INFINITY----
                    
evenNumberSet = fromList $ filter even [1..]         
                     
infinity = TestList [
    TestCase ( assertBool "Even numbers are in there " 
        (all (\i -> member i evenNumberSet) [2,4,6,100,10^4])),
    TestCase ( assertBool "Odd numbers are in there " 
        (all (\i ->not $ member i evenNumberSet) [1,3,5,99,10^4+1]))
    ] 
    
        
---  FizzBuzz ---

isFizzBuzz x = x `mod` 7 == 0 || '7' `elem` show x       

fizzbuzzes = fromList $ filter isFizzBuzz [1..]

fizzbuzztest = TestList[
    TestCase ( assertBool "fizzes" 
        (all (\i ->       member i fizzbuzzes) [7,14,21,70,71,77])),
    TestCase ( assertBool "does not fizz" 
        (all (\i -> not $ member i fizzbuzzes) [1,2,3,8,9,10,16,22,100]))    ]
        
                     
main = do
    result <- runTestTT tests
    let allpassed = (errors result + failures result) == 0
    if allpassed then exitSuccess else exitFailure   