import Test.HUnit
import System.Exit
import LazySet


toTenList = [1..10]
toTenSet = fromList toTenList

noZero   = TestCase $ assertBool "Zero not in there" $not (member 0 toTenSet)
oneToTen = TestCase $ assertBool "1 to 10 present in set." $ all (\i -> member i toTenSet) toTenList
noEleven = TestCase $ assertBool "11 not in there" $not (member 11 toTenSet)

                     
tests = TestList [noZero, oneToTen, noEleven]
                     
                     
                     
main = do
    result <- runTestTT tests
    let allpassed = (errors result + failures result) == 0
    if allpassed then exitSuccess else exitFailure   