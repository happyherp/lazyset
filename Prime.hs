module Prime where 
import Data.List
import Data.List.Ordered
import Data.Ord(comparing)
  
  
import Debug.Trace(trace)  
  

  
  
--uses only at most n factors    
{-                 
products :: Integral a => Int -> [a] -> [a]
products 1 factors = factors
products n [] = []
products n factors = let 
   (lowest:rest) = map (* head factors) (products (n-1) factors)
   withOneFactorLess = products n (tail factors)
   in lowest:(merge rest withOneFactorLess)
      
  

products :: Integral a => Int -> [a] -> [a]
products 1 factors = factors
products n factors = let 
    f factor prods = let 
        (lowest:rest) = map (* factor) (products (n-1) factors)
        in lowest:(merge rest prods)
    in foldr f [] factors

   

products :: Integral a => Int -> [a] -> [a]
products 1 factors = factors
products n factors = let 
    f factor = let 
        (lowest:rest) = map (* factor) (products (n-1) factors)
        in (lowest:rest)
    in myMergeAll $  map f factors    
-}  

{-
products :: Integral a => Show a => Int -> [a] -> [a]
products n factors = map product $  products2 n factors


products2 :: Integral a => Show a => Int -> [a] -> [[a]]
products2 1 factors = map (:[]) factors
products2 n factors = let 
    step (lowest:rest) = map (lowest:) $ products2 (n-1) (lowest:rest)
    in myMergeAllBy (comparing product) $ map step $ init $ tails factors

-}

allProducts ::  Integral a => Show a => [a] -> [[a]]
allProducts factors =  let 
    productsByFactors = iterate addFactor $ map (:[]) factors
    addFactor products = myMergeAllBy (comparing product) $ map (\f->[f:term | term<-products, f <= head term]) factors 
    in myMergeAllBy (comparing product) (tail productsByFactors)
    
    
composites = map product $ allProducts primes
                
primes :: [Integer]
primes = 2:(minus [3..] composites)

isPrime :: Integer -> Bool
isPrime n = member n primes


--Data.List.Ordered.mergeAll hangs on circularly defined lists. 
myMergeAll :: Ord a => [[a]] -> [a]
myMergeAll = myMergeAllBy compare 


myMergeAllBy :: (a->a->Ordering) -> [[a]] -> [a]
myMergeAllBy _ [] = []
myMergeAllBy cmp ((x:xs):xxs) = x:(mergeBy cmp xs (myMergeAllBy cmp xxs))