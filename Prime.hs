module Prime where 
import LazySet
import Data.Ord(comparing)

   
                      
-- returns a list of all products of the given factors in ascending order(assuming factors is ascending as well)    
allProducts factors = let 
    f n = let 
        (first:others) = products n factors
        more = f (n+1)
        in first:(merge others more)
    in f 2
            
products n factors = map product (compositeTerms n factors)
                
                
--uses only at most n factors                     
compositeTerms :: Integral a => Int -> LazySet a -> LazySet [a]
compositeTerms 1 factors = map (:[]) factors
compositeTerms n [] = []
compositeTerms n factors = let 
   first = head factors
   (lowest:rest) = map (first:) (compositeTerms (n-1) factors)
   withOneFactorLess = compositeTerms n (tail factors)
   in lowest:(mergeBy (comparing product) rest withOneFactorLess)
   
   
allCompositeTerms factors  = let 
    f n = let 
        (first:others) = compositeTerms n factors
        more = f (n+1)
        in first:(mergeBy (comparing product) others more)
    in f 2
    
    
composites = allProducts (2:primes)
                
primes :: LazySet Integer
primes = difference (fromListAsc [2..]) composites

isPrime :: Integer -> Bool
isPrime n = member n primes