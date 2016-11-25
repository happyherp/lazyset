
module LazySet where 

import qualified Data.List as List
import qualified Data.Set as NSet
import qualified Data.List.Ordered as OList
import Data.Ord

    
data LazySet a = LazySet [NSet.Set a]
    deriving (Eq, Show)
    
empty :: Ord a => LazySet a    
empty = fromList []
    
    
member :: Ord a => a -> LazySet a ->  Bool
member e (LazySet sets) = findIn sets
    where findIn [] = False
          findIn (set:rest) = case NSet.lookupLE e set of 
            Nothing -> False
            Just x ->  x == e || findIn rest
      


spanAntitone :: Ord a => (a -> Bool) -> LazySet a -> (LazySet a, LazySet a)
spanAntitone pred (LazySet sets) = let
    (lesser, (middle:higher)) = List.span (pred . NSet.findMax) sets
    (middleLesser, middleHigher) = NSet.spanAntitone pred middle
    in (LazySet (lesser++[middleLesser]), LazySet (middleHigher:higher))
    
    
union :: Ord a => LazySet a -> LazySet a -> LazySet a
union s1 s2 = let  
    in fromList $ OList.union (toList s1) (toList s2)



build :: Ord a => Int -> [a] -> [NSet.Set a]         
build _ [] = []
build level xs = let 
    growth = 2          
    (elementsForThisLevel, elementsFurtherDown) = List.splitAt (growth^level) xs
    in (NSet.fromList elementsForThisLevel):(build (level + 1) elementsFurtherDown)
    
    
fromAscList :: Ord a => [a] -> LazySet a
fromAscList xs = LazySet (build 0 (checkDir xs))
    where checkDir (a:b:s)| a > b = error "Elements must be ascending." 
          checkDir  xs = xs                  


fromList :: Ord a => [a] -> LazySet a
fromList = fromAscList
          
          
fromDescList :: Ord a => [a] -> LazySet (Down a)
fromDescList xs = fromAscList (map Down xs)
          
      
--List with all elements in the order of the set.
toList :: Ord a => LazySet a -> [a]
toList (LazySet sets) = concat $ map NSet.toAscList sets
   
   
null :: LazySet a -> Bool
null (LazySet (x:xs)) = True
null _ = False

size = sum . toList 
