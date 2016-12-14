
module Data.Set.Lazy where 

import Prelude hiding(lookup)
import qualified Data.List as List
import qualified Data.Set as NSet
import qualified Data.List.Ordered as OList
import Data.Maybe (isJust)
import Data.Ord

    
data LazySet a = LazySet [NSet.Set a]
    deriving (Eq, Show)
    
empty :: Ord a => LazySet a    
empty = fromList []
    
lookup :: Ord a => a -> LazySet a -> Maybe a
lookup e (LazySet sets) = findIn sets
    where findIn [] = Nothing
          findIn (set:rest) = case NSet.lookupLE e set of 
            Nothing -> Nothing
            Just x ->  if (x == e) then Just x else findIn rest  
    
member :: Ord a => a -> LazySet a ->  Bool
member e set = isJust $ lookup e set
      


spanAntitone :: Ord a => (a -> Bool) -> LazySet a -> (LazySet a, LazySet a)
spanAntitone pred (LazySet sets) = let
    (lesser, (middle:higher)) = List.span (pred . NSet.findMax) sets
    (middleLesser, middleHigher) = NSet.spanAntitone pred middle
    in (LazySet (lesser++[middleLesser]), LazySet (middleHigher:higher))
    
    
union :: Ord a => LazySet a -> LazySet a -> LazySet a
union s1 s2 = let  
    in fromList $ OList.union (toList s1) (toList s2)



build :: Ord a => Int -> Float -> [a] -> [NSet.Set a]         
build _ _ [] = []
build level growth xs = let 
    (elementsForThisLevel, elementsFurtherDown) = List.splitAt (ceiling $ growth^level) xs
    in (NSet.fromAscList elementsForThisLevel):(build (level + 1) growth elementsFurtherDown)
    
    
fromAscList :: Ord a => [a] -> LazySet a
fromAscList = growFromAscList 2.0               

growFromAscList :: Ord a => Float -> [a] -> LazySet a     
growFromAscList growth _ | growth < 1.0 = error "growth must be at least 1" 
growFromAscList growth xs = LazySet (build 0 growth (checkDir xs))
    where checkDir (a:b:s)| a > b = error "Elements must be ascending." 
          checkDir  xs = xs   

fromList :: Ord a => [a] -> LazySet a
fromList = fromAscList
          
          
fromDescList :: Ord a => [a] -> LazySet (Down a)
fromDescList xs = fromAscList (map Down xs)
          
      
-- List with all elements in the order of the set.
toList :: Ord a => LazySet a -> [a]
toList (LazySet sets) = concat $ map NSet.toAscList sets
   
   
null :: LazySet a -> Bool
null (LazySet (x:xs)) = True
null _ = False

size = sum . toList 

