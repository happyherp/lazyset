{-| 
Module      : LazySet
Description : A truly lazy Set. 
Copyright   : (c) Carlos Freund, 2016
License     : MIT
Maintainer  : carlosfreund@gmail.com
Stability   : experimental

A Set that can be created from lazy, ordered, infinite lists. 
-}
module Data.Set.Lazy where 

import Prelude hiding(lookup)
import qualified Data.List as List
import qualified Data.Set as NSet
import qualified Data.List.Ordered as OList
import Data.Maybe (isJust)
import Data.Ord

    
data LazySet a = LazySet [NSet.Set a]
    deriving (Eq, Show)
    
-- * Query    

-- | Checks if the value is a member of the 'LazySet'. 
-- Performance: O(m)=log m  
--Where m is the position of the element beeing searched for. 
-- This only applies after the element has been fetched from the underlying list.
member :: Ord a => a -> LazySet a ->  Bool
member e set = isJust $ lookup e set             
    
-- | Searches for a value in a Set. If it can not be found returns 'Nothing' otherwhise 
-- Returns 'Just a' if it can find it. The returned value will be the one from the set, not the one that was passed. 
lookup :: Ord a => a -> LazySet a -> Maybe a
lookup e (LazySet sets) = findIn sets
    where findIn [] = Nothing
          findIn (set:rest) = case NSet.lookupLE e set of 
            Nothing -> Nothing
            Just x ->  if (x == e) then Just x else findIn rest  


-- | Returns true if the 'LazSet' is empty. 
null :: LazySet a -> Bool
null (LazySet (x:xs)) = True
null _ = False

-- | Returns the size of the set. Do not use this on infinite Sets.
size :: Ord a => LazySet a -> Int
size = length . toList 

-- * Combine
    
-- | Splits the 'LazySet' into two parts. 
-- The first containing all consecutive elements of the Set where the predicate applies.
-- The second contains the (infinite) rest. 
spanAntitone :: Ord a => (a -> Bool) -> LazySet a -> (LazySet a, LazySet a)
spanAntitone pred (LazySet sets) = let
    (lesser, (middle:higher)) = List.span (pred . NSet.findMax) sets
    (middleLesser, middleHigher) = NSet.spanAntitone pred middle
    in (LazySet (lesser++[middleLesser]), LazySet (middleHigher:higher))
    
-- | Union of two LazySets.     
union :: Ord a => LazySet a -> LazySet a -> LazySet a
union s1 s2 = let  
    in fromList $ OList.union (toList s1) (toList s2)


-- * Build


-- | Create an 'empty' 'LazySet'.    
empty :: Ord a => LazySet a    
empty = fromList []
    
  
-- | Builds a 'LazySet' from an ascending ordered list.
-- If the list is not ordered an error is thrown. 
fromAscList :: Ord a => [a] -> LazySet a
fromAscList = growFromAscList 2.0               


-- | Like 'fromAscList' but with a custom growth-factor. 
growFromAscList :: Ord a => 
    Float   -- ^ The factor by which the subtrees grow. 
    --Must be >= 1.0. A growth of 1.0 makes the 'LazySet' behave like a List. 
    -- The higher it is set, the more it behaves like a 'Data.Set'.
    -- The downside of a higher growth-factor is that bigger batches are extracted from the source-list at once. 
    -> [a]  -- ^ An ascending List
    -> LazySet a     
growFromAscList growth _ | growth < 1.0 = error "growth must be at least 1" 
growFromAscList growth xs = LazySet (build 0 growth (checkDir xs))
    where checkDir (a:b:s)| a > b = error "Elements must be ascending." 
          checkDir  xs = xs   

-- | Alias for 'fromAscList'.
fromList :: Ord a => [a] -> LazySet a
fromList = fromAscList
          
-- | Create a 'LazSet' from a descending list.           
fromDescList :: Ord a => [a] -> LazySet (Down a)
fromDescList xs = fromAscList (map Down xs)
    
-- | Kind of internal. 
build :: Ord a => Int -- ^ starting-depth 
    -> Float -- ^ growth-factor
    -> [a] -- ^Ascending source-list 
    -> [NSet.Set a]         
build _ _ [] = []
build level growth xs = let 
    (elementsForThisLevel, elementsFurtherDown) = List.splitAt (ceiling $ growth^level) xs
    in (NSet.fromAscList elementsForThisLevel):(build (level + 1) growth elementsFurtherDown)
    
    
-- * Export
      
-- | List with all elements in order.
toList :: Ord a => LazySet a -> [a]
toList (LazySet sets) = concat $ map NSet.toAscList sets
   
