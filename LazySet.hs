
module LazySet where 

import qualified Data.List as List
import qualified Data.Set as NSet
import qualified Data.List.Ordered as OList
import Data.Ord(comparing)



data Direction = ASC | DESC
    deriving (Eq,Show)
    
data LazySet a = LazySet Direction [NSet.Set a]
    deriving (Eq, Show)
    
empty :: Ord a => LazySet a    
empty = fromList []
    
    
member :: Ord a => a -> LazySet a ->  Bool
member e (LazySet dir sets) = findIn sets
    where findIn [] = False
          findIn (set:rest) = case lookup e set of 
            Nothing -> False
            Just x ->  x == e || findIn rest
          lookup = case dir of ASC -> NSet.lookupLE; DESC -> NSet.lookupGE
      


spanAntitone :: Ord a => (a -> Bool) -> LazySet a -> (LazySet a, LazySet a)
spanAntitone pred (LazySet dir sets) = let
    (minmax, cpred)= if dir == ASC then (NSet.findMax,pred) else (NSet.findMin, not . pred)
    (lesser, (middle:higher)) = List.span (pred . minmax) sets
    (middleLesser, middleHigher) = NSet.spanAntitone cpred middle
    in (LazySet dir (lesser++[middleLesser]), LazySet dir (middleHigher:higher))
    
    
union :: Ord a => LazySet a -> LazySet a -> LazySet a
union s1@(LazySet d1 _) s2@(LazySet d2 _) | d1 == d2 = let  
    (fromList,cmp) = if d1 == ASC then (fromAscList, comparing id) 
                                  else (fromDescList, flip (comparing id))
    in fromList $ OList.unionBy cmp (toList s1) (toList s2)

      
{-         
lookupLT :: Ord a => a -> LazySet a -> Maybe a
lookupLT e (LazySet ASC sets) =  let 
  candidates =  List.takeWhile (\s -> NSet.findMin s < e) sets
  in foldMap (NSet.lookupLT e) $List.reverse candidates    
-}  

build :: Ord a => Int -> [a] -> [NSet.Set a]         
build _ [] = []
build level xs = let 
    growth = 2          
    (elementsForThisLevel, elementsFurtherDown) = List.splitAt (growth^level) xs
    in (NSet.fromList elementsForThisLevel):(build (level + 1) elementsFurtherDown)
    
    
fromAscList :: Ord a => [a] -> LazySet a
fromAscList xs = LazySet ASC (build 0 (checkDir xs))
    where checkDir (a:b:s)| a > b = error "Elements must be ascending." 
          checkDir  xs = xs                  


fromList :: Ord a => [a] -> LazySet a
fromList = fromAscList
          
          
fromDescList :: Ord a => [a] -> LazySet a
fromDescList xs = LazySet DESC (build 0 (checkDir xs))
    where checkDir (a:b:s)| a < b = error "Elements must be descending." 
          checkDir  xs = xs          
          
      
--List with all elements in the order of the set.
toList :: Ord a => LazySet a -> [a]
toList (LazySet dir sets) = concat $ map setToList sets
   where setToList = case dir of ASC -> NSet.toAscList; DESC -> NSet.toDescList
   
   
null :: LazySet a -> Bool
null (LazySet dir (x:xs)) = True
null _ = False

size = sum . toList 
