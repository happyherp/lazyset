
module LazySet where 

import qualified Data.List as List
import qualified Data.Set as NSet
import Data.Ord(comparing)

import qualified Data.Foldable as Foldable


data Direction = ASC | DESC
data LazySet a = LazySet Direction [NSet.Set a]
    
instance Foldable.Foldable (LazySet) where 

    foldMap f (LazySet _ sets) =  undefined -- foldMap $  foldMap sets
    
    
member :: Ord a => a -> LazySet a ->  Bool
member e (LazySet dir sets) = findIn sets
    where findIn [] = False
          findIn (set:rest) = case lookup e set of 
            Nothing -> False
            Just x ->  x == e || findIn rest
          lookup = case dir of ASC -> NSet.lookupLE; DESC -> NSet.lookupGE
                                 
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
    
    
fromListAsc :: Ord a => [a] -> LazySet a
fromListAsc xs = LazySet ASC (build 0 (checkDir xs))
    where checkDir (a:b:s)| a > b = error "Elements must be ascending." 
          checkDir  xs = xs                  


fromList :: Ord a => [a] -> LazySet a
fromList = fromListAsc
          
          
fromListDesc :: Ord a => [a] -> LazySet a
fromListDesc xs = LazySet DESC (build 0 (checkDir xs))
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
