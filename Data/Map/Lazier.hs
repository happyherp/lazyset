{-| 
Copyright   : (c) Carlos Freund, 2016
License     : MIT
Maintainer  : carlosfreund@gmail.com
Stability   : experimental

A Map that can be created from lazy, ordered, infinite lists. 
Based on 'Data.Set.Lazy.LazySet'.
-}
module Data.Map.Lazier (

    -- * Types 
    Map,
    
    -- * Query 
    member,
    lookup,
    
    -- * Creation
    fromList
    
    )where 

import qualified Data.Set.Lazy as LS
import Data.Ord(compare)
import Prelude hiding(lookup)

-- | A mapping of type 'k' to type 'e'.
type Map k e = LS.LazySet (MapEntry k e) 

data MapEntry k e = MapEntry k e | JustKey k
    deriving Show

instance (Eq k) => Eq (MapEntry k e) where
    a == b = getKey a == getKey b

instance (Ord k) => Ord (MapEntry k e)  where 
    compare a b = compare (getKey a) (getKey b)    
    
getKey :: MapEntry k e -> k
getKey (MapEntry k e) = k
getKey (JustKey k) = k    

entryFromTuple :: Ord k => (k,e) -> MapEntry k e
entryFromTuple (k,e) = MapEntry k e
    
-- | Create a new Map from a list of tuples. The list must be sorted by key ascending.     
fromList :: Ord k => [(k,e)] -> Map k e
fromList xs = LS.fromList $ map entryFromTuple xs

-- | Check if a key exists in this Map. 
member :: Ord k => k -> Map k e -> Bool
member key = LS.member (JustKey key)

-- | Return the value of a given key or 'Nothing'
lookup :: Ord k => k -> Map k e -> Maybe e
lookup k map=  case LS.lookup (JustKey k) map of 
    Just (MapEntry _ e) -> Just e 
    _ -> Nothing