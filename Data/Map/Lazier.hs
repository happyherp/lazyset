module Data.Map.Lazier where 

import qualified Data.Set.Lazy as LS
import Data.Ord(compare)

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
    
fromList :: Ord k => [(k,e)] -> Map k e
fromList xs = LS.fromList $ map entryFromTuple xs


member :: Ord k => k -> Map k e -> Bool
member key = LS.member (JustKey key)

lookup :: Ord k => k -> Map k e -> Maybe e
lookup k map=  case LS.lookup (JustKey k) map of 
    Just (MapEntry _ e) -> Just e 
    _ -> Nothing