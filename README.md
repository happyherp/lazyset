# lazyset
A Lazy Set and Map implemented in Haskell. 


Allows efficient, lazy lookups on sorted lists. The sort list may be of ininite size. 

The Source-List must 
+ contain elements that implement *Ord* 
+ must be ascending (or descending: see *fromDescList*)
+ either produce an infinite number of elements or terminate


##Set Sample usage
```haskell

import Data.Set.Lazy

set = fromAscList $ map (*3) [1..]

3 `member` set -> True
4 `member` set -> False
```


##Map Sample usage

```haskell

import Prelude hiding(lookup)
import Data.Map.Lazier


sqrtmap = fromList $ map (\i->(i, sqrt i)) [1..]
lookup 2 sqrtmap -> Just 1.4142135623730951

```

##Performance

Elements from the Source-List will be requested in batches of increasing size. By default the batch-size is increases by two. This would lead to batches of *1,2,4,8,16*. This can be changed by using *growFromAscList factor list*. For Example a factor of *1.3* casues the batches to be *1,2,2,3,3,4,5*.

Increasing the growth-factor reduces lookup times but increases the batch-size. When it is set to 1.0 it performs like a list.


lookup: O(m) = log m where m is the index of the element in the source-list. 


This breaks the set, because the underlying list stops producing elements. 
```haskell

set = fromList $ filter (<4) [1..]
5 `member` set

```
