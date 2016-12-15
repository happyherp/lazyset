import Data.List


--Some calculations of the number of function calls necessary to do a lookup. 

o :: Double -> Int -> Int
o growth m = let 
    tree_count = length $ takeWhile (<=m) $  total_sizes growth
    tree_depth = ceiling(logBase growth (fromIntegral (tree_count-1)))
    in tree_count + tree_depth
    
   

total_sizes :: Double   ->[Int]
total_sizes growth =  map (total_size growth) [0..]


total_size growth 0 = 1
total_size growth level = total_size growth (level-1) + ceiling (growth^level)


tree_sizes :: Double   ->[Int]
tree_sizes growth = map (\level ->ceiling $ growth^level)[0..]
 