{-# LANGUAGE BangPatterns #-}

import System.Exit
import LazySet
import System.TimeIt

expensive n = let x = sum [1..4*10^5+n] in x-x+n


                     
main = do
    let evenList = filter even [1..]
    let r1 = member (2*10^6) $ fromList evenList
    timeIt $ putStrLn $ "finding one in a million." ++ show r1
    let set = fromList evenList
    let r2 = and $ map (\i-> member i set) (take 1000 [2*10^6..])
    timeIt $ putStrLn $ "finding a thousand in a million " ++ show r2
    let expensiveSet = fromList $ map expensive [1..]
    putStrLn "Going through a lazy set one by one. Watch the times to see when a new batch is loaded."
    sequence $ map (timeIt . print . (`LazySet.lookup` expensiveSet) ) [1..20]
    let flatSet = growFromAscList 1.3 $ map expensive [1..]
    putStrLn "This time the set if flatter."
    sequence $ map (timeIt . print . (`LazySet.lookup` flatSet) ) [1..20]    
    return ()
