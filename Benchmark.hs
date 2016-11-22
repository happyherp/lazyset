{-# LANGUAGE BangPatterns #-}

import System.Exit
import LazySet
import Data.Time.Clock



                     
main = do
    let evenList = filter even [1..]
    p1 <- getCurrentTime
    let !r1 = member (2*10^6) $ fromList evenList
    p2 <- getCurrentTime
    let diff1 = diffUTCTime p2 p1
    putStrLn $ "finding one in a million took " ++ show diff1
    let set = fromList evenList
    let !r2 = and $ map (\i-> member i set) (take 1000 [2*10^6..])
    p3 <- getCurrentTime 
    let diff2 = diffUTCTime p3 p2
    putStrLn $ "finding a thousand in a million took " ++ show diff2
    if diff1 * 3 < diff2 then (error "Should have been faster.") else return ()
