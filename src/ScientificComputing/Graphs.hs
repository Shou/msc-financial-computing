
module ScientificComputing.Graphs where

import Control.Monad (join, replicateM)
import Data.List (nub)
import Stats


randomSparseGraph :: Int -> Int -> IO [(Int, Int)]
randomSparseGraph n m = zip <$> randomList n (1, m) <*> randomList n (1, m)

isConnected :: [(Int, Int)] -> Int -> Int -> [[Int]]
isConnected graphA i j = map (++ [j])
                       . filter (not . null)
                       . join
                       $ map (isConnected' [i]) is
  where
    graphA' = nub graphA
    is = flip filter graphA' $ \(i', _) -> i == i'
    isConnected' :: [Int] -> (Int, Int) -> [[Int]]
    isConnected' hist (a, b)
      | j == b = [hist]
      | b `elem` hist = []
      | otherwise = filter (not . null) . join $ map (isConnected' (hist ++ [b])) bs
      where
        bs = flip filter graphA' $ \(a', _) -> b == a'

