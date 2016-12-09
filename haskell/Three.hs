module Three where

import Data.String.Utils
import Data.List

isTriangle :: [Int] -> Bool
isTriangle (a:(b:(c:[]))) = (a + b > c) && (b + c > a) && (c + a > b)
isTriangle _ = False

inThrees :: [[Int]] -> [[Int]]
inThrees (a:(b:(c:cs))) = concat [(transpose [a, b, c]), (inThrees cs)]
inThrees _ = []

threeA :: IO Int
threeA = do
  list <- readFile "../input/3.txt"
  return $ length (filter isTriangle (map (fmap read . words) (lines list)))

threeB :: IO Int
threeB = do
  list <- readFile "../input/3.txt"
  return $ length (filter isTriangle (inThrees (map (fmap read . words) (lines list))))
