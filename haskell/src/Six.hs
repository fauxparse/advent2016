module Six where

import Data.List

mostCommon :: [Char] -> Char
mostCommon column = (head . last) $ sortOn length $ (group . sort) column

leastCommon :: [Char] -> Char
leastCommon column = (head . head) $ sortOn length $ (group . sort) column

sixA :: IO String
sixA = do
  list <- readFile "../input/6.txt"
  return $ map mostCommon $ (transpose . lines) list

sixB :: IO String
sixB = do
  list <- readFile "../input/6.txt"
  return $ map leastCommon $ (transpose . lines) list
