module Two where

import Data.String.Utils
import Data.Char

code :: [String] -> (Int, Int) -> String
code [] _ = ""
code ("":lines) (x, y) = ((key x y):(code lines (x, y)))
code ((c:cs):lines) (x, y) = code (cs:lines) (move c x y)

move :: Char -> Int -> Int -> (Int, Int)
move 'U' x y = (x, (max (y - 1) 1))
move 'D' x y = (x, (min (y + 1) 3))
move 'L' x y = ((max (x - 1) 1), y)
move 'R' x y = ((min (x + 1) 3), y)

key :: Int -> Int -> Char
key x y = intToDigit ((y - 1) * 3 + x)

codeB :: [String] -> (Int, Int) -> String
codeB [] _ = ""
codeB ("":lines) (x, y) = ((keyB x y):(codeB lines (x, y)))
codeB ((c:cs):lines) (x, y) = codeB (cs:lines) (moveB c x y)

moveB :: Char -> Int -> Int -> (Int, Int)
moveB 'U' x y = (x, (max (y - 1) ((abs (3 - x)) + 1)))
moveB 'D' x y = (x, (min (y + 1) (5 - (abs (3 - x)))))
moveB 'L' x y = ((max (x - 1) ((abs (3 - y)) + 1)), y)
moveB 'R' x y = ((min (x + 1) (5 - (abs (3 - y)))), y)

keyB :: Int -> Int -> Char
keyB 3 1 = '1'
keyB 2 2 = '2'
keyB 3 2 = '3'
keyB 4 2 = '4'
keyB 1 3 = '5'
keyB 2 3 = '6'
keyB 3 3 = '7'
keyB 4 3 = '8'
keyB 5 3 = '9'
keyB 2 4 = 'A'
keyB 3 4 = 'B'
keyB 4 4 = 'C'
keyB 3 5 = 'D'

removeBlanks :: [String] -> [String]
removeBlanks [] = []
removeBlanks ("":ss) = removeBlanks ss
removeBlanks (s:ss) = (s:(removeBlanks ss))

twoA :: IO String
twoA = do
  instructions <- readFile "../input/2.txt"
  return $ code (removeBlanks (split "\n" instructions)) (2, 2)

twoB :: IO String
twoB = do
  instructions <- readFile "../input/2.txt"
  return $ codeB (removeBlanks (split "\n" instructions)) (1, 3)
