module One where

import Data.String.Utils

data Direction = Anticlockwise | Clockwise | Ahead deriving Show
type Instruction = (Direction, Int)

type Location = (Int, Int, Int)

parse :: String -> [Instruction]
parse str = map parseInstruction $ map strip (split "," str)

parseInstruction :: String -> Instruction
parseInstruction ('L':blocks) = (Anticlockwise, read blocks)
parseInstruction ('R':blocks) = (Clockwise, read blocks)

turn :: Direction -> Location -> Location
turn Anticlockwise (x, y, facing) = (x, y, mod (facing + 3) 4)
turn Clockwise (x, y, facing) = (x, y, mod (facing + 1) 4)
turn Ahead location = location

forward :: Int -> Location -> Location
forward blocks (x, y, 0) = (x, y - blocks, 0)
forward blocks (x, y, 1) = (x + blocks, y, 1)
forward blocks (x, y, 2) = (x, y + blocks, 2)
forward blocks (x, y, 3) = (x - blocks, y, 3)

distance :: Location -> Int
distance (x, y, _) = (abs x) + (abs y)

starting :: Location
starting = (0, 0, 0)

follow :: Location -> Instruction -> Location
follow (x, y, facing) (direction, blocks) = forward blocks (turn direction (x, y, facing))

walk :: Location -> [Instruction] -> [(Int, Int)] -> Location
walk location [] _ = location
walk (x, y, facing) ((direction, blocks):instructions) visited
  | elem (x, y) visited = (x, y, facing)
  | blocks == 0         = walk (x, y, facing) instructions visited
  | otherwise           = walk (forward 1 (turn direction (x, y, facing))) ((Ahead, blocks - 1):instructions) ((x, y):visited)

oneA :: IO Int
oneA = do
  instructions <- readFile "../input/1.txt"
  return $ distance (foldl follow starting (parse instructions))

oneB :: IO Int
oneB = do
  instructions <- readFile "../input/1.txt"
  return $ distance (walk starting (parse instructions) [])
