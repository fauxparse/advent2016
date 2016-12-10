module Four where

import Data.String.Utils
import Data.List

type Room = (String, Int, String)

roomName :: Room -> String
roomName (name, _, _) = name

sectorID :: Room -> Int
sectorID (_, sector, _) = sector

checksum :: String -> String
checksum name = map letter $ take 5 (sortBy compareCounts (counts name))

letter :: (Char, Int) -> Char
letter (c, _) = c

counts :: String -> [(Char, Int)]
counts name = nub [(letter, (length [c | c <- name, c == letter])) | letter <- name, not (letter == '-') ]

compareCounts :: (Char, Int) -> (Char, Int) -> Ordering
compareCounts (first, firstCount) (second, secondCount)
  | firstCount > secondCount = LT
  | firstCount < secondCount = GT
  | first < second           = LT
  | first > second           = GT
  | otherwise                = EQ

isRoom :: Room -> Bool
isRoom (name, _, purportedChecksum) = purportedChecksum == (checksum name)

parseRoom :: String -> Room
parseRoom line = (name, sector, checksum)
  where
    name = concat (intersperse "-" (init nameAndSector))
    sector = read (last nameAndSector)
    checksum = last parts
    parts = split "[" (init line)
    nameAndSector = split "-" (head parts)

rotate :: Char -> Char
rotate '-' = ' '
rotate ' ' = ' '
rotate 'z' = 'a'
rotate c = succ c

decode :: Room -> String
decode (name, sector, _) = [(head (drop (mod sector 26) (iterate rotate letter))) | letter <- name]

fourA :: IO Int
fourA = do
  list <- readFile "../input/4.txt"
  return $ sum (map sectorID (filter isRoom (map parseRoom (lines list))))

fourB :: IO Int
fourB = do
  list <- readFile "../input/4.txt"
  return $ head [sectorID room | room <- (filter isRoom (map parseRoom (lines list))), (decode room) == "northpole object storage"]
