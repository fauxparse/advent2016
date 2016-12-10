module Five where

import Data.List
import Data.Hash.MD5

interestingHashes :: String -> [String]
interestingHashes roomCode = [hash | hash <- (map (\i -> (md5s . Str) (concat [roomCode, (show i)])) [0..]), (take 5 hash) == "00000"]

decodableHashes :: String -> Int -> [String]
decodableHashes roomCode n = take n $ nubBy (\a b -> (sixth a) == (sixth b)) [hash | hash <- interestingHashes roomCode, elem (sixth hash) (take n ['0'..])]

assemble :: String -> Int -> String
assemble roomCode n = map seventh (sortBy (\a b -> compare (sixth a) (sixth b)) (decodableHashes roomCode n))

sixth :: [a] -> a
sixth as = as !! 5

seventh :: [a] -> a
seventh as = as !! 6

fiveA :: String -> String
fiveA roomCode = map sixth (take 8 (interestingHashes roomCode))

fiveB :: String -> String
fiveB roomCode = assemble roomCode 8
