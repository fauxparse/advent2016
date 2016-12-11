module Seven where

import Data.String.Utils
import Data.List

supportsTLS :: String -> Bool
supportsTLS string = (any abba supernets) && (not $ any abba hypernets)
  where (supernets, hypernets) = stripe $ sections string

sections :: String -> [String]
sections string = concat $ map (split "[") $ split "]" string

stripe :: [a] -> ([a], [a])
stripe [] = ([], [])
stripe [x] = ([x], [])
stripe (x:y:z) = (x:xs, y:ys) where (xs, ys) = stripe z

abba :: String -> Bool
abba (a:b:c:d:_)
  | a /= b && [a, b] == [d, c] = True
abba (_:xs) = abba xs
abba _ = False

aba :: String -> [String]
aba (a:b:c:xs)
  | a == c && a /= b = [[a, b, a]] ++ (aba $ b:c:xs)
aba (_:xs) = aba xs
aba _ = []

supportsSSL :: String -> Bool
supportsSSL string = not . null $ abas `intersect` babs
  where
    (supernets, hypernets) = stripe $ sections string
    abas = concatMap aba supernets
    babs = map (\(b:a:_) -> [a, b, a]) $ concatMap aba hypernets

sevenA :: IO Int
sevenA = do
  input <- readFile "../input/7.txt"
  return $ length $ filter supportsTLS $ lines input

sevenB :: IO Int
sevenB = do
  input <- readFile "../input/7.txt"
  return $ length $ filter supportsSSL $ lines input
