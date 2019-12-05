module Day04.Solution
  ( main
  ) where

import Data.List.Split (splitOn)

import Shared (getLines)

hasTwoEqualAdjacentDigits :: String -> Bool
hasTwoEqualAdjacentDigits = go
  where
    go :: String -> Bool
    go [] = False
    go [d] = False
    go (d0:d1:ds) =
      if d0 == d1
        then True
        else go (d1 : ds)

hasTwoEqualAndIsolatedAdjacentDigits :: String -> Bool
hasTwoEqualAndIsolatedAdjacentDigits = go 0
  where
    go :: Int -> String -> Bool
    go c [] = False
    go c [d] =
      if c == 1
        then True
        else False
    go c (d0:d1:ds) =
      if c == 1 && d0 /= d1
        then True
        else if d0 == d1
               then go (c + 1) (d1 : ds)
               else go 0 (d1 : ds)

hasIncreasingDigits :: String -> Bool
hasIncreasingDigits = go
  where
    go :: String -> Bool
    go [] = True
    go [d] = True
    go (d1:d2:ds) =
      if d1 > d2
        then False
        else go (d2 : ds)

part1Criteria :: Int -> Bool
part1Criteria n = hasTwoEqualAdjacentDigits nStr && hasIncreasingDigits nStr
  where
    nStr = show n

part2Criteria :: Int -> Bool
part2Criteria n =
  hasTwoEqualAndIsolatedAdjacentDigits nStr && hasIncreasingDigits nStr
  where
    nStr = show n

main :: IO ()
main = do
  [input] <- getLines "src/Day04/input.txt"
  let [lo, hi] = map read $ splitOn "-" input
      part1Answer = length $ filter part1Criteria [lo .. hi]
      part2Answer = length $ filter part2Criteria [lo .. hi]
  print part1Answer
  print part2Answer
