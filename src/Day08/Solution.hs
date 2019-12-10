module Day08.Solution (main) where

import Data.List (minimumBy)
import Data.List.Split (divvy)

import Shared (getLines)

imageW :: Int
imageW = 25

imageH :: Int
imageH = 6

imageL :: Int
imageL = imageW * imageH

countX :: Int -> [Int] -> Int
countX x = length . filter (==x)

getImages :: Int -> String -> [[Int]]
getImages n = map (map charToInt) . divvy n n
  where charToInt = read . (:[])

findFewest0s :: [[Int]] -> [Int]
findFewest0s = minimumBy (\a b -> count0s a `compare` count0s b)
  where count0s = countX 0

main :: IO ()
main = do
  [input] <- getLines "src/Day08/input.txt"
  let images = getImages imageL input
      fewest0s = findFewest0s images
      oneCount = countX 1 fewest0s
      twoCount = countX 2 fewest0s
  print $ oneCount * twoCount
