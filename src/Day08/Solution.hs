module Day08.Solution (main) where

import Data.List (find, intercalate, minimumBy, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)

import Shared (getLines)

imageW :: Int
imageW = 25

imageH :: Int
imageH = 6

imageL :: Int
imageL = imageW * imageH

countX :: Int -> [Int] -> Int
countX x = length . filter (==x)

getLayers :: Int -> String -> [[Int]]
getLayers n = map (map charToInt) . chunksOf n
  where charToInt = read . (:[])

findFewest0s :: [[Int]] -> [Int]
findFewest0s = minimumBy (\a b -> count0s a `compare` count0s b)
  where count0s = countX 0

placeLayers :: [[Int]] -> [Int]
placeLayers = catMaybes . map (find (\x -> x == 0 || x == 1)) . transpose

getImage :: [Int] -> String
getImage = intercalate "\n" . chunksOf imageW . map draw
  where draw x = if x == 0 then ' ' else 'X'

main :: IO ()
main = do
  [input] <- getLines "src/Day08/input.txt"
  let layers = getLayers imageL input
      fewest0s = findFewest0s layers
      oneCount = countX 1 fewest0s
      twoCount = countX 2 fewest0s
      part1Answer = oneCount * twoCount
      part2Answer = getImage $ placeLayers layers
  print part1Answer
  putStrLn part2Answer
  
