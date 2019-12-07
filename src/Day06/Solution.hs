module Day06.Solution (main) where

import Data.List (find, elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)

import Shared (getLines)

getTuple :: String -> (String,String)
getTuple s = (a,b)
  where [a,b] = splitOn ")" s

countOrbits :: Int -> [(String,String)] -> String -> Int
countOrbits c ps p = c + sum satelites
  where satelites = map (countOrbits (c + 1) ps . snd) . filter ((==p) . fst) $ ps

getPath :: [(String,String)] -> String -> [String]
getPath ps p = p : if center == "COM" then ["COM"] else getPath ps center
  where (center,_) = fromJust $ find ((==p) . snd) ps

findFirstCommon :: [String] -> [String] -> String
findFirstCommon xs ys = fromJust $ find (\x -> isJust $ find (==x) ys) xs

main :: IO ()
main = do
  lines <- getLines "src/Day06/input.txt"
  let ps = map getTuple lines
      part1Answer = countOrbits 0 ps "COM"
  print part1Answer
  let myPath = getPath ps "YOU"
      santaPath = getPath ps "SAN"
      closestParent = findFirstCommon myPath santaPath
      myDist = fromJust $ elemIndex closestParent myPath
      santaDist = fromJust $ elemIndex closestParent santaPath
      part2Answer = myDist + santaDist - 2
  print part2Answer
  
