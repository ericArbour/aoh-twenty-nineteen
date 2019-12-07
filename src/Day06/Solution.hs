module Day06.Solution (main) where

import Data.List.Split (splitOn)

import Shared (getLines)

getTuple :: String -> (String,String)
getTuple s = (a,b)
  where [a,b] = splitOn ")" s

countOrbits :: Int -> [(String,String)] -> String -> Int
countOrbits c ps p = c + sum satelites
  where satelites = map (countOrbits (c + 1) ps . snd) . filter ((==p) . fst) $ ps

main :: IO ()
main = do
  lines <- getLines "src/Day06/input.txt"
  let ps = map getTuple lines
  print $ countOrbits 0 ps "COM"
