module Day03.Solution
  ( main
  ) where

import Data.List (findIndex, intersect)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Shared (getLines)

plotLine :: (Int, Int) -> String -> [(Int, Int)]
plotLine (x, y) (dir:xs) =
  case dir of
    'R' -> zip (repeat x) [y + 1 .. y + dist]
    'L' ->
      if dist == 1
        then [(x, (y - 1))]
        else zip (repeat x) [y - 1,y - 2 .. y - dist]
    'U' -> zip [x + 1 .. x + dist] (repeat y)
    'D' ->
      if dist == 1
        then [((x - 1), y)]
        else zip [x - 1,x - 2 .. x - dist] (repeat y)
    otherwise -> error "Invalid input."
  where
    dist = read xs

getWirePath :: [String] -> [(Int, Int)]
getWirePath = foldl (\path move -> path ++ plotLine (last path) move) [(0, 0)]

-- Static list of intersections for part 2 because calculation is slow
cachedIntersections :: [(Int, Int)]
cachedIntersections =
  [ (-543, 525)
  , (-615, 468)
  , (-1459, 921)
  , (-1459, 1146)
  , (-1459, 1261)
  , (-1459, 1296)
  , (-1748, 1323)
  , (-1720, 1261)
  , (-1081, 1296)
  , (-1081, 1261)
  , (-1569, 1220)
  , (-1763, 921)
  , (-1430, 921)
  , (-2052, 372)
  , (-1948, 372)
  , (-1877, 372)
  , (-1464, 271)
  , (-656, -361)
  , (-656, -845)
  , (-1028, -196)
  , (-846, -315)
  , (-656, -877)
  , (-548, -1078)
  ]

countSteps :: (Int, Int) -> [(Int, Int)] -> Int
countSteps point = fromJust . findIndex (== point)

main :: IO ()
main = do
  [line1, line2] <- getLines "src/Day03/input.txt"
  let wire1Moves = splitOn "," line1
      wire2Moves = splitOn "," line2
  let wire1Path = getWirePath wire1Moves
      wire2Path = getWirePath wire2Moves
      intersections = intersect (tail wire1Path) (tail wire2Path)
      part1Answer = minimum $ map (\(x, y) -> (abs x) + (abs y)) intersections
      part2Answer =
        minimum $
        map
          (\point -> countSteps point wire1Path + countSteps point wire2Path)
          cachedIntersections
  return ()
