-- Thanks to Michael Gilliland
-- https://www.youtube.com/watch?v=nbJtV2K3_3k
module Day10.Solution
  ( main
  ) where

import Control.Monad (guard)
import Data.Function (on)
import Data.List (groupBy, maximumBy, nubBy, sortBy, sortOn)

import Shared (getLines)

type Coord = (Int, Int)

type Slope = Float

type XDir = Float

type YDir = Float

data Dir
  = Pos
  | Neg
  deriving (Eq, Ord, Show)

type Angle = (Slope, Dir)

getCoords :: [String] -> [Coord]
getCoords lines = do
  (y, row) <- zip [0 ..] lines
  (x, chr) <- zip [0 ..] row
  guard $ chr == '#'
  pure (x, y)

getAngle :: Coord -> Coord -> (Coord, Angle)
getAngle (x0, y0) (x1, y1) = ((x1, y1), (numer / denom, xdir))
  where
    numer = fromIntegral (y1 - y0)
    denom = fromIntegral (x1 - x0)
    xdir =
      if denom >= 0
        then Pos
        else Neg

getAnglesList :: [Coord] -> [(Coord, [(Coord, Angle)])]
getAnglesList coords = map getAngles coords
  where
    getAngles coord =
      let coords' = filter ((/=) coord) coords
       in (coord, map (getAngle coord) coords')

countUniqAngles :: (Coord, [(Coord, Angle)]) -> Int
countUniqAngles = length . nubBy ((==) `on` snd) . snd

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

distance :: Coord -> Coord -> Float
distance (x0, y0) (x1, y1) =
  sqrt $ fromIntegral (((x1 - x0) ^ 2) + ((y1 - y0) ^ 2))

groupByDir :: [(Coord, Angle)] -> [[(Coord, Angle)]]
groupByDir = groupOn (snd . snd) . sortOn (snd . snd)

groupByAngle :: [(Coord, Angle)] -> [[(Coord, Angle)]]
groupByAngle = groupOn (fst . snd) . sortOn (fst . snd)

blastOrder :: Coord -> [(Coord, Angle)] -> [(Coord, Angle)]
blastOrder station asteroids =
  sortBy (compare `on` (distance station . fst)) asteroids

attack :: Int -> [[(Coord, Angle)]] -> Coord
attack c ((target:rest):battlefield) =
  if c == 200
    then (fst target)
    else attack (c + 1) (battlefield ++ [rest])

main :: IO ()
main = do
  lines <- getLines "src/Day10/input.txt"
  let coords = getCoords lines
      anglesList = getAnglesList coords
      bestLOSCount = maximumBy (compare `on` countUniqAngles) anglesList
      part1Answer = countUniqAngles bestLOSCount
  print part1Answer
  let station = fst bestLOSCount
      asteroids = snd bestLOSCount
      [r, l] =
        map (map (blastOrder station) . groupByAngle) $ groupByDir asteroids
      battlefield = r ++ l
      (x, y) = attack 1 battlefield
      part2Answer = (x * 100) + y
  print part2Answer
