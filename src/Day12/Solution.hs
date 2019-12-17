-- Thanks to Michael Gilliland
-- https://www.youtube.com/watch?v=S1OmyL0dGSY
module Day12.Solution
  ( main
  ) where

import Data.List (notElem, unfoldr)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

import Shared (getLines)

parsePosition :: String -> (Int, Int, Int)
parsePosition = toTrip . catMaybes . map (readMaybe . cleanInput) . splitOn ", "
  where
    cleanInput = filter (flip notElem "<>=xyz")
    toTrip [a, b, c] = (a, b, c)

getX :: (Int, Int, Int) -> Int
getX (x, _, _) = x

getY :: (Int, Int, Int) -> Int
getY (_, y, _) = y

getZ :: (Int, Int, Int) -> Int
getZ (_, _, z) = z

initial :: ((Int, Int, Int) -> Int) -> [(Int, Int, Int)] -> [(Int, Int)]
initial g = map (\pos -> (g pos, 0))

deltaV :: [(Int, Int)] -> (Int, Int) -> Int
deltaV moons moon@(mPos, _) = sum $ map (\(pos, _) -> gravity pos mPos) moons
  where
    gravity x x' =
      case compare x x' of
        LT -> -1
        EQ -> 0
        GT -> 1

motion :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
motion moons moon@(pos, v) =
  let v' = v + deltaV moons moon
   in (pos + v', v')

steps :: [(Int, Int)] -> [[(Int, Int)]]
steps moons = unfoldr step moons
  where
    step moons =
      let moons' = map (motion moons) moons
       in Just (moons', moons')

energy :: ((Int, Int), (Int, Int), (Int, Int)) -> Int
energy ((pX, vX), (pY, vY), (pZ, vZ)) =
  (abs pX + abs pY + abs pZ) * (abs vX + abs vY + abs vZ)

getStep :: Int -> [[(Int, Int)]] -> [(Int, Int)]
getStep n = last . take n

diffPairs :: [Int] -> [Int]
diffPairs (a:b:rest) = (b - a) : diffPairs rest
diffPairs _ = []

deltas :: M.Map [(Int, Int)] [Int] -> M.Map [(Int, Int)] (Int, [Int])
deltas = M.mapWithKey (\_ vs -> (head $ reverse vs, diffPairs $ reverse vs))

encounters :: [[(Int, Int)]] -> M.Map [(Int, Int)] [Int]
encounters = go 1 M.empty
  where
    go _ m [] = m
    go n m (x:xs) =
      case M.lookup x m of
        Just occurs -> go (n + 1) (M.insert x (n : occurs) m) xs
        Nothing -> go (n + 1) (M.insert x [n] m) xs

getCycleLength :: [[(Int, Int)]] -> Int
getCycleLength = extractCycleLength . getEncounterDeltas
  where getEncounterDeltas = M.toList . deltas . encounters
        extractCycleLength = head . snd . snd . head . filter (not . null . snd . snd)

lcmTrip :: (Int, Int, Int) -> Integer
lcmTrip (a, b, c) = lcm (lcm a' b') c'
  where a' = toInteger a
        b' = toInteger b
        c' = toInteger c

main :: IO ()
main = do
  lines <- getLines "src/Day12/input.txt"
  let input = map parsePosition lines
      initialXs = initial getX input
      initialYs = initial getY input
      initialZs = initial getZ input
      xSteps = steps initialXs
      ySteps = steps initialYs
      zSteps = steps initialZs
      thousandthX = getStep 1000 $ xSteps
      thousandthY = getStep 1000 $ ySteps
      thousandthZ = getStep 1000 $ zSteps
      part1Answer = sum $ map energy $ zip3 thousandthX thousandthY thousandthZ
  print part1Answer
  let xCycleLength = getCycleLength $ take 1000000 xSteps
      yCycleLength = getCycleLength $ take 1000000 ySteps
      zCycleLength = getCycleLength $ take 1000000 zSteps
      part2Answer = lcmTrip (xCycleLength, yCycleLength, zCycleLength)
  print part2Answer
