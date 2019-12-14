module Day11.Solution
  ( main
  ) where

import Data.Function (on)
import Data.List (find, intercalate, maximumBy, minimumBy, nub, uncons, unfoldr)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Shared (getLines)

type Coord = (Integer, Integer)

data Dir
  = N
  | E
  | S
  | W
  deriving (Eq, Ord, Show, Bounded, Enum)

type Color = Integer

data Panel =
  Panel
    { coord :: Coord
    , dir :: Dir
    , color :: Color
    }
  deriving (Show)

parseInput :: String -> [Integer]
parseInput = map read . splitOn ","

digits :: (Num a, Eq a, Integral a) => a -> [a]
digits 0 = [0]
digits n = reverse $ unfoldr step n
  where
    step 0 = Nothing
    step n = Just (n `mod` 10, n `div` 10)

padL5 :: [Integer] -> [Integer]
padL5 ds = take (5 - length ds) (repeat 0) ++ ds

getOp :: Integer -> Maybe (Integer -> Integer -> Integer)
getOp 1 = Just (+)
getOp 2 = Just (*)
getOp 7 =
  Just
    (\a b ->
       if a < b
         then 1
         else 0)
getOp 8 =
  Just
    (\a b ->
       if a == b
         then 1
         else 0)
getOp _ = Nothing

getIp :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
getIp 5 a b ip =
  Just $
  if a /= 0
    then b
    else (ip + 3)
getIp 6 a b ip =
  Just $
  if a == 0
    then b
    else (ip + 3)
getIp _ _ _ _ = Nothing

run ::
     Integer
  -> Integer
  -> [Integer]
  -> M.Map Integer Integer
  -> Maybe (Integer, Integer, Maybe Integer, M.Map Integer Integer)
run ip base inputs m = do
  opcode <- accessM ip
  case padL5 $ digits opcode of
    [_, _, _, 9, 9] -> pure (ip, base, Nothing, m)
    [_, _, c, _, 3] -> do
      pos <- accessM (ip + 1)
      val <- getValW c pos
      (input, inputs') <- uncons inputs
      run (ip + 2) base inputs' $ M.insert val input m
    [_, _, c, _, 4] -> do
      pos <- accessM (ip + 1)
      val <- getVal c pos
      pure (ip + 2, base, Just val, m)
    [_, _, c, _, 9] -> do
      pos <- accessM (ip + 1)
      val <- getVal c pos
      run (ip + 2) (base + val) inputs m
    [_, b, c, _, n]
      | n `elem` [5, 6] -> do
        pos1 <- accessM (ip + 1)
        pos2 <- accessM (ip + 2)
        val1 <- getVal c pos1
        val2 <- getVal b pos2
        ip' <- getIp n val1 val2 ip
        run ip' base inputs m
    [a, b, c, _, n]
      | n `elem` [1, 2, 7, 8] -> do
        pos1 <- accessM (ip + 1)
        pos2 <- accessM (ip + 2)
        pos3 <- accessM (ip + 3)
        val1 <- getVal c pos1
        val2 <- getVal b pos2
        val3 <- getValW a pos3
        op <- getOp n
        run (ip + 4) base inputs $ M.insert val3 (op val1 val2) m
    otherwise -> Nothing
  where
    accessM pos = Just $ M.findWithDefault 0 pos m
    getVal 0 pos = accessM pos
    getVal 1 pos = Just pos
    getVal 2 pos = getVal 0 (pos + base)
    getValW 0 pos = Just pos
    getValW 2 pos = Just (pos + base)

cyclePrev :: (Eq a, Enum a, Bounded a) => a -> a
cyclePrev x =
  if x == minBound
    then maxBound
    else pred x

cycleNext :: (Eq a, Enum a, Bounded a) => a -> a
cycleNext x =
  if x == maxBound
    then minBound
    else succ x

turn :: Dir -> Integer -> Dir
turn dir' 0 = cyclePrev dir'
turn dir' 1 = cycleNext dir'

step :: Coord -> Dir -> Coord
step (x, y) dir' =
  case dir' of
    N -> (x, y + 1)
    E -> (x + 1, y)
    S -> (x, y - 1)
    W -> (x - 1, y)

getColor :: Coord -> [Panel] -> Integer
getColor coord' panels =
  case find ((== coord') . coord) panels of
    Just panel -> color panel
    Nothing -> 0

robot :: Integer -> Integer -> M.Map Integer Integer -> [Panel] -> [Panel]
robot ip base program (panel:panels) =
  let (ip', base', maybeColor, program') =
        fromJust $ run ip base [color panel] program
      (ip'', base'', maybeRot, program'') = fromJust $ run ip' base' [] program'
   in case (maybeColor, maybeRot) of
        (Just color', Just rot) ->
          robot ip'' base'' program'' $
          (nextPanel rot) : (panel' color') : panels
        otherwise -> (panel : panels)
  where
    panel' color' = Panel {coord = coord panel, dir = dir panel, color = color'}
    nextPanel rot =
      let dir' = turn (dir panel) rot
          coord' = step (coord panel) dir'
          color' = getColor coord' panels
       in Panel {coord = coord', dir = dir', color = color'}

getGrid :: [Integer] -> [Integer] -> [(Integer, Integer)]
getGrid xs ys = do
  y <- ys
  x <- xs
  pure (x, y)

draw :: M.Map Coord Integer -> Coord -> Char
draw m coord' = paint $ M.findWithDefault 0 coord' m
  where
    paint 0 = ' '
    paint 1 = 'X'

main :: IO ()
main = do
  [input] <- getLines "src/Day11/input.txt"
  let parsedInput = parseInput input
      initial = M.fromList $ zip [0 ..] parsedInput
      firstPanel = Panel {coord = (0, 0), dir = N, color = 0}
      path = robot 0 0 initial [firstPanel]
      part1Answer = length . nub $ map coord path
  print part1Answer
  let firstPanel = Panel {coord = (0, 0), dir = N, color = 1}
      path = robot 0 0 initial [firstPanel]
      (xMin, _) = coord $ minimumBy (compare `on` fst . coord) path
      (xMax, _) = coord $ maximumBy (compare `on` fst . coord) path
      (_, yMin) = coord $ minimumBy (compare `on` snd . coord) path
      (_, yMax) = coord $ maximumBy (compare `on` snd . coord) path
      xRange = length [xMin .. xMax]
      grid = chunksOf xRange $ getGrid [xMin .. xMax] [yMax,(yMax - 1) .. yMin]
      colorMap = M.fromList $ map (\panel -> (coord panel, color panel)) path
      paintedGrid = map (map (draw colorMap)) grid
  putStrLn $ intercalate "\n" paintedGrid
