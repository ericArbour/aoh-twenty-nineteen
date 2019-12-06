module Day05.Solution
  ( main
  ) where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Shared (getLines)

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

updateAt :: Int -> a -> [a] -> [a]
updateAt n x xs = take n xs ++ [x] ++ drop (n + 1) xs

initializeMemory :: Int -> Int -> [Int] -> [Int]
initializeMemory noun verb = updateAt 2 verb . updateAt 1 noun

parseOpCodeAndModes :: Int -> (Int, Int, Int, Int)
parseOpCodeAndModes n = (read opcode, read [p1m], read [p2m], read [p3m])
  where
    padLeft5 s =
      if length s < 5
        then padLeft5 $ '0' : s
        else s
    (p3m:p2m:p1m:opcode) = padLeft5 $ show n

runInstruction :: Int -> Int -> ([Int], [Int]) -> ([Int], [Int])
runInstruction input pointer (xs, outputs) =
  case opcode of
    99 -> (xs, outputs)
    1 ->
      runInstruction
        input
        (pointer + 4)
        (updateAt param3 (value1 + value2) xs, outputs)
    2 ->
      runInstruction
        input
        (pointer + 4)
        (updateAt param3 (value1 * value2) xs, outputs)
    3 -> runInstruction input (pointer + 2) (updateAt param1 input xs, outputs)
    4 -> runInstruction input (pointer + 2) (xs, value1 : outputs)
    5 ->
      runInstruction
        input
        (if value1 /= 0
           then value2
           else pointer + 3)
        (xs, outputs)
    6 ->
      runInstruction
        input
        (if value1 == 0
           then value2
           else pointer + 3)
        (xs, outputs)
    7 ->
      runInstruction
        input
        (pointer + 4)
        ( updateAt
            param3
            (if value1 < value2
               then 1
               else 0)
            xs
        , outputs)
    8 ->
      runInstruction
        input
        (pointer + 4)
        ( updateAt
            param3
            (if value1 == value2
               then 1
               else 0)
            xs
        , outputs)
    otherwise -> error $ "Invalid opcode: " <> show opcode
  where
    opcodenmodes = xs !! pointer
    (opcode, p1m, p2m, p3m) = parseOpCodeAndModes opcodenmodes
    param1 = xs !! (pointer + 1)
    param2 = xs !! (pointer + 2)
    value1 =
      if p1m == 0
        then xs !! param1
        else param1
    value2 =
      if p2m == 0
        then xs !! param2
        else param2
    param3 = xs !! (pointer + 3)

main :: IO ()
main = do
  [input] <- getLines "src/Day05/input.txt"
  let parsedInput = parseInput input
      part1Answer = head . snd $ runInstruction 1 0 (parsedInput, [])
      part2Answer = head . snd $ runInstruction 5 0 (parsedInput, [])
  print part1Answer
  print part2Answer
