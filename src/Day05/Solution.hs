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

runInstruction :: Int -> ([Int], [Int]) -> ([Int], [Int])
runInstruction pointer (xs, outputs) =
  case opcode of
    99 -> (xs, outputs)
    1 -> 
      runInstruction nextPointer (updateAt target (value1 + value2) xs, outputs)
    2 ->
      runInstruction nextPointer (updateAt target (value1 * value2) xs, outputs)
    3 -> runInstruction nextPointer (updateAt param1 1 xs, outputs)
    4 -> runInstruction nextPointer (xs, value1 : outputs)
    otherwise -> error $ "Invalid opcode: " <> show opcode <> " " <> show opcodenmodes
  where
    opcodenmodes = xs !! pointer
    (opcode,p1m,p2m,p3m) = parseOpCodeAndModes opcodenmodes
    param1 = xs !! (pointer + 1)
    param2 = xs !! (pointer + 2)
    value1 = if p1m == 0 then xs !! param1 else param1
    value2 = if p2m == 0 then xs !! param2 else param2
    target = xs !! (pointer + 3)
    nextPointer =
      if opcode == 3 || opcode == 4
        then pointer + 2
        else pointer + 4

compute :: [Int] -> ([Int], [Int])
compute programState = runInstruction 0 (programState, [])

nounVerbPairs :: [(Int, Int)]
nounVerbPairs = do
  noun <- [0 .. 99]
  verb <- [0 .. 99]
  return (noun, verb)

main :: IO ()
main = do
  [input] <- getLines "src/Day05/input.txt"
  let parsedInput = parseInput input
      part1Answer = compute parsedInput
  print part1Answer
