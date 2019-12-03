module Day02.Solution (main) where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Shared (getLines)

parseFirstLine :: [String] -> [Int]
parseFirstLine = map read . splitOn "," . head

updateAt :: Int -> a -> [a] -> [a]
updateAt n x xs = take n xs ++ [x] ++ drop (n + 1) xs

initializeMemory :: Int -> Int -> [Int] -> [Int]
initializeMemory noun verb = updateAt 2 verb . updateAt 1 noun

runInstruction :: Int -> [Int] -> [Int]
runInstruction pointer xs =
  case opcode of
    99 -> xs
    1 -> runInstruction nextPointer $ updateAt output (input1 + input2) xs
    2 -> runInstruction nextPointer $ updateAt output (input1 * input2) xs
    otherwise -> error "Invalid opcode."
  where
    opcode = xs !! pointer
    parameter1 = xs !! (pointer + 1)
    parameter2 = xs !! (pointer + 2)
    input1 = xs !! parameter1
    input2 = xs !! parameter2
    output = xs !! (pointer + 3)
    nextPointer = pointer + 4

compute :: Int -> Int -> [Int] -> Int
compute noun verb programState = head $ runInstruction 0 initialMemory
  where
    initialMemory = initializeMemory noun verb programState

nounVerbPairs :: [(Int, Int)]
nounVerbPairs = do
  noun <- [0 .. 99]
  verb <- [0 .. 99]
  return (noun, verb)

main :: IO ()
main = do
  lines <- getLines "src/Day02/input.txt"
  let input = parseFirstLine lines
      part1Answer = compute 12 2 input
  print part1Answer
  let maybePair =
        find (\(n, v) -> (compute n v input) == 19690720) nounVerbPairs
      (noun, verb) = fromJust maybePair
      part2Answer = 100 * noun + verb
  print part2Answer
