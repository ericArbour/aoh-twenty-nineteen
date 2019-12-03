module Day02.Solution where

import Data.List.Split (splitOn)

import Shared (getLines)

parseFirstLine :: [String] -> [Int]
parseFirstLine = map read . splitOn "," . head

updateAt :: Int -> a -> [a] -> [a]
updateAt n x xs = take n xs ++ [x] ++ drop (n + 1) xs

restoreInput :: [Int] -> [Int]
restoreInput = updateAt 2 2 . updateAt 1 12

compute :: Int -> [Int] -> [Int]
compute i xs =
  case opcode of
    99 -> xs
    1 -> compute nextI $ updateAt output (input1 + input2) xs
    2 -> compute nextI $ updateAt output (input1 * input2) xs
    otherwise -> error "Invalid opcode."
  where
    opcode = xs !! i
    input1 = xs !! (xs !! (i + 1))
    input2 = xs !! (xs !! (i + 2))
    output = xs !! (i + 3)
    nextI = i + 4

main :: IO ()
main = do
  lines <- getLines "src/Day02/input.txt"
  let input = parseFirstLine lines
      restoredInput = restoreInput input
      result = compute 0 restoredInput
  print result
