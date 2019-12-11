module Day09.Solution
  ( main
  ) where

import Data.List (uncons, unfoldr)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Shared (getLines)

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
  -> [Integer]
  -> Integer
  -> M.Map Integer Integer
  -> Maybe ([Integer], M.Map Integer Integer)
run ip inputs base m = do
  opcode <- accessM ip
  case padL5 $ digits opcode of
    [_, _, _, 9, 9] -> pure ([], m)
    [_, _, c, _, 3] -> do
      pos <- accessM (ip + 1)
      val <- getValW c pos
      (i, is) <- uncons inputs
      run (ip + 2) is base $ M.insert val i m
    [_, _, c, _, 4] -> do
      pos <- accessM (ip + 1)
      val <- getVal c pos
      (outputs, m) <- run (ip + 2) inputs base m
      pure (val : outputs, m)
    [_, _, c, _, 9] -> do
      pos <- accessM (ip + 1)
      val <- getVal c pos
      run (ip + 2) inputs (base + val) m
    [_, b, c, _, n]
      | n `elem` [5, 6] -> do
        pos1 <- accessM (ip + 1)
        pos2 <- accessM (ip + 2)
        val1 <- getVal c pos1
        val2 <- getVal b pos2
        ip' <- getIp n val1 val2 ip
        run ip' inputs base m
    [a, b, c, _, n]
      | n `elem` [1, 2, 7, 8] -> do
        pos1 <- accessM (ip + 1)
        pos2 <- accessM (ip + 2)
        pos3 <- accessM (ip + 3)
        val1 <- getVal c pos1
        val2 <- getVal b pos2
        val3 <- getValW a pos3
        op <- getOp n
        run (ip + 4) inputs base $ M.insert val3 (op val1 val2) m
    otherwise -> Nothing
  where 
    accessM pos = Just $ M.findWithDefault 0 pos m
    getVal 0 pos = accessM pos
    getVal 1 pos = Just pos
    getVal 2 pos = getVal 0 (pos + base)
    getValW 0 pos = Just pos
    getValW 2 pos = Just (pos + base)

main :: IO ()
main = do
  [input] <- getLines "src/Day09/input.txt"
  let parsedInput = parseInput input
      initial = M.fromList $ zip [0 ..] parsedInput
      part1Answer = fst . fromJust $ run 0 [1] 0 initial
      part2Answer = fst . fromJust $ run 0 [2] 0 initial
  print part1Answer
  print part2Answer
