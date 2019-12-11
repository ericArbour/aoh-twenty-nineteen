-- Thanks to Michael Gilliland
-- https://www.youtube.com/watch?v=55ZWC5sSPMI

module Day07.Solution
  ( main
  ) where

import Data.List (permutations, uncons, unfoldr)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Shared (getLines)

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse $ unfoldr step n
  where
    step 0 = Nothing
    step n = Just (n `mod` 10, n `div` 10)

padL4 :: [Int] -> [Int]
padL4 ds = take (4 - length ds) (repeat 0) ++ ds

getOp :: Int -> Maybe (Int -> Int -> Int)
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

getIp :: Int -> Int -> Int -> Int -> Maybe Int
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

run :: Int -> [Int] -> M.Map Int Int -> Maybe ([Int], M.Map Int Int)
run ip inputs m = do
  opcode <- M.lookup ip m
  case padL4 $ digits opcode of
    [_, _, 9, 9] -> pure ([], m)
    [_, _, _, 3] -> do
      pos <- M.lookup (ip + 1) m
      (i, is) <- uncons inputs
      run (ip + 2) is $ M.insert pos i m
    [_, c, _, 4] -> do
      pos <- M.lookup (ip + 1) m
      val <-
        if c == 0
          then M.lookup pos m
          else Just pos
      (outputs, m) <- run (ip + 2) inputs m
      pure (val : outputs, m)
    [b, c, _, n]
      | n `elem` [5, 6] -> do
        pos1 <- M.lookup (ip + 1) m
        pos2 <- M.lookup (ip + 2) m
        val1 <-
          if c == 0
            then M.lookup pos1 m
            else Just pos1
        val2 <-
          if b == 0
            then M.lookup pos2 m
            else Just pos2
        ip' <- getIp n val1 val2 ip
        run ip' inputs m
    [b, c, _, n]
      | n `elem` [1, 2, 7, 8] -> do
        pos1 <- M.lookup (ip + 1) m
        pos2 <- M.lookup (ip + 2) m
        pos3 <- M.lookup (ip + 3) m
        val1 <-
          if c == 0
            then M.lookup pos1 m
            else Just pos1
        val2 <-
          if b == 0
            then M.lookup pos2 m
            else Just pos2
        op <- getOp n
        run (ip + 4) inputs $ M.insert pos3 (op val1 val2) m
    otherwise -> Nothing

runLoop ::
     Int -> [Int] -> M.Map Int Int -> Maybe (Int, Maybe Int, M.Map Int Int)
runLoop ip inputs m = do
  opcode <- M.lookup ip m
  case padL4 $ digits opcode of
    [_, _, 9, 9] -> pure (ip, Nothing, m)
    [_, _, _, 3] -> do
      pos <- M.lookup (ip + 1) m
      (input, rest) <- uncons inputs
      runLoop (ip + 2) rest $ M.insert pos input m
    [_, c, _, 4] -> do
      pos <- M.lookup (ip + 1) m
      val <-
        if c == 0
          then M.lookup pos m
          else Just pos
      pure (ip + 2, Just val, m)
    [b, c, _, n]
      | n `elem` [5, 6] -> do
        pos1 <- M.lookup (ip + 1) m
        pos2 <- M.lookup (ip + 2) m
        val1 <-
          if c == 0
            then M.lookup pos1 m
            else Just pos1
        val2 <-
          if b == 0
            then M.lookup pos2 m
            else Just pos2
        ip' <- getIp n val1 val2 ip
        runLoop ip' inputs m
    [b, c, _, n]
      | n `elem` [1, 2, 7, 8] -> do
        pos1 <- M.lookup (ip + 1) m
        pos2 <- M.lookup (ip + 2) m
        pos3 <- M.lookup (ip + 3) m
        val1 <-
          if c == 0
            then M.lookup pos1 m
            else Just pos1
        val2 <-
          if b == 0
            then M.lookup pos2 m
            else Just pos2
        op <- getOp n
        runLoop (ip + 4) inputs $ M.insert pos3 (op val1 val2) m
    otherwise -> Nothing

getOutputs :: Maybe ([Int], M.Map Int Int) -> Maybe [Int]
getOutputs Nothing = Nothing
getOutputs (Just (outputs, _)) = pure outputs

runAmps :: M.Map Int Int -> [Int] -> Maybe [Int]
runAmps initial = foldl step (Just [0])
  where
    step maybeInputs phase =
      case maybeInputs of
        Nothing -> Nothing
        Just inputs -> getOutputs $ run 0 (phase : inputs) initial

pt1Trials :: [[Int]]
pt1Trials = permutations [0, 1, 2, 3, 4]

pt2Trials :: [[Int]]
pt2Trials = permutations [5, 6, 7, 8, 9]

runPt1Trials :: M.Map Int Int -> [Maybe [Int]]
runPt1Trials initial = map (runAmps initial) pt1Trials

type AmpConfigMap = M.Map Int (Int, [Int], M.Map Int Int)

getAmpConfigMap :: M.Map Int Int -> [Int] -> AmpConfigMap
getAmpConfigMap initial phases = M.fromList . zip [0..] $ map (\inputs -> (0, inputs, initial)) inputsList
  where phasesList = map (:[]) phases
        inputsList = init phasesList ++ [last phasesList ++ [0]]

runAmpLoops ::
     Int -> M.Map Int (Int, [Int], M.Map Int Int) -> Maybe AmpConfigMap
runAmpLoops ap ampConfigMap = do
  (ip, outputs, m) <- M.lookup ap ampConfigMap
  (prevIp, inputs, prevM) <- M.lookup prevAp ampConfigMap
  (ip', maybeOutput, m') <- runLoop ip inputs m
  case maybeOutput of
    Nothing -> pure ampConfigMap
    Just output -> do
      ampConfigMap' <-
        pure $
        M.insert prevAp (prevIp, [], prevM) $
        M.insert ap (ip', outputs ++ [output], m') ampConfigMap
      runAmpLoops
        (if ap == 4
           then 0
           else ap + 1)
        ampConfigMap'
  where
    prevAp =
      if ap == 0
        then 4
        else ap - 1
    nextAp =
      if ap == 4
        then 0
        else ap - 1

runPt2Trials :: [AmpConfigMap] -> [Int]
runPt2Trials ampConfigMaps =
  map snd' $ catMaybes $ map getLastOutput ampConfigMaps
  where
    getLastOutput ampConfigMap =
      case runAmpLoops 0 ampConfigMap of
        Nothing -> Nothing
        Just lastMap -> M.lookup 4 lastMap
    snd' (_, [b], _) = b

main :: IO ()
main = do
  [input] <- getLines "src/Day07/input.txt"
  let parsedInput = parseInput input
      initial = M.fromList $ zip [0 ..] parsedInput
      part1Answer = maximum . catMaybes $ runPt1Trials initial
  print part1Answer
  let part2Answer = maximum . runPt2Trials $ map (getAmpConfigMap initial) pt2Trials
  print part2Answer
