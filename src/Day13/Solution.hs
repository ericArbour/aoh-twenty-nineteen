module Day13.Solution
  ( main
  ) where

import Control.Concurrent (threadDelay)
import Data.List (intercalate, uncons, unfoldr)
import Data.List.Split (chunksOf, splitOn)
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

data InputState
  = Feed
  | Done
  deriving (Show)

run ::
     Integer
  -> Integer
  -> Maybe Integer
  -> M.Map Integer Integer
  -> Maybe (Integer, Integer, InputState, [Integer], M.Map Integer Integer)
run ip base maybeInput m = do
  opcode <- accessM ip
  case padL5 $ digits opcode of
    [_, _, _, 9, 9] -> pure (ip, base, Done, [], m)
    [_, _, c, _, 3] -> do
      pos <- accessM (ip + 1)
      val <- getValW c pos
      case maybeInput of
        Nothing -> pure (ip, base, Feed, [], m)
        Just input -> run (ip + 2) base Nothing $ M.insert val input m
    [_, _, c, _, 4] -> do
      pos <- accessM (ip + 1)
      val <- getVal c pos
      (ip', base', inputState', outputs, m') <- run (ip + 2) base maybeInput m
      pure (ip', base', inputState', (val : outputs), m')
    [_, _, c, _, 9] -> do
      pos <- accessM (ip + 1)
      val <- getVal c pos
      run (ip + 2) (base + val) maybeInput m
    [_, b, c, _, n]
      | n `elem` [5, 6] -> do
        pos1 <- accessM (ip + 1)
        pos2 <- accessM (ip + 2)
        val1 <- getVal c pos1
        val2 <- getVal b pos2
        ip' <- getIp n val1 val2 ip
        run ip' base maybeInput m
    [a, b, c, _, n]
      | n `elem` [1, 2, 7, 8] -> do
        pos1 <- accessM (ip + 1)
        pos2 <- accessM (ip + 2)
        pos3 <- accessM (ip + 3)
        val1 <- getVal c pos1
        val2 <- getVal b pos2
        val3 <- getValW a pos3
        op <- getOp n
        run (ip + 4) base maybeInput $ M.insert val3 (op val1 val2) m
    otherwise -> Nothing
  where
    accessM pos = Just $ M.findWithDefault 0 pos m
    getVal 0 pos = accessM pos
    getVal 1 pos = Just pos
    getVal 2 pos = getVal 0 (pos + base)
    getValW 0 pos = Just pos
    getValW 2 pos = Just (pos + base)

getCoordValPair :: [a] -> ((a, a), a)
getCoordValPair [a, b, c] = ((a, b), c)

getTileX :: Integer -> M.Map (Integer, Integer) Integer -> Integer
getTileX n = fst . fst . head . filter ((== n) . snd) . M.toList

brain :: Integer -> Integer -> Integer
brain bx px
  | bx > px = 1
brain bx px
  | bx < px = -1
brain _ _ = 0

game ::
     Integer
  -> Integer
  -> Maybe Integer
  -> M.Map Integer Integer
  -> M.Map (Integer, Integer) Integer
  -> IO ()
game ip base maybeInput program gameMap = do
  case run ip base maybeInput program of
    Nothing -> error "Nothing"
    Just (ip', base', inputState, outputs, program') -> do
      let outputMap = M.fromList $ map getCoordValPair $ chunksOf 3 outputs
          gameMap' = M.union outputMap gameMap
      putStrLn $ drawFrame gameMap'
      score <- maybe (return 0) return $ M.lookup (-1, 0) gameMap'
      putStrLn $ "Current Score: " <> show score
      case inputState of
        Feed -> do
          let ballX = getTileX 4 gameMap'
              paddleX = getTileX 3 gameMap'
              input = brain ballX paddleX
          threadDelay 10000
          game ip' base' (Just input) program' gameMap'
        Done -> do
          putStrLn "Game Over"

screen :: [(Integer, Integer)]
screen = do
  y <- [0 .. 20]
  x <- [0 .. 35]
  pure (x, y)

drawFrame :: M.Map (Integer, Integer) Integer -> String
drawFrame gameMap = intercalate "\n" . chunksOf 36 $ map drawTile screen
  where
    drawTile coord =
      case M.lookup coord gameMap of
        Nothing -> error "Invalid Coordinate"
        Just val ->
          case val of
            0 -> ' '
            1 -> '|'
            2 -> 'X'
            3 -> '_'
            4 -> 'O'
            otherwise -> '*'

main :: IO ()
main = do
  [line] <- getLines "src/Day13/input.txt"
  let input = M.insert 0 2 $ M.fromList $ zip [0 ..] $ parseInput line
  game 0 0 Nothing input M.empty
