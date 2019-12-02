module Day01.Solution where

import Control.Monad.Loops (whileM)
import System.IO
  ( FilePath
  , Handle()
  , IOMode(ReadMode)
  , hGetLine
  , hIsEOF
  , withFile
  )

checkEOF :: Handle -> IO Bool
checkEOF inputHandle = do
  isEOF <- hIsEOF inputHandle
  return $ not isEOF

getInputs :: FilePath -> IO [Integer]
getInputs filePath = do
  withFile filePath ReadMode $ \inputHandle -> do
    inputs <-
      whileM (checkEOF inputHandle) $ do
        input <- hGetLine inputHandle
        return $ read input
    return inputs

calc :: Integer -> Integer
calc = (subtract 2) . (`div` 3)

main :: IO ()
main = do
  inputs <- getInputs "src/Day01/input.txt"
  let answer = sum $ map calc inputs
  putStrLn $ show answer
