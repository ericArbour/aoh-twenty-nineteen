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

calcFuelForMass :: Integer -> Integer
calcFuelForMass = (subtract 2) . (`div` 3)

calcFuelForFuelMass :: Integer -> Integer
calcFuelForFuelMass fuelMass =
  if fuelForFuel <= 0
    then fuelMass
    else fuelMass + (calcFuelForFuelMass fuelForFuel)
  where
    fuelForFuel = calcFuelForMass fuelMass

main :: IO ()
main = do
  moduleMasses <- getInputs "src/Day01/input.txt"
  let moduleFuelMasses = map calcFuelForMass moduleMasses
      partOneAnswer = sum moduleFuelMasses
      partTwoAnswer = sum $ map calcFuelForFuelMass moduleFuelMasses
  putStrLn $ show partOneAnswer
  putStrLn $ show partTwoAnswer
