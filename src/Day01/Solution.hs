module Day01.Solution (main) where

import Shared (getLines)

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
  lines <- getLines "src/Day01/input.txt"
  let moduleMasses = map read lines
      moduleFuelMasses = map calcFuelForMass moduleMasses
      partOneAnswer = sum moduleFuelMasses
      partTwoAnswer = sum $ map calcFuelForFuelMass moduleFuelMasses
  putStrLn $ show partOneAnswer
  putStrLn $ show partTwoAnswer
