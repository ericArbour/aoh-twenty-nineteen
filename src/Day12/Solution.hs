module Day12.Solution
  ( main
  ) where

import Data.List (notElem, unfoldr)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Text.Read (readMaybe)

import Shared (getLines)

type Position = (Int, Int, Int)

type Velocity = (Int, Int, Int)

type Status = (Position, Velocity)

parsePosition :: String -> Position
parsePosition = toTrip . catMaybes . map (readMaybe . cleanInput) . splitOn ", "
  where
    cleanInput = filter (flip notElem "<>=xyz")
    toTrip [a, b, c] = (a, b, c)

initVelocity :: Velocity
initVelocity = (0, 0, 0)

gravity ::
     (Position, Position, Position, Position)
  -> (Velocity, Velocity, Velocity, Velocity)
gravity ((px0, py0, pz0), (px1, py1, pz1), (px2, py2, pz2), (px3, py3, pz3)) =
  let vx0 = getDimV px0 [px1, px2, px3]
      vx1 = getDimV px1 [px0, px2, px3]
      vx2 = getDimV px2 [px0, px1, px3]
      vx3 = getDimV px3 [px0, px1, px2]
      vy0 = getDimV py0 [py1, py2, py3]
      vy1 = getDimV py1 [py0, py2, py3]
      vy2 = getDimV py2 [py0, py1, py3]
      vy3 = getDimV py3 [py0, py1, py2]
      vz0 = getDimV pz0 [pz1, pz2, pz3]
      vz1 = getDimV pz1 [pz0, pz2, pz3]
      vz2 = getDimV pz2 [pz0, pz1, pz3]
      vz3 = getDimV pz3 [pz0, pz1, pz2]
   in ((vx0, vy0, vz0), (vx1, vy1, vz1), (vx2, vy2, vz2), (vx3, vy3, vz3))
  where
    getDimV x = foldr (sumDiffs x) 0
    sumDiffs x a c =
      if a > x
        then c + 1
        else if a < x
               then c - 1
               else c

motion ::
     (Position, Position, Position, Position)
  -> (Velocity, Velocity, Velocity, Velocity)
  -> (Position, Position, Position, Position)
motion ((px0, py0, pz0), (px1, py1, pz1), (px2, py2, pz2), (px3, py3, pz3)) ((vx0, vy0, vz0), (vx1, vy1, vz1), (vx2, vy2, vz2), (vx3, vy3, vz3)) =
  ( (px0 + vx0, py0 + vy0, pz0 + vz0)
  , (px1 + vx1, py1 + vy1, pz1 + vz1)
  , (px2 + vx2, py2 + vy2, pz2 + vz2)
  , (px3 + vx3, py3 + vy3, pz3 + vz3))

addTrips :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTrips (a0, b0, c0) (a1, b1, c1) = (a0 + a1, b0 + b1, c0 + c1)

simulate ::
     (Status, Status, Status, Status) -> [(Status, Status, Status, Status)]
simulate = unfoldr step
  where
    step ((p0, v0), (p1, v1), (p2, v2), (p3, v3)) =
      let positions = (p0, p1, p2, p3)
          (vd0, vd1, vd2, vd3) = gravity positions
          velocities =
            (addTrips v0 vd0, addTrips v1 vd1, addTrips v2 vd2, addTrips v3 vd3)
          (v0', v1', v2', v3') = velocities
          (p0', p1', p2', p3') = motion positions velocities
          result = ((p0', v0'), (p1', v1'), (p2', v2'), (p3', v3'))
       in Just (result, result)

sumTrip :: Num a => (a, a, a) -> a
sumTrip (x, y, z) = abs x + abs y + abs z

energy :: (Status, Status, Status, Status) -> Int
energy ((p0, v0), (p1, v1), (p2, v2), (p3, v3)) =
  (sumTrip p0 * sumTrip v0) + (sumTrip p1 * sumTrip v1) +
  (sumTrip p2 * sumTrip v2) +
  (sumTrip p3 * sumTrip v3)

main :: IO ()
main = do
  lines <- getLines "src/Day12/input.txt"
  let [a, b, c, d] = map (flip (,) initVelocity . parsePosition) lines
      initialStatuses = (a, b, c, d)
      moves = simulate initialStatuses
      part1Answer = energy . head . reverse $ take 1000 moves
  print part1Answer
