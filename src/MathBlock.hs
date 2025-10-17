module MathBlock
  ( InterpolationMethod (..),
    Point,
    linearInt,
    newtonInt,
    lagrangeInt,
  )
where

import Data.Foldable (find)
import Data.List (sortOn)

data InterpolationMethod = Linear | Newton | Lagrange deriving (Show, Eq, Read)

type Point = (Double, Double)

linearInt :: [Point] -> Double -> Double
linearInt points x =
  case findSegment points x of
    Just ((x1, y1), (x2, y2)) ->
      y1 + (y2 - y1) * (x - x1) / (x2 - x1)
    Nothing ->
      let sorted = sortOn fst points
       in case sorted of
            [] -> error "No points"
            [single] -> snd single
            (p1 : p2 : _) ->
              if x < fst p1
                then linearInt [p1, p2] x
                else linearInt [last (init sorted), last sorted] x

findSegment :: [Point] -> Double -> Maybe (Point, Point)
findSegment points x =
  let sorted = sortOn fst points
      segments = zip sorted (tail sorted)
   in find (\((x1, _), (x2, _)) -> x1 <= x && x <= x2) segments

{-===================-}
newtonInt :: [Point] -> Double -> Double
newtonInt points x =
  let sortedPoints = sortOn fst points
      n = length sortedPoints
      xs = map fst sortedPoints
      diffTable = buildDiffTable sortedPoints
      coeffs = map head diffTable
   in sum [ coeffs !! k * product [x - xs !! j | j <- [0..k-1]]
          | k <- [0..n-1] ]

buildDiffTable :: [Point] -> [[Double]]
buildDiffTable points =
  let xs = map fst points
      ys = map snd points
      n = length points
  in reverse $ foldl (\table k ->
        let prevLevel = head table
            newLevel = [ (prevLevel !! (i+1) - prevLevel !! i) / 
                         (xs !! (i+k) - xs !! i)
                       | i <- [0..length prevLevel - 2] ]
        in newLevel : table
      ) [ys] [1..n-1]
{-===================-}

lagrangeInt :: [Point] -> Double -> Double
lagrangeInt points x =
  let sortedPoints = sortOn fst points
      xs = map fst sortedPoints
      ys = map snd sortedPoints
   in sum [y * lagrangeBasis xs i x | (i, y) <- zip [0 ..] ys]

lagrangeBasis :: [Double] -> Int -> Double -> Double
lagrangeBasis xs i x =
  product
    [ if j == i then 1 else (x - xs !! j) / (xs !! i - xs !! j)
      | j <- [0 .. length xs - 1]
    ]
