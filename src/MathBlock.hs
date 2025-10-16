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
      xs = map fst sortedPoints
      dividedDiffs = computeDividedDifferences sortedPoints
   in newtonPolynomial xs dividedDiffs x

-- Вычисление разделенных разностей (исправленная версия)
computeDividedDifferences :: [Point] -> [Double]
computeDividedDifferences points =
  let n = length points
      xs = map fst points
      ys = map snd points
   in head $ newtonDD xs ys (n - 1)
  where
    newtonDD :: [Double] -> [Double] -> Int -> [[Double]]
    newtonDD _ ys 0 = [ys]
    newtonDD xs ys k =
      let prevDD = head $ newtonDD xs ys (k - 1)
          newDD =
            [ (prevDD !! (j + 1) - prevDD !! j) / (xs !! (j + k) - xs !! j)
              | j <- [0 .. length prevDD - 2]
            ]
       in newDD : newtonDD xs ys (k - 1)

-- Полином Ньютона
newtonPolynomial :: [Double] -> [Double] -> Double -> Double
newtonPolynomial xs diffs x =
  sum $
    zipWith
      (\diff i -> diff * product [x - xs !! j | j <- [0 .. i - 1]])
      diffs
      [0 ..]

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
