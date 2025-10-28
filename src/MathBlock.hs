module MathBlock
  ( InterpolationMethod (..),
    Point,
    linearInt,
    newtonInt,
    lagrangeInt,
    linearNewtonInt,
    linearLagrangeInt,
    newtonLagrangeInt,
  )
where

import Data.Foldable (find)
import Data.List (sortOn)

data InterpolationMethod = Linear | Newton | Lagrange | NewtonLagrange | LinearNewton | LinearLagrange deriving (Show, Eq, Read)

type Point = (Double, Double)

linearInt :: [Point] -> Double -> Double
linearInt points x =
  let sorted = sortOn fst points
   in case findSegment sorted x of
        Just ((x1, y1), (x2, y2)) ->
          y1 + (y2 - y1) * (x - x1) / (x2 - x1)
        Nothing ->
          case sorted of
            [] -> error "No points"
            [single] -> snd single
            (p1 : p2 : _) ->
              if x < fst p1
                then extrapolateLeft p1 p2 x
                else extrapolateRight (last (init sorted)) (last sorted) x

findSegment :: [Point] -> Double -> Maybe (Point, Point)
findSegment points x =
  let sorted = sortOn fst points
      segments = zip sorted (tail sorted)
   in find (\((x1, _), (x2, _)) -> x1 <= x && x <= x2) segments

extrapolateLeft :: Point -> Point -> Double -> Double
extrapolateLeft (x1, y1) (x2, y2) x =
  y1 + (y2 - y1) * (x - x1) / (x2 - x1)

extrapolateRight :: Point -> Point -> Double -> Double
extrapolateRight (x1, y1) (x2, y2) x =
  y1 + (y2 - y1) * (x - x1) / (x2 - x1)

{-===================-}

newtonInt :: [Point] -> Double -> Double
newtonInt points x =
  let sortedPoints = sortOn fst points
      n = length sortedPoints
      xs = map fst sortedPoints
      diffTable = buildDiffTable sortedPoints
      coeffs = map head diffTable
   in sum
        [ coeffs !! k * product [x - xs !! j | j <- [0 .. k - 1]]
          | k <- [0 .. n - 1]
        ]

buildDiffTable :: [Point] -> [[Double]]
buildDiffTable points =
  let xs = map fst points
      ys = map snd points
      n = length points
   in reverse $
        foldl
          ( \table level ->
              let prevLevel = head table
                  newLevel =
                    [ (prevLevel !! (i + 1) - prevLevel !! i)
                        / (xs !! (i + level) - xs !! i)
                      | i <- [0 .. length prevLevel - 2]
                    ]
               in newLevel : table
          )
          [ys]
          [1 .. n - 1]

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

{-==============-}

linearNewtonInt :: [Point] -> Double -> Double
linearNewtonInt points x
  | length points <= 2 = linearInt points x
  | otherwise =
      let linearResult = linearInt points x
          newtonResult = newtonInt points x
          sortedXs = map fst (sortOn fst points)
          minX = head sortedXs
          maxX = last sortedXs
          linearWeight =
            if x < minX || x > maxX
              then 0.8
              else 0.3
       in linearWeight * linearResult + (1 - linearWeight) * newtonResult

linearLagrangeInt :: [Point] -> Double -> Double
linearLagrangeInt points x
  | length points <= 3 = linearInt points x
  | length points > 10 = linearInt points x
  | otherwise =
      let linearResult = linearInt points x
          lagrangeResult = lagrangeInt points x
          sortedPoints = sortOn fst points
          distances = map (abs . (x -) . fst) sortedPoints
          minDistance = minimum distances
          lagrangeWeight = exp (-(minDistance * 2))
       in lagrangeWeight * lagrangeResult + (1 - lagrangeWeight) * linearResult

newtonLagrangeInt :: [Point] -> Double -> Double
newtonLagrangeInt points x
  | length points <= 5 = lagrangeInt points x
  | otherwise =
      let newtonResult = newtonInt points x
          lagrangeResult = lagrangeInt points x
          diff = abs (newtonResult - lagrangeResult)
          weight
            | diff < 1e-10 = 0.5
            | diff < 1e-5 = 0.7
            | otherwise = 0.9
       in weight * newtonResult + (1 - weight) * lagrangeResult
