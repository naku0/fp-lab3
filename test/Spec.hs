{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.List (nub, sortOn)
import MathBlock (Point, lagrangeInt, linearInt, newtonInt)
import Test.Hspec (describe, hspec, it, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    choose,
    listOf1,
    vectorOf,
    (==>),
  )

approxEqual :: Double -> Double -> Bool
approxEqual a b = abs (a - b) < 1e-2

newtype TestPoint = TestPoint Point deriving (Show)

instance Arbitrary TestPoint where
  arbitrary = do
    x <- choose (-10, 10)
    y <- choose (-10, 10)
    return $ TestPoint (x, y)

newtype UniquePoints = UniquePoints [Point] deriving (Show)

instance Arbitrary UniquePoints where
  arbitrary = do
    xs <- nub <$> listOf1 (choose (-10, 10))
    ys <- vectorOf (length xs) (choose (-10, 10))
    return $ UniquePoints (zip xs ys)

main :: IO ()
main = hspec $ do
  describe "Linear Interpolation" $ do
    it "interpolates between two points" $ do
      let points = [(0, 0), (2, 4)]
      linearInt points 1.0 `shouldSatisfy` approxEqual 2.0
      linearInt points 0.0 `shouldSatisfy` approxEqual 0.0
      linearInt points 2.0 `shouldSatisfy` approxEqual 4.0

    it "handles extrapolation" $ do
      let points = [(0, 0), (1, 1)]
      linearInt points (-1.0) `shouldSatisfy` approxEqual (-1.0)
      linearInt points 2.0 `shouldSatisfy` approxEqual 2.0

    it "handles single point" $ do
      let points = [(1, 5)]
      linearInt points 1.0 `shouldSatisfy` approxEqual 5.0
      linearInt points 10.0 `shouldSatisfy` approxEqual 5.0

    it "sorts points internally" $ do
      let points = [(2, 4), (0, 0)] -- не отсортированы
      linearInt points 1.0 `shouldSatisfy` approxEqual 2.0

  describe "Newton Interpolation" $ do
    it "gives exact values at data points" $ do
      let points = [(0, 1), (1, 2), (2, 5)]
      newtonInt points 0.0 `shouldSatisfy` approxEqual 1.0
      newtonInt points 1.0 `shouldSatisfy` approxEqual 2.0
      newtonInt points 2.0 `shouldSatisfy` approxEqual 5.0

    it "interpolates quadratic function correctly" $ do
      let points = [(0, 0), (1, 1), (2, 4)] -- y = x²
      newtonInt points 1.5 `shouldSatisfy` approxEqual 2.25

    it "equals linear for 2 points" $ do
      let points = [(0, 0), (2, 4)]
          x = 1.0
      newtonInt points x `shouldSatisfy` approxEqual (linearInt points x)

  describe "Lagrange Interpolation" $ do
    it "gives exact values at data points" $ do
      let points = [(-1, 1), (0, 0), (1, 1)] -- y = x²
      lagrangeInt points (-1.0) `shouldSatisfy` approxEqual 1.0
      lagrangeInt points 0.0 `shouldSatisfy` approxEqual 0.0
      lagrangeInt points 1.0 `shouldSatisfy` approxEqual 1.0

    it "equals linear for 2 points" $ do
      let points = [(0, 0), (2, 4)]
          x = 1.0
      lagrangeInt points x `shouldSatisfy` approxEqual (linearInt points x)

  describe "Methods Consistency" $ do
    it "all methods give same result for 2 points" $ do
      let points = [(0, 0), (1, 1)]
          x = 0.5
      linearInt points x `shouldSatisfy` approxEqual (newtonInt points x)
      linearInt points x `shouldSatisfy` approxEqual (lagrangeInt points x)

  describe "Property-Based Tests" $ do
    prop "exact at data points for linear" $ \(UniquePoints points) ->
      not (null points) ==>
        let sortedPoints = sortOn fst points
         in all (\(x, y) -> approxEqual (linearInt sortedPoints x) y) points

    prop "methods agree for 2 points" $ \(x1, y1) (x2, y2) x ->
      x1 /= x2 ==>
        let points = [(x1, y1), (x2, y2)]
            sorted = sortOn fst points
         in approxEqual (linearInt sorted x) (newtonInt sorted x) && approxEqual (linearInt sorted x) (lagrangeInt sorted x)

    prop "linear exact for linear functions" $ \a b x ->
      let f = \t -> a * t + b
          points = [(-1, f (-1)), (0, f 0), (1, f 1)]
       in approxEqual (linearInt points x) (f x)
