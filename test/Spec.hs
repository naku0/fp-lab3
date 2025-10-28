{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.List (nub, sortOn)
import MathBlock (Point, lagrangeInt, linearInt, linearLagrangeInt, linearNewtonInt, newtonInt, newtonLagrangeInt)
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
  describe "Basic Methods" $ do
    it "linear works for 2 points" $ do
      let points = [(0, 0), (2, 4)]
      linearInt points 1.0 `shouldSatisfy` approxEqual 2.0

    it "newton works for parabola" $ do
      let points = [(0, 0), (1, 1), (2, 4)]
      newtonInt points 1.5 `shouldSatisfy` approxEqual 2.25

    it "lagrange works for parabola" $ do
      let points = [(-1, 1), (0, 0), (1, 1)]
      lagrangeInt points 0.5 `shouldSatisfy` approxEqual 0.25

  describe "Hybrid Methods" $ do
    it "linearNewton equals linear for 2 points" $ do
      let points = [(0, 0), (2, 4)]
          x = 1.0
      linearNewtonInt points x `shouldSatisfy` approxEqual (linearInt points x)

    it "linearLagrange gives reasonable results" $ do
      let points = [(0, 0), (1, 1), (2, 4)]
          x = 1.5
      linearLagrangeInt points x `shouldSatisfy` approxEqual 2.5

    it "newtonLagrange close to parent methods" $ do
      let points = [(0, 0), (1, 1), (2, 4)]
          x = 1.5
          hybrid = newtonLagrangeInt points x
          newton = newtonInt points x
          lagrange = lagrangeInt points x
      abs (hybrid - newton) `shouldSatisfy` (< 0.1)
      abs (hybrid - lagrange) `shouldSatisfy` (< 0.1)

  describe "QuickCheck Tests" $ do
    prop "all methods same for 2 points" $ \(x1, y1) (x2, y2) x ->
      x1 /= x2 ==>
        let points = [(x1, y1), (x2, y2)]
            sorted = sortOn fst points
            linearVal = linearInt sorted x
            newtonVal = newtonInt sorted x
            lagrangeVal = lagrangeInt sorted x
         in approxEqual linearVal newtonVal && approxEqual linearVal lagrangeVal
