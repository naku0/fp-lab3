module Parse
  ( parseRead,
    parsePoint,
  )
where

import Config (Config (..))
import Data.Char (toLower)
import MathBlock (InterpolationMethod (..), Point)

parseRead :: [String] -> Config -> Config
parseRead [] config = config
parseRead ("--method" : x : xs) config =
  case parseMethod x of
    Just method' -> parseRead xs config {method = method'}
    Nothing -> error $ "There's no such method as" <> x <> "\n" <> "Available methods: linear, newton, lagrange, newton-lagrange, linear-newton, linear-lagrange"
parseRead ("--n" : x : xs) config =
  case parsePoints x of
    Just n -> parseRead xs config {startPoints = n}
    Nothing -> error $ "Your input should be >=0, not " <> show x
parseRead ("--step" : x : xs) config =
  case parseStep x of
    Just stepV -> parseRead xs config {step = stepV}
    Nothing -> error $ "Your input should be positive number, not " <> show x
parseRead (_ : xs) config = parseRead xs config

parseMethod :: String -> Maybe InterpolationMethod
parseMethod s = case map toLower s of
  "linear" -> Just Linear
  "lin" -> Just Linear
  "newton" -> Just Newton
  "new" -> Just Newton
  "lagrange" -> Just Lagrange
  "lag" -> Just Lagrange
  "newton-lagrange" -> Just NewtonLagrange
  "lagrange-newton" -> Just NewtonLagrange
  "new-lag" -> Just NewtonLagrange
  "newlag" -> Just NewtonLagrange
  "lag-new" -> Just NewtonLagrange
  "lagnew" -> Just NewtonLagrange
  "linear-newton" -> Just LinearNewton
  "newton-linear" -> Just LinearNewton
  "linnew" -> Just LinearNewton
  "lin-new" -> Just LinearNewton
  "newlin" -> Just LinearNewton
  "new-lin" -> Just LinearNewton
  "linear-lagrange" -> Just LinearLagrange
  "lin-lag" -> Just LinearLagrange
  "linlag" -> Just LinearLagrange
  "lagrange-linear" -> Just LinearLagrange
  "lag-lin" -> Just LinearLagrange
  "lin-lag" -> Just LinearLagrange
  _ -> Nothing

parsePoints :: String -> Maybe Int
parsePoints s = case reads s of
  [(n, "")] | n >= 0 -> Just n
  _ -> Nothing

parseStep :: String -> Maybe Double
parseStep s = case reads s of
  [(sV, "")] | sV > 0 -> Just sV
  _ -> Nothing

parsePoint :: String -> Maybe Point
parsePoint input = case words (map normalize input) of
  [xStr, yStr] -> case (reads xStr, reads yStr) of
    ([(x, "")], [(y, "")]) -> Just (x, y)
    _ -> Nothing
  _ -> Nothing
  where
    normalize c = if c `elem` ",;" then ' ' else c
