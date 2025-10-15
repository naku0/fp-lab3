module Parse(
    parseRead
) where

import Config(Config(..))
import MathBlock (InterpolationMethod(..))
import Data.Char (toLower)

parseRead :: [String] -> Config -> Config
parseRead [] config = config
parseRead ("--method":x:xs) config = parseRead xs config { method = parseMethod x }
parseRead ("--n":x:xs) config = parseRead xs config { startPoints = parsePoints x }
parseRead ("--step":x:xs) config = parseRead xs config {   step   = parseStep   x }
parseRead (_:xs) config = parseRead xs config

parseMethod :: String -> InterpolationMethod
parseMethod s = case map toLower s of 
    "linear"   -> Linear
    "lin"      -> Linear
    "newton"   -> Newton
    "new"      -> Newton
    "lagrange" -> Lagrange
    "lag"      -> Lagrange
    _ -> error $ "There's no such method as" <> s <> "\n" <> "Available methods: linear, newton, lagrange" 

parsePoints :: String -> Int
parsePoints s = case reads s of
    [(n, "")] | n>0 -> n
    [(n, "")]       -> error $ "Your number should be positive, not " <> show n
    _               -> error $ "I think you misinputed, got \"" <> s <> "\" and i don't think this is a number" 

parseStep :: String -> Double
parseStep s = case reads s of
    [(sV, "")] | sV > 0 -> sV
    [(sV, "")]          -> error $ "Your number should be positive, not " <> show sV
    _                   -> error $ "I think you misinputed, got \"" <> s <> "\" and i don't think this is a number" 