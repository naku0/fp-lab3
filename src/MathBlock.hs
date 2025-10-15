module MathBlock (
    InterpolationMethod(..),

) where

data InterpolationMethod = Linear | Newton | Lagrange deriving (Show, Eq, Read)
{-
linearInt

newtonInt

lagrangeInt -}