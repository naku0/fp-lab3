module Main (main) where

import Console(processRead)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= processRead
